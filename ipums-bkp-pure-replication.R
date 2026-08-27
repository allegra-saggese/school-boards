library(data.table)
library(ggplot2)
library(DBI)
library(RSQLite)
library(sandwich)
library(lmtest)

source("functions.R")
source("R/paths.R")

# =========================================================
# BKP (Bertrand, Kamenica & Pan, QJE 2015) — PURE replication
#
# Corrects ipums-bkp-replication-approximate.R against BKP's actual sample
# construction, verified directly against the NBER working paper (w19023) text.
# Queries data/interim/ipums_data.sqlite directly (does NOT use the shared
# ipums_married_oppositesex_spouse_pairs_with_kids.csv pair-builder in
# ipums-county-household-analysis.R, which has different restrictions used by
# many other scripts — see project plan for the rationale).
#
# Sample construction, matched to BKP Section 3.1 and Section 5:
#   - Figure 1: ACS 2008-2010 (BKP's "3-year aggregate"; here approximated by
#     stacking the three 1-year ACS files — see caveat below), young couples:
#     wife 22-31, husband 24-33, both labor income (INCWAGE) > 0 (interior).
#   - Figure 2: same young-couple restriction, one Census/ACS year per decade
#     (1970, 1980, 1990; 2000 decennial unavailable, ACS 2001 used as a labeled
#     proxy) — BKP's Section 3.1 introduces Figure 2 directly after Figure 1
#     with no separate restriction stated, so we carry the same sample.
#   - Table 2/3: both spouses 18-65, husband employed, BKP-era years
#     (1970, 1980, 1990, 2001-proxy, 2008-2010) plus a 2011-2023 extension.
#   - No county filter (BKP doesn't use one; the shared pipeline's COUNTYICP
#     filter silently drops most PUMAs). Household composition: only requires
#     a mutually-linked opposite-sex spouse pair via SPLOC — extra household
#     members (parents, adult children) are NOT excluded, unlike the shared
#     pipeline's "exactly two adults 25+" filter.
#
# KNOWN DATA GAPS (flagged inline where they bind; see plan /
# claude/bkp-replication-v2-changes.md):
#   1) "Labor income" = INCWAGE only here. BKP's labor income includes
#      self-employment (INCBUS00/INCFARM); those columns aren't in our current
#      IPUMS extract (only their quality flags are). Swap-in point is marked
#      INCOME_MEASURE_SWAP below — becomes a one-line change once a new extract
#      with INCBUS00/INCFARM lands.
#   2) "ACS 2008-2010 3-year aggregate" is approximated by stacking three
#      1-year ACS files (2008, 2009, 2010), not the Census Bureau's pooled
#      3-year microdata product (different weighting).
#   3) 2000 decennial sample isn't in our extract; ACS 2001 is used as the
#      nearest available proxy in Figure 2 and the Table 2/3 BKP-era panel.
#   4) Race buckets use RACE only (White/Black/Other) — no HISPAN variable in
#      our extract, so BKP's white/Black/Hispanic three-way split isn't exactly
#      reproduced.
#
# Scope: distribution (Figure 1/2) + labor supply (Table 2/3) only. Table 1
# (Bartik-IV marriage-formation regression) and Tables 4-6 (NSFH marital
# stability, ATUS chores) need data/instruments we don't have and are out of
# scope here — see claude/future-extensions.md.
# =========================================================

results_dir <- data_path("processed", "results")
panel_dir   <- data_path("processed", "panel")
ensure_dir(results_dir)
ensure_dir(panel_dir)

sqlite_path <- data_path("interim", "ipums_data.sqlite")
if (!file.exists(sqlite_path)) stop("Missing SQLite file: ", sqlite_path)

con <- dbConnect(SQLite(), sqlite_path)
on.exit(dbDisconnect(con), add = TRUE)
dbExecute(con, "PRAGMA busy_timeout = 5000")

# Only one index is needed here: (YEAR, AGE, SEX) makes the per-year, per-sex
# age-range pulls in pull_person_side() index-only scans. The other indices
# ipums-county-household-analysis.R creates aren't used by this script, and
# index creation on a 42GB table is slow and consumes scarce disk — so don't
# create what we don't need. Already-existing index makes this a no-op.
invisible(dbExecute(
  con, "CREATE INDEX IF NOT EXISTS idx_ipums_age_sex ON ipums_table (YEAR, AGE, SEX)"
))

# ── 0) Config ──────────────────────────────────────────────────────────────
young_wife_age  <- c(22, 31)
young_husb_age  <- c(24, 33)
adult_age       <- c(18, 65)

bkp_era_young_years <- c(2008L, 2009L, 2010L)              # Figure 1
bkp_era_decade_years <- c(1970L, 1980L, 1990L, 2001L)       # Figure 2 (2001 = 2000 proxy)
bkp_era_table_years  <- c(1970L, 1980L, 1990L, 2001L, 2008L, 2009L, 2010L) # Table 2/3
extension_young_years <- c(2021L, 2022L, 2023L)             # 10-years-on Figure 1
extension_table_years <- 2011L:2023L                        # 10-years-on Table 2/3

min_cell_n <- 30L   # minimum weighted-N to trust a demographic-cell wage percentile

# lm() + vcovCL on the full multi-million-row, multi-year pooled sample (with
# ~90 dummy/continuous RHS columns) is impractically slow/memory-heavy. Fit the
# Table 2/3 regressions on a capped random subsample instead — the LFP-rate
# sanity check still uses the full (uncapped) sample. Purely a compute-time
# guard, not a scope cut: standard practice for large weighted microdata.
max_reg_n <- 250000L
set.seed(42)

# INCOME_MEASURE_SWAP: change this single line once INCBUS00 (+ INCFARM
# pre-2000) exist in ipums_data.sqlite, to move from wage-only to BKP's true
# "labor income" (wage + self-employment).
labor_income_expr <- "INCWAGE"

# ── 1) Helpers ────────────────────────────────────────────────────────────

weighted_quantile <- function(x, w, probs) {
  ok <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) == 0) return(rep(NA_real_, length(probs)))
  ord <- order(x); x <- x[ord]; w <- w[ord]
  cum_w <- cumsum(w) / sum(w)
  sapply(probs, function(p) {
    idx <- which(cum_w >= p)[1]
    if (is.na(idx)) NA_real_ else x[idx]
  })
}

bucket_educ5 <- function(educ) {
  # IPUMS general EDUC codes -> 5 BKP-style buckets.
  fcase(
    is.na(educ),        NA_character_,
    educ <= 5,           "< HS",
    educ == 6,            "HS",
    educ %in% 7:9,        "Some college",
    educ == 10,           "College",
    educ == 11,           "> College",
    default = NA_character_
  )
}

bucket_race3 <- function(race) {
  # No HISPAN variable in our extract -> White/Black/Other only (caveat #4 above).
  fcase(
    is.na(race), NA_character_,
    race == 1,    "White",
    race == 2,    "Black",
    default =    "Other"
  )
}

age_bin5 <- function(age) {
  breaks <- seq(15, 70, by = 5)
  cut(age, breaks = breaks, right = FALSE,
      labels = paste0(head(breaks, -1), "-", tail(breaks, -1) - 1))
}

# BKP footnote 12: triangular-kernel recode of the exact-0.5 mass across n bins.
# Bin k in {1,...,n} receives weight (n/2 - |n/2-(k-1)|) / ((n/2)(n/2-1)) of the
# mass at exactly 0.5. Sum-normalized to 1 as a numerical safeguard (footnote's
# indexing is asymmetric at the two edges; normalizing preserves total mass
# regardless of that edge convention).
triangular_kernel_weights <- function(n_bins) {
  k <- seq_len(n_bins)
  half <- n_bins / 2
  w <- (half - abs(half - (k - 1))) / (half * (half - 1))
  w <- pmax(w, 0)
  w / sum(w)
}

recode_half_mass_triangular <- function(dt, z_col, wt_col, n_bins = 20L) {
  # dt: data.table with a running variable in [0,1] and a weight column.
  # Returns a binned density data.table (bin center, weighted share) with the
  # exact-0.5 mass redistributed across bins via the triangular kernel instead
  # of appearing as a spike.
  breaks <- seq(0, 1, length.out = n_bins + 1)
  bin_mid <- (head(breaks, -1) + tail(breaks, -1)) / 2

  is_half <- abs(dt[[z_col]] - 0.5) < 1e-9
  half_mass <- sum(dt[[wt_col]][is_half], na.rm = TRUE)
  total_mass <- sum(dt[[wt_col]], na.rm = TRUE)

  non_half <- dt[!is_half]
  bin_idx <- findInterval(non_half[[z_col]], breaks, all.inside = TRUE)
  base_wt <- tapply(non_half[[wt_col]], factor(bin_idx, levels = seq_len(n_bins)), sum)
  base_wt[is.na(base_wt)] <- 0

  half_wt <- triangular_kernel_weights(n_bins) * half_mass
  total_wt <- as.numeric(base_wt) + half_wt

  data.table(bin_mid = bin_mid, wt = total_wt, share = total_wt / total_mass)
}

cluster_se <- function(model, cluster_var) {
  coeftest(model, vcov = vcovCL(model, cluster = cluster_var))
}

# ── 2) Spouse-pair builder (BKP-style: no county filter, no household-size
#      restriction beyond a mutual SPLOC link) ──────────────────────────────

# PERFORMANCE NOTE: do NOT do this spouse match as a single SQL self-join.
# ipums_table has no index on the join keys (YEAR, SAMPLE, SERIAL, PERNUM/
# SPLOC), but it does have idx_ipums_age_sex (YEAR, AGE, SEX). Given both, the
# SQLite planner picks the age index for BOTH sides of a self-join and then
# nested-loops the match — billions of row comparisons, effectively a hang
# (measured: >2.75 hours for one year, vs ~3.5s for the approach below).
# Instead: two independent indexed range scans (fast, uses idx_ipums_age_sex),
# then match in-memory with data.table. Building a join-key index on a 42GB
# table is the other fix, but costs disk we don't have.
pull_person_side <- function(yr, sex, age_range, extra_where = "") {
  sql <- paste0(
    "SELECT YEAR, SAMPLE, SERIAL, PERNUM, STATEICP, AGE, SEX, SPLOC, RACE, EDUC, ",
    "EMPSTAT, UHRSWORK, WKSWORK1, INCWAGE, INCTOT, HHWT, PERWT, NCHILD ",
    "FROM ipums_table WHERE YEAR = ", yr, " AND SEX = ", sex,
    " AND AGE BETWEEN ", age_range[1], " AND ", age_range[2], extra_where
  )
  setDT(dbGetQuery(con, sql))
}

build_bkp_pairs <- function(years, wife_age = c(18, 65), husb_age = c(18, 65),
                             require_husb_working = FALSE) {
  keys <- c("YEAR", "SAMPLE", "SERIAL")
  out <- vector("list", length(years))
  for (i in seq_along(years)) {
    yr <- years[i]
    message("  Building BKP spouse pairs for year ", yr, " (wife ", wife_age[1], "-",
            wife_age[2], ", husband ", husb_age[1], "-", husb_age[2], ")")

    wives <- pull_person_side(yr, 2, wife_age)
    husbs <- pull_person_side(yr, 1, husb_age,
                              if (require_husb_working) " AND EMPSTAT = 1" else "")

    # Prefix non-key columns so the two sides don't collide on merge.
    setnames(wives, setdiff(names(wives), keys),
             paste0("female_", tolower(setdiff(names(wives), keys))))
    setnames(husbs, setdiff(names(husbs), keys),
             paste0("male_", tolower(setdiff(names(husbs), keys))))

    # Match within household, then keep only mutually-linked spouse pairs
    # (BKP's marriage definition; same SPLOC rule as the shared pipeline).
    cand <- merge(wives, husbs, by = keys, allow.cartesian = TRUE)
    cand <- cand[female_sploc == male_pernum & male_sploc == female_pernum]

    # One HHWT per household; both sides carry it, keep the wife's.
    setnames(cand, "female_hhwt", "HHWT")
    cand[, male_hhwt := NULL]
    setnames(cand, "female_stateicp", "STATEICP")
    cand[, male_stateicp := NULL]
    cand[, nchild := female_nchild]
    cand[, c("female_nchild", "male_nchild") := NULL]

    out[[i]] <- cand
  }
  rbindlist(out, use.names = TRUE, fill = TRUE)
}

# ── 3) Figure 1: young couples, ACS 2008-2010, triangular-kernel cliff ─────

message("Building Figure 1 sample (young couples, ACS 2008-2010) ...")
pairs_young_fig1 <- build_bkp_pairs(bkp_era_young_years, young_wife_age, young_husb_age)
message("  N pairs (pre income filter): ", nrow(pairs_young_fig1))

# INCOME_MEASURE_SWAP point: female/male_incwage stand in for labor_income_expr
# until INCBUS00 (+ INCFARM pre-2000) are available in ipums_data.sqlite.
pairs_young_fig1[, female_labor_income := female_incwage]
pairs_young_fig1[, male_labor_income   := male_incwage]

fig1_interior <- pairs_young_fig1[female_labor_income > 0 & male_labor_income > 0]
fig1_interior[, z := female_labor_income / (female_labor_income + male_labor_income)]
message("  Interior (both labor income > 0) N: ", nrow(fig1_interior),
        "  weighted N: ", round(sum(fig1_interior$HHWT)))

fig1_density <- recode_half_mass_triangular(fig1_interior, "z", "HHWT", n_bins = 20L)

p_fig1 <- ggplot(fig1_density, aes(x = bin_mid, y = share)) +
  geom_point(color = "steelblue4", size = 1.8) +
  geom_smooth(data = fig1_density[bin_mid < 0.5], method = "loess", se = FALSE,
              color = "#4575b4", linewidth = 0.9, formula = y ~ x) +
  geom_smooth(data = fig1_density[bin_mid >= 0.5], method = "loess", se = FALSE,
              color = "#d73027", linewidth = 0.9, formula = y ~ x) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey30") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(
    title    = "Figure 1 replication: distribution of wife's labor income share",
    subtitle = paste0(
      "Young couples (wife ", young_wife_age[1], "-", young_wife_age[2],
      ", husband ", young_husb_age[1], "-", young_husb_age[2],
      "), ACS 2008-2010 stacked (proxy for BKP's 3-yr aggregate), ",
      "interior sample, triangular-kernel 0.5-mass recode, 20 bins.\n",
      "Income concept: ", labor_income_expr, " only (BKP = wage + self-employment; ",
      "see INCOME_MEASURE_SWAP note in script header)."
    ),
    x = "Wife's share of couple labor income", y = "Fraction"
  ) +
  theme_minimal(base_size = 11)

save_plot("bkp_pure_figure1_young_couples_density.png", { print(p_fig1) }, width = 1800, height = 1200)

# Sanity check (Section 3.2 of BKP): wife earns more in ~26% of 18-65 couples,
# ACS 2008-2010 (this is the 18-65 sample, not the young-couples sample above).
message("Building 18-65 sanity-check sample (ACS 2008-2010) ...")
pairs_1865_acs0810 <- build_bkp_pairs(bkp_era_young_years, adult_age, adult_age)
pairs_1865_acs0810[, female_labor_income := female_incwage]
pairs_1865_acs0810[, male_labor_income   := male_incwage]
sanity_interior <- pairs_1865_acs0810[female_labor_income > 0 & male_labor_income > 0]
sanity_wife_more <- weighted.mean(
  sanity_interior$female_labor_income > sanity_interior$male_labor_income,
  sanity_interior$HHWT
)
message("  SANITY CHECK vs. BKP's reported 26%: wife earns more in ",
        round(sanity_wife_more * 100, 1), "% of 18-65 ACS 2008-2010 couples ",
        "(both labor income > 0).")

# ── 4) Figure 2: distribution by decade, young-couple restriction ─────────

message("Building Figure 2 sample (young couples, by decade) ...")
pairs_young_fig2 <- build_bkp_pairs(bkp_era_decade_years, young_wife_age, young_husb_age)
pairs_young_fig2[, female_labor_income := female_incwage]
pairs_young_fig2[, male_labor_income   := male_incwage]
fig2_interior <- pairs_young_fig2[female_labor_income > 0 & male_labor_income > 0]
fig2_interior[, z := female_labor_income / (female_labor_income + male_labor_income)]
fig2_interior[, decade_label := fcase(
  YEAR == 1970, "1970",
  YEAR == 1980, "1980",
  YEAR == 1990, "1990",
  YEAR == 2001, "2000 (ACS 2001 proxy)"
)]

fig2_density <- fig2_interior[, {
  recode_half_mass_triangular(.SD, "z", "HHWT", n_bins = 20L)
}, by = decade_label]

p_fig2 <- ggplot(fig2_density, aes(x = bin_mid, y = share)) +
  geom_point(color = "steelblue4", size = 1.3) +
  geom_line(color = "steelblue4", alpha = 0.5) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey30") +
  facet_wrap(~decade_label, ncol = 4) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  labs(
    title    = "Figure 2 replication: distribution of wife's labor income share by decade",
    subtitle = "Young couples (same age bands as Figure 1); triangular-kernel 0.5-mass recode",
    x = "Wife's share of couple labor income", y = "Fraction"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

save_plot("bkp_pure_figure2_by_decade_density.png", { print(p_fig2) }, width = 2200, height = 900)

# ── 5) Potential-income imputation (for Table 2/3) ─────────────────────────

build_potential_income_lookup <- function(years, min_n = min_cell_n) {
  out <- vector("list", length(years))
  for (i in seq_along(years)) {
    yr <- years[i]
    sql <- paste0(
      "SELECT AGE, EDUC, RACE, STATEICP, INCWAGE, PERWT FROM ipums_table ",
      "WHERE YEAR = ", yr, " AND SEX = 2 AND AGE BETWEEN 18 AND 65 ",
      "  AND EMPSTAT = 1 AND INCWAGE > 0"
    )
    dt <- setDT(dbGetQuery(con, sql))
    dt[, YEAR := yr]
    out[[i]] <- dt
  }
  women <- rbindlist(out, use.names = TRUE)
  women[, age5  := age_bin5(AGE)]
  women[, educ5 := bucket_educ5(EDUC)]
  women[, race3 := bucket_race3(RACE)]
  women <- women[!is.na(age5) & !is.na(educ5) & !is.na(race3)]

  probs <- seq(0.05, 0.95, by = 0.05)  # 19 vigintile-ish points, per BKP Section 5

  lookup <- women[, {
    n_wt <- sum(PERWT)
    if (n_wt < min_n) {
      as.list(setNames(rep(NA_real_, length(probs)), paste0("potential_p", seq_along(probs))))
    } else {
      as.list(setNames(weighted_quantile(INCWAGE, PERWT, probs), paste0("potential_p", seq_along(probs))))
    }
  }, by = .(YEAR, STATEICP, age5, educ5, race3)]

  list(lookup = lookup, probs = probs)
}

# ── 6) Table 2/3: wife's LFP and income gap ~ PrWifeEarnsMore ──────────────

run_table2_table3 <- function(years, era_label) {
  message("Building Table 2/3 sample (", era_label, ") ...")
  pairs <- build_bkp_pairs(years, adult_age, adult_age, require_husb_working = TRUE)
  pairs[, female_labor_income := female_incwage]
  pairs[, male_labor_income   := male_incwage]
  pairs[, age5  := age_bin5(female_age)]
  pairs[, educ5 := bucket_educ5(female_educ)]
  pairs[, race3 := bucket_race3(female_race)]
  pairs[, husb_age5  := age_bin5(male_age)]
  pairs[, husb_educ5 := bucket_educ5(male_educ)]
  pairs[, husb_race3 := bucket_race3(male_race)]
  pairs <- pairs[!is.na(age5) & !is.na(educ5) & !is.na(race3) &
                 !is.na(husb_age5) & !is.na(husb_educ5) & !is.na(husb_race3)]

  pot <- build_potential_income_lookup(years)
  pairs <- merge(
    pairs, pot$lookup,
    by.x = c("YEAR", "STATEICP", "age5", "educ5", "race3"),
    by.y = c("YEAR", "STATEICP", "age5", "educ5", "race3"),
    all.x = TRUE
  )

  potential_cols <- paste0("potential_p", seq_along(pot$probs))
  n_before <- nrow(pairs)
  pairs <- pairs[stats::complete.cases(pairs[, ..potential_cols])]
  message("  Dropped ", n_before - nrow(pairs), " of ", n_before,
          " pairs with no reliable demographic-cell wage distribution ",
          "(cell weighted N < ", min_cell_n, ").")

  pot_mat <- as.matrix(pairs[, ..potential_cols])
  pairs[, PrWifeEarnsMore := rowMeans(pot_mat > male_labor_income)]

  pairs[, wife_lfp   := as.integer(female_empstat %in% c(1, 2))]
  pairs[, wife_working := as.integer(female_empstat == 1)]
  pairs[, ln_husb_income := log1p(pmax(male_labor_income, 0))]
  pairs[, wife_potential_mean := rowMeans(pot_mat)]
  pairs[, income_gap := ifelse(
    wife_working == 1 & wife_potential_mean > 0,
    (female_labor_income - wife_potential_mean) / wife_potential_mean,
    NA_real_
  )]
  pairs[, cell_id := interaction(YEAR, STATEICP, age5, educ5, race3, drop = TRUE)]

  potential_formula <- paste(potential_cols, collapse = " + ")

  reg_sample <- if (nrow(pairs) > max_reg_n) {
    message("  Subsampling ", nrow(pairs), " -> ", max_reg_n,
            " rows for regression fitting (compute-time guard; sanity checks above use the full sample).")
    pairs[sample(.N, max_reg_n)]
  } else pairs

  # Table 2, Column (1)-style baseline: wifeLFP ~ PrWifeEarnsMore + potential
  # vigintile controls + ln(husb income) + year/state FE + race + age-group +
  # education-group dummies (both spouses).
  fmla_lfp <- as.formula(paste0(
    "wife_lfp ~ PrWifeEarnsMore + ", potential_formula,
    " + ln_husb_income + factor(YEAR) + factor(STATEICP) + race3 + husb_race3",
    " + age5 + husb_age5 + educ5 + husb_educ5"
  ))
  fit_lfp <- lm(fmla_lfp, data = reg_sample, weights = HHWT)
  se_lfp  <- cluster_se(fit_lfp, reg_sample$cell_id)

  # Table 3-style: income gap ~ same RHS, working wives only.
  reg_sample_working <- reg_sample[wife_working == 1]
  fmla_gap <- as.formula(paste0(
    "income_gap ~ PrWifeEarnsMore + ", potential_formula,
    " + ln_husb_income + factor(YEAR) + factor(STATEICP) + race3 + husb_race3",
    " + age5 + husb_age5 + educ5 + husb_educ5"
  ))
  fit_gap <- lm(fmla_gap, data = reg_sample_working, weights = HHWT)
  se_gap  <- cluster_se(fit_gap, reg_sample_working$cell_id)

  extract_beta1 <- function(se_obj, fit, label) {
    row <- se_obj["PrWifeEarnsMore", ]
    data.table(era = era_label, outcome = label,
               beta1 = round(row["Estimate"], 4),
               se    = round(row["Std. Error"], 4),
               p     = round(row["Pr(>|t|)"], 4),
               n_obs = nobs(fit),
               n_full_sample = nrow(pairs))
  }

  results <- rbindlist(list(
    extract_beta1(se_lfp, fit_lfp, "Wife LFP (Table 2 analog)"),
    extract_beta1(se_gap, fit_gap, "Income gap (Table 3 analog)")
  ))

  message("  Table 2/3 results (", era_label, "):")
  print(results)

  # Sanity check vs. BKP's reported LFP levels (44% 1970 -> 70% 1990 -> 74% 2010).
  lfp_by_year <- pairs[, .(lfp_rate = weighted.mean(wife_lfp, HHWT)), by = YEAR][order(YEAR)]
  message("  Wife LFP rate by year (SANITY CHECK vs. BKP: ~44% 1970, ~70% 1990, ~74% 2010):")
  print(lfp_by_year)

  list(results = results, pairs = pairs, lfp_by_year = lfp_by_year)
}

table23_bkp_era <- run_table2_table3(bkp_era_table_years, "BKP era (1970-2010)")

# ── 7) 10-years-on extension ───────────────────────────────────────────────

message("Building 10-years-on Figure 1 analog (young couples, ACS 2021-2023) ...")
pairs_young_ext <- build_bkp_pairs(extension_young_years, young_wife_age, young_husb_age)
pairs_young_ext[, female_labor_income := female_incwage]
pairs_young_ext[, male_labor_income   := male_incwage]
ext_interior <- pairs_young_ext[female_labor_income > 0 & male_labor_income > 0]
ext_interior[, z := female_labor_income / (female_labor_income + male_labor_income)]
ext_density <- recode_half_mass_triangular(ext_interior, "z", "HHWT", n_bins = 20L)
ext_density[, era := "2021-2023 (10 years on)"]
fig1_density[, era := "2008-2010 (BKP era)"]

fig1_compare <- rbindlist(list(fig1_density, ext_density))
p_fig1_compare <- ggplot(fig1_compare, aes(x = bin_mid, y = share, color = era)) +
  geom_point(size = 1.6) +
  geom_line(alpha = 0.6) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey30") +
  scale_color_manual(values = c("2008-2010 (BKP era)" = "steelblue4",
                                 "2021-2023 (10 years on)" = "#d73027"), name = NULL) +
  labs(
    title    = "Has the 0.5 cliff changed 10+ years after BKP's sample?",
    subtitle = "Young couples, triangular-kernel recode, same construction as Figure 1",
    x = "Wife's share of couple labor income", y = "Fraction"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

save_plot("bkp_pure_figure1_era_comparison.png", { print(p_fig1_compare) }, width = 1800, height = 1200)

table23_extension <- run_table2_table3(extension_table_years, "Post-BKP (2011-2023)")

era_comparison <- rbindlist(list(table23_bkp_era$results, table23_extension$results))
message("\nPrWifeEarnsMore coefficient (beta1), BKP era vs. 10-years-on:")
print(era_comparison)

fwrite(era_comparison, file.path(results_dir, "bkp_pure_table23_era_comparison.csv"))
fwrite(fig1_compare, file.path(results_dir, "bkp_pure_figure1_era_comparison_density.csv"))

message("\nBKP pure replication complete.")
message("Outputs: data/graphs/*bkp_pure_*.png, data/processed/results/bkp_pure_*.csv")
