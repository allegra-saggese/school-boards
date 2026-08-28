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
# Sample construction, matched to BKP Section 3.1 and Section 5:
#   - Figure 1: ACS 2008-2010, young couples (wife 22-31, husband 24-33), both
#     spouses with labor income > 0 (interior).
#   - Figure 2: same young-couple restriction, one decennial per decade
#     (1970, 1980, 1990, 2000) — BKP's Section 3.1 introduces Figure 2 directly
#     after Figure 1 with no separate restriction stated, so we carry the same
#     sample forward.
#   - Table 2/3: both spouses 18-65, husband employed; BKP-era years
#     (1970, 1980, 1990, 2000, 2008-2010) plus a 2011-2024 extension.
#   - Income = LABOR income (wage + self-employment), matching BKP. See the
#     labor_income() helper for the era-splicing and the N/A-code handling.
#   - Race = BKP's three marriage-market groups (white / Black / Hispanic,
#     other races dropped), via RACE + HISPAN.
#   - No county filter (BKP doesn't use one; the shared pipeline's COUNTYICP
#     filter silently drops most PUMAs). Household composition: only requires
#     a mutually-linked opposite-sex spouse pair via SPLOC — extra household
#     members (parents, adult children) are NOT excluded, unlike the shared
#     pipeline's "exactly two adults 25+" filter.
#
# REMAINING DEVIATIONS FROM BKP (deliberate; see
# claude/bkp-replication-v2-changes.md):
#   1) BKP's Figure 1 used the ACS 2008-2010 3-year aggregate. We stack the
#      three 1-year files instead. The 3-year product contains the same
#      respondents, so including both would double-count; BKP's use of it
#      reflected the data vintage available to them in 2013, not a property
#      that needs reproducing. Weighting differs slightly as a result.
#   2) 1970 carries two non-overlapping questionnaire forms. Which one(s) we
#      use is decided FROM THE DATA: detect_1970_samples() keeps the form(s)
#      that actually report self-employment income (INCBUS/INCFARM), since the
#      1970 census split questions across its two long forms and that income is
#      part of BKP's labor-income concept. If both qualify they are pooled to
#      ~2% and weights are halved.
#   3) Extended through 2024, past BKP's 2011 endpoint, to test whether the
#      0.5 cliff has changed in the decade since publication.
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

sqlite_path <- data_path("interim", "ipums_bkp.sqlite")
if (!file.exists(sqlite_path)) {
  stop("Missing SQLite file: ", sqlite_path, "\n",
       "Build it first with ipums-bkp-build-database.R (downloads IPUMS extract 4).")
}

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

bkp_era_young_years   <- c(2008L, 2009L, 2010L)                    # Figure 1
bkp_era_decade_years  <- c(1970L, 1980L, 1990L, 2000L)             # Figure 2 (real 2000 decennial)
bkp_era_table_years   <- c(1970L, 1980L, 1990L, 2000L, 2008L, 2009L, 2010L)  # Table 2/3
extension_young_years <- c(2022L, 2023L, 2024L)                    # 10-years-on Figure 1
extension_table_years <- 2011L:2024L                               # 10-years-on Table 2/3

min_cell_n <- 30L   # minimum weighted-N to trust a demographic-cell wage percentile

# ── Sequential range scans instead of index seeks ──────────────────────────
# PERFORMANCE, measured — this is the difference between minutes and hours.
#
# Finding rows via idx_ipums_age_sex is instant, but FETCHING 20+ columns for
# millions of matches means millions of random page reads into a 14.9GB file.
# For the 2000 decennial (4.4M matching women) that measured 5m09s at 1% CPU —
# pure I/O wait — and the pipeline does this ~40 times.
#
# The data was inserted in file order, so each YEAR occupies a CONTIGUOUS
# rowid range (verified: all 28 years contiguous). Scanning that range reads
# sequentially instead of seeking randomly: the same 2000 fetch takes 1.06s at
# 99% CPU. Roughly a 290x speedup.
#
# NOT INDEXED stops the planner reverting to the age index (it prefers the
# index seek, which is the slow path here). The YEAR = ? predicate is kept as
# a correctness guard so results stay right even if contiguity ever breaks.
year_rowid_ranges <- setDT(dbGetQuery(
  con, "SELECT YEAR, MIN(rowid) AS lo, MAX(rowid) AS hi, COUNT(*) AS n
        FROM ipums_table GROUP BY YEAR"
))
if (nrow(year_rowid_ranges[(hi - lo + 1) != n]) > 0) {
  warning("Some YEARs are not contiguous in rowid order; range scans will be ",
          "less selective but remain correct (the YEAR predicate still applies).")
}
setkey(year_rowid_ranges, YEAR)

year_rowid_clause <- function(yr) {
  r <- year_rowid_ranges[J(as.integer(yr))]
  if (nrow(r) == 0L || is.na(r$lo)) stop("Year not present in database: ", yr)
  paste0(" rowid BETWEEN ", r$lo, " AND ", r$hi, " AND YEAR = ", yr)
}

# ── 1970 sample selection: driven by the data, not hard-coded ──────────────
# The extract contains both 1970 questionnaire forms (Form 1 = SAMPLE 197001,
# Form 2 = 197002). They are drawn from different households, so they do not
# overlap. But the 1970 census split questions across the two long forms, and
# self-employment income (INCBUS/INCFARM) is part of BKP's labor-income
# concept — so we use whichever form(s) actually carry it, determined by
# querying coverage rather than assuming.
#
# If both forms qualify, they are pooled and weights halved: each form's
# weights are calibrated so that its own 1% sample sums to the US population,
# so pooling without adjustment would imply 2x the true population. (Shares and
# weighted means are unaffected — the factor cancels — but weighted counts are.)
selfemp_coverage_floor <- 0.90   # share of adult records with a usable value

detect_1970_samples <- function() {
  cov <- setDT(dbGetQuery(con, paste0(
    "SELECT SAMPLE, COUNT(*) AS n, ",
    "  SUM(CASE WHEN INCBUS  IS NOT NULL AND INCBUS  < 999999 THEN 1 ELSE 0 END) AS n_bus, ",
    "  SUM(CASE WHEN INCFARM IS NOT NULL AND INCFARM < 999999 THEN 1 ELSE 0 END) AS n_farm ",
    "FROM ipums_table WHERE YEAR = 1970 AND AGE >= 16 GROUP BY SAMPLE ORDER BY SAMPLE"
  )))
  if (nrow(cov) == 0) stop("No 1970 records found in ", sqlite_path)
  cov[, `:=`(bus_cov = n_bus / n, farm_cov = n_farm / n)]
  message("1970 self-employment income coverage by sample:")
  print(cov[, .(SAMPLE, n, bus_cov = round(bus_cov, 3), farm_cov = round(farm_cov, 3))])

  keep <- cov[bus_cov >= selfemp_coverage_floor & farm_cov >= selfemp_coverage_floor, SAMPLE]
  if (length(keep) == 0) {
    stop("No 1970 sample carries INCBUS/INCFARM at >= ",
         selfemp_coverage_floor * 100, "% coverage. Inspect the table above: ",
         "1970 may need to be dropped, or the labor-income concept relaxed to ",
         "wage-only for that year (which would make 1970 non-comparable to the ",
         "other decades).")
  }
  message("  -> using 1970 SAMPLE(s): ", paste(keep, collapse = ", "),
          if (length(keep) > 1) "  [pooled; weights halved]" else "  [single form]")
  keep
}
samples_1970 <- detect_1970_samples()

# lm() + vcovCL on the full multi-million-row, multi-year pooled sample (with
# ~90 dummy/continuous RHS columns) is impractically slow/memory-heavy. Fit the
# Table 2/3 regressions on a capped random subsample instead — the LFP-rate
# sanity check still uses the full (uncapped) sample. Purely a compute-time
# guard, not a scope cut: standard practice for large weighted microdata.
max_reg_n <- 250000L
set.seed(42)

# ── Labor income (BKP's income concept) ───────────────────────────────────
# BKP define individual income as LABOR income: wages/salary plus
# self-employment. IPUMS splits self-employment across variables and eras:
#   INCBUS + INCFARM  -> 1950-2000 (business and farm reported separately)
#   INCBUS00          -> 2000 onward (business and farm combined)
# 2000 carries both; we prefer INCBUS00 where present so the ACS era and the
# 2000 census use the same definition.
#
# Two data hazards handled here:
#  1) IPUMS N/A sentinels. Income variables use 9999999/999999/999998-style
#     codes for "not in universe" / missing, NOT real dollars. Verified on the
#     old extract: INCWAGE = 999999 occurs for every under-16 record and none
#     at 16+, so the adult age restrictions already excluded them — but the
#     guard belongs in code, not in an age filter that might later change.
#  2) INCBUS/INCFARM can be legitimately NEGATIVE (business/farm losses).
#     So we must not clamp the components at zero individually; we sum first,
#     then clamp the total at zero (someone whose only income is a $5k loss
#     has zero labor income, not negative).
na_sentinel <- function(x) {
  # IPUMS income N/A / missing codes are large repunit-style values.
  fifelse(x %in% c(999999, 999998, 9999999, 9999998, -9999999), NA_real_, as.numeric(x))
}

labor_income <- function(incwage, incbus, incfarm, incbus00) {
  w  <- na_sentinel(incwage)
  b0 <- na_sentinel(incbus00)
  b  <- na_sentinel(incbus)
  f  <- na_sentinel(incfarm)
  self_emp <- fifelse(!is.na(b0), b0, rowSums(cbind(b, f), na.rm = TRUE))
  pmax(rowSums(cbind(w, self_emp), na.rm = TRUE), 0)
}

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

bucket_race3 <- function(race, hispan) {
  # BKP's three marriage-market race groups: (non-Hispanic) white,
  # (non-Hispanic) Black, and Hispanic. BKP: "We drop individuals of other
  # races." Hispanic origin takes precedence over RACE, matching their
  # construction. HISPAN: 0 = not Hispanic, 1-4 = Hispanic, 9 = missing.
  fcase(
    is.na(race) | is.na(hispan),        NA_character_,
    hispan %in% 1:4,                    "Hispanic",
    hispan == 0 & race == 1,            "White",
    hispan == 0 & race == 2,            "Black",
    default =                           NA_character_   # other races dropped, per BKP
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
  # SPLOC > 0 pre-filters to spouse-linked people before the column fetch —
  # lossless here, since only mutually-linked spouses can form a pair.
  sql <- paste0(
    "SELECT YEAR, SAMPLE, SERIAL, PERNUM, STATEICP, AGE, SEX, SPLOC, RACE, HISPAN, EDUC, ",
    "EMPSTAT, UHRSWORK, WKSWORK1, INCWAGE, INCBUS, INCFARM, INCBUS00, INCTOT, ",
    "HHWT, PERWT, NCHILD ",
    "FROM ipums_table NOT INDEXED WHERE", year_rowid_clause(yr),
    " AND SEX = ", sex, " AND SPLOC > 0",
    " AND AGE BETWEEN ", age_range[1], " AND ", age_range[2], extra_where
  )
  dt <- setDT(dbGetQuery(con, sql))

  # 1970: keep only the form(s) that actually carry INCBUS/INCFARM (determined
  # by detect_1970_samples() above), and halve weights if both are pooled.
  if (yr == 1970L) {
    dt <- dt[SAMPLE %in% samples_1970]
    if (length(samples_1970) > 1) dt[, `:=`(HHWT = HHWT / 2, PERWT = PERWT / 2)]
  }
  dt
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

pairs_young_fig1[, female_labor_income := labor_income(female_incwage, female_incbus, female_incfarm, female_incbus00)]
pairs_young_fig1[, male_labor_income   := labor_income(male_incwage, male_incbus, male_incfarm, male_incbus00)]

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
      "Income concept: labor income = wage + self-employment, matching BKP ",
      "(INCWAGE + INCBUS/INCFARM pre-2000, + INCBUS00 from 2000)."
    ),
    x = "Wife's share of couple labor income", y = "Fraction"
  ) +
  theme_minimal(base_size = 11)

save_plot("bkp_pure_figure1_young_couples_density.png", { print(p_fig1) }, width = 1800, height = 1200)

# Sanity check (Section 3.2 of BKP): wife earns more in ~26% of 18-65 couples,
# ACS 2008-2010 (this is the 18-65 sample, not the young-couples sample above).
message("Building 18-65 sanity-check sample (ACS 2008-2010) ...")
pairs_1865_acs0810 <- build_bkp_pairs(bkp_era_young_years, adult_age, adult_age)
pairs_1865_acs0810[, female_labor_income := labor_income(female_incwage, female_incbus, female_incfarm, female_incbus00)]
pairs_1865_acs0810[, male_labor_income   := labor_income(male_incwage, male_incbus, male_incfarm, male_incbus00)]
# IMPORTANT — match BKP's population exactly. BKP (Section 3.2) say the wife
# earns more in 26% of "the couples where both individuals are between 18 and
# 65 years old". That is an AGE restriction only; it does NOT require both to
# have positive income. Comparing 26% against the interior (both-earning)
# sample is the wrong comparison: the interior filter drops male-only-earner
# couples — ~21% of couples, every one a case where the husband out-earns —
# which mechanically inflates the wife-earns-more share by ~3pp.
# Measured here: 25.6% on BKP's population vs 28.4% interior-only.
sanity_all <- pairs_1865_acs0810   # BKP's stated population: age restriction only
sanity_wife_more_all <- weighted.mean(
  sanity_all$female_labor_income > sanity_all$male_labor_income,
  sanity_all$HHWT
)
sanity_interior <- pairs_1865_acs0810[female_labor_income > 0 & male_labor_income > 0]
sanity_wife_more_int <- weighted.mean(
  sanity_interior$female_labor_income > sanity_interior$male_labor_income,
  sanity_interior$HHWT
)
message("  SANITY CHECK vs. BKP's reported 26% (all couples 18-65, BKP's ",
        "stated restriction): ", round(sanity_wife_more_all * 100, 1), "%")
message("  For reference, interior only (both labor income > 0): ",
        round(sanity_wife_more_int * 100, 1),
        "% — NOT the figure BKP's 26% refers to.")

# ── 4) Figure 2: distribution by decade, young-couple restriction ─────────

message("Building Figure 2 sample (young couples, by decade) ...")
pairs_young_fig2 <- build_bkp_pairs(bkp_era_decade_years, young_wife_age, young_husb_age)
pairs_young_fig2[, female_labor_income := labor_income(female_incwage, female_incbus, female_incfarm, female_incbus00)]
pairs_young_fig2[, male_labor_income   := labor_income(male_incwage, male_incbus, male_incfarm, male_incbus00)]
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
      "SELECT AGE, EDUC, RACE, HISPAN, STATEICP, SAMPLE, ",
      "INCWAGE, INCBUS, INCFARM, INCBUS00, PERWT ",
      "FROM ipums_table NOT INDEXED WHERE", year_rowid_clause(yr),
      "  AND SEX = 2 AND AGE BETWEEN 18 AND 65 AND EMPSTAT = 1",
      # Match pull_person_side(): same 1970 form(s), so the potential-income
      # distribution is estimated on the same sample as the couples it is
      # merged onto.
      if (yr == 1970L) paste0(" AND SAMPLE IN (", paste(samples_1970, collapse = ","), ")") else ""
    )
    dt <- setDT(dbGetQuery(con, sql))
    dt[, YEAR := yr]
    # Weight halving when both 1970 forms are pooled (see detect_1970_samples).
    if (yr == 1970L && length(samples_1970) > 1) dt[, PERWT := PERWT / 2]
    out[[i]] <- dt
  }
  women <- rbindlist(out, use.names = TRUE)
  # Potential earnings are drawn from the distribution of LABOR income among
  # working women (BKP's concept), not wage income alone — so the imputation
  # and the running variable use the same definition.
  women[, lab_inc := labor_income(INCWAGE, INCBUS, INCFARM, INCBUS00)]
  women <- women[lab_inc > 0]
  women[, age5  := age_bin5(AGE)]
  women[, educ5 := bucket_educ5(EDUC)]
  women[, race3 := bucket_race3(RACE, HISPAN)]
  women <- women[!is.na(age5) & !is.na(educ5) & !is.na(race3)]

  probs <- seq(0.05, 0.95, by = 0.05)  # 19 vigintile-ish points, per BKP Section 5

  lookup <- women[, {
    n_wt <- sum(PERWT)
    if (n_wt < min_n) {
      as.list(setNames(rep(NA_real_, length(probs)), paste0("potential_p", seq_along(probs))))
    } else {
      as.list(setNames(weighted_quantile(lab_inc, PERWT, probs), paste0("potential_p", seq_along(probs))))
    }
  }, by = .(YEAR, STATEICP, age5, educ5, race3)]

  list(lookup = lookup, probs = probs)
}

# ── 6) Table 2/3: wife's LFP and income gap ~ PrWifeEarnsMore ──────────────

run_table2_table3 <- function(years, era_label) {
  message("Building Table 2/3 sample (", era_label, ") ...")
  pairs <- build_bkp_pairs(years, adult_age, adult_age, require_husb_working = TRUE)
  pairs[, female_labor_income := labor_income(female_incwage, female_incbus, female_incfarm, female_incbus00)]
  pairs[, male_labor_income   := labor_income(male_incwage, male_incbus, male_incfarm, male_incbus00)]
  pairs[, age5  := age_bin5(female_age)]
  pairs[, educ5 := bucket_educ5(female_educ)]
  pairs[, race3 := bucket_race3(female_race, female_hispan)]
  pairs[, husb_age5  := age_bin5(male_age)]
  pairs[, husb_educ5 := bucket_educ5(male_educ)]
  pairs[, husb_race3 := bucket_race3(male_race, male_hispan)]
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

  # WHY BOTH SPECIFICATIONS ARE REPORTED — this is not cosmetic.
  #
  # PrWifeEarnsMore is a deterministic function of the wife's potential-income
  # vector and the husband's actual income. Her potential vector is constant
  # within a demographic cell, so WITHIN a cell PrWifeEarnsMore varies only
  # through the husband's income, as a decreasing step function of it. beta1 is
  # therefore identified purely off how that step function departs from
  # whatever functional form the husband-income control takes.
  #
  # That makes the linear-ln(husbIncome) baseline (BKP Column 1) fragile: if
  # wives' participation falls with husband income faster than log-linear, the
  # unmodelled curvature loads onto beta1 with a POSITIVE sign. Measured here:
  # +0.02 to +0.11 in every year, versus BKP's -0.254.
  #
  # Adding BKP's own cubic in ln(husbIncome) (their Column 2) restores a
  # negative estimate in every year (-0.04 to -0.15, vs BKP's -0.182). BKP
  # include that cubic for exactly this reason. We report both so the
  # sensitivity is visible rather than hidden behind one chosen spec.
  base_controls <- paste0(
    " + factor(YEAR) + factor(STATEICP) + race3 + husb_race3",
    " + age5 + husb_age5 + educ5 + husb_educ5"
  )

  # Column (1): linear in ln(husband income)
  fmla_lfp <- as.formula(paste0(
    "wife_lfp ~ PrWifeEarnsMore + ", potential_formula,
    " + ln_husb_income", base_controls
  ))
  fit_lfp <- lm(fmla_lfp, data = reg_sample, weights = HHWT)
  se_lfp  <- cluster_se(fit_lfp, reg_sample$cell_id)

  # Column (2): cubic polynomial in ln(husband income) — preferred
  fmla_lfp_cubic <- as.formula(paste0(
    "wife_lfp ~ PrWifeEarnsMore + ", potential_formula,
    " + ln_husb_income + I(ln_husb_income^2) + I(ln_husb_income^3)", base_controls
  ))
  fit_lfp_cubic <- lm(fmla_lfp_cubic, data = reg_sample, weights = HHWT)
  se_lfp_cubic  <- cluster_se(fit_lfp_cubic, reg_sample$cell_id)

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
    extract_beta1(se_lfp,       fit_lfp,       "Wife LFP  [col 1: linear ln(husb inc)]"),
    extract_beta1(se_lfp_cubic, fit_lfp_cubic, "Wife LFP  [col 2: CUBIC ln(husb inc)] <- preferred"),
    extract_beta1(se_gap,       fit_gap,       "Income gap (Table 3 analog)")
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

message("Building 10-years-on Figure 1 analog (young couples, ACS 2022-2024) ...")
pairs_young_ext <- build_bkp_pairs(extension_young_years, young_wife_age, young_husb_age)
pairs_young_ext[, female_labor_income := labor_income(female_incwage, female_incbus, female_incfarm, female_incbus00)]
pairs_young_ext[, male_labor_income   := labor_income(male_incwage, male_incbus, male_incfarm, male_incbus00)]
ext_interior <- pairs_young_ext[female_labor_income > 0 & male_labor_income > 0]
ext_interior[, z := female_labor_income / (female_labor_income + male_labor_income)]
ext_density <- recode_half_mass_triangular(ext_interior, "z", "HHWT", n_bins = 20L)
ext_density[, era := "2022-2024 (10+ years on)"]
fig1_density[, era := "2008-2010 (BKP era)"]

fig1_compare <- rbindlist(list(fig1_density, ext_density))
p_fig1_compare <- ggplot(fig1_compare, aes(x = bin_mid, y = share, color = era)) +
  geom_point(size = 1.6) +
  geom_line(alpha = 0.6) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey30") +
  scale_color_manual(values = c("2008-2010 (BKP era)" = "steelblue4",
                                 "2022-2024 (10+ years on)" = "#d73027"), name = NULL) +
  labs(
    title    = "Has the 0.5 cliff changed 10+ years after BKP's sample?",
    subtitle = "Young couples, triangular-kernel recode, same construction as Figure 1",
    x = "Wife's share of couple labor income", y = "Fraction"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

save_plot("bkp_pure_figure1_era_comparison.png", { print(p_fig1_compare) }, width = 1800, height = 1200)

table23_extension <- run_table2_table3(extension_table_years, "Post-BKP (2011-2024)")

era_comparison <- rbindlist(list(table23_bkp_era$results, table23_extension$results))
message("\nPrWifeEarnsMore coefficient (beta1), BKP era vs. 10-years-on:")
print(era_comparison)

fwrite(era_comparison, file.path(results_dir, "bkp_pure_table23_era_comparison.csv"))
fwrite(fig1_compare, file.path(results_dir, "bkp_pure_figure1_era_comparison_density.csv"))

message("\nBKP pure replication complete.")
message("Outputs: data/graphs/*bkp_pure_*.png, data/processed/results/bkp_pure_*.csv")
