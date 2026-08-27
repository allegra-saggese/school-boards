library(data.table)
library(ggplot2)
library(DBI)
library(RSQLite)

source("functions.R")
source("R/paths.R")

# =========================================================
# BKP augmented tests (T1-T3)
#
# Second track of the BKP replication update: not a closer copy of BKP's
# figures, but a set of tests aimed at (a) economic mechanisms behind the 0.5
# cliff and (b) the two live rebuttals in the literature —
#   Binder & Lam (JHR 2022): the cliff can arise mechanically from assortative
#     matching on income, without any gender-identity norm.
#   Murray-Close & Heggeness (Census 2018): survey income near 0.5 is
#     behaviorally contaminated (couples under-report her / over-report him
#     when she out-earns) — BKP's own SIPP-SSA/DER admin-data check (Figure 3)
#     addresses this but that linked admin data isn't available to us.
#
# Reuses the SHARED pair panel and donut-RDD design from
# ipums-rdd-breadwinner-norm.R (same donut_primary/rdd_bw, same political-group
# and frontier merges) rather than rebuilding a BKP-specific sample — T1-T3 are
# meant to extend "our design," not replicate BKP's.
#
# T1 — income-share decomposition: wife's share of (a) labor earnings,
#   (b) total income, (c) capital/non-labor income. If the cliff is about
#   effort/market-work (identity norm) rather than total household resources,
#   it should be sharp in (a), attenuated in (b), and absent in (c) — a direct
#   test against Binder & Lam, who can't distinguish these three shares under
#   an assortative-matching-only story.
#
# T2 — hourly-wage decomposition: horse-race the husband's hourly wage rate
#   (his "market price," harder to game by working fewer hours) against his
#   total income (easier for either spouse to distort/misreport) as predictors
#   of the wife's labor supply. If the identity threat is about the wife's
#   income exceeding a stable measure of the husband's earning power (not just
#   noisy reported income), the hourly-wage version should do more work.
#
# T3 — cultural/income-elastic heterogeneity: stratify T1/T2 by county
#   political lean and frontier-culture status (Bazzi et al.), reusing the
#   existing political-group and frontier merges.
#
# Retires the old INCTOT - INCSS - INCWELFR measure (incoherent: leaves
# capital income and non-SS/welfare transfers in) in favor of the explicit
# three-way T1 decomposition.
# =========================================================

# ── 0) Config (matches ipums-rdd-breadwinner-norm.R) ──────────────────────
donut_primary <- 0.02
rdd_bw        <- 0.20
t3_years      <- 2010:2020   # window with political-group + frontier coverage

panel_dir   <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
ensure_dir(results_dir)

sqlite_path <- data_path("interim", "ipums_data.sqlite")

below_above_ratio <- function(dt, z_col, wt_col, donut_w = donut_primary) {
  dt[, zb := round(get(z_col) * 100) / 100]
  below <- dt[zb >= 0.40 & zb < (0.5 - donut_w), sum(get(wt_col))]
  above <- dt[zb >  (0.5 + donut_w) & zb <= 0.60, sum(get(wt_col))]
  data.table(below_share = below / dt[, sum(get(wt_col))],
             above_share = above / dt[, sum(get(wt_col))],
             ratio = below / above)
}

# ── 1) Load shared pair panel (T1a/T1b, T2 base) ───────────────────────────

cols_base <- c(
  "YEAR", "SAMPLE", "SERIAL", "HHWT", "STATEICP", "COUNTYICP",
  "female_pernum", "male_pernum",
  "female_income_wage_nonneg", "male_income_wage_nonneg",
  "female_income_total_nonneg", "male_income_total_nonneg",
  "female_weekly_hours", "male_weekly_hours",
  "female_annual_hours", "male_annual_hours",
  "female_empstat", "male_empstat"
)
message("Reading shared pair panel ...")
dt <- fread(
  file.path(panel_dir, "ipums_married_oppositesex_spouse_pairs_with_kids.csv"),
  select = cols_base, showProgress = FALSE
)

# ── 2) T1: three-way income-share decomposition ────────────────────────────

message("T1: building three-way income-share decomposition ...")

# (a) labor earnings share (wage/salary only — same concept as BKP)
dt[, z_labor := fifelse(
  female_income_wage_nonneg > 0 & male_income_wage_nonneg > 0,
  female_income_wage_nonneg / (female_income_wage_nonneg + male_income_wage_nonneg),
  NA_real_
)]

# (b) total income share (pollutes the labor-effort signal with transfers/
# capital by construction — BKP's Canada-LAD robustness check mirrors this)
dt[, z_total := fifelse(
  female_income_total_nonneg > 0 & male_income_total_nonneg > 0,
  female_income_total_nonneg / (female_income_total_nonneg + male_income_total_nonneg),
  NA_real_
)]

# (c) capital/non-labor income share — needs INCINVST/INCOTHER, not in the
# shared panel file. Lightweight supplementary pull (no self-join, so much
# cheaper than rebuilding the pair file), merged on person keys already
# present in the panel (female_pernum/male_pernum).
message("  Pulling supplementary capital-income (INCINVST+INCOTHER) by person ...")
con <- dbConnect(SQLite(), sqlite_path)
years_needed <- sort(unique(dt$YEAR))
capital_income <- rbindlist(lapply(years_needed, function(yr) {
  sql <- paste0(
    "SELECT YEAR, SAMPLE, SERIAL, PERNUM, ",
    "  COALESCE(INCINVST,0) + COALESCE(INCOTHER,0) AS capital_income ",
    "FROM ipums_table WHERE YEAR = ", yr
  )
  setDT(dbGetQuery(con, sql))
}))
dbDisconnect(con)

dt <- merge(dt, capital_income,
            by.x = c("YEAR", "SAMPLE", "SERIAL", "female_pernum"),
            by.y = c("YEAR", "SAMPLE", "SERIAL", "PERNUM"), all.x = TRUE)
setnames(dt, "capital_income", "female_capital_income")
dt <- merge(dt, capital_income,
            by.x = c("YEAR", "SAMPLE", "SERIAL", "male_pernum"),
            by.y = c("YEAR", "SAMPLE", "SERIAL", "PERNUM"), all.x = TRUE)
setnames(dt, "capital_income", "male_capital_income")

dt[, female_capital_income := pmax(female_capital_income, 0)]
dt[, male_capital_income   := pmax(male_capital_income, 0)]
dt[, z_capital := fifelse(
  female_capital_income > 0 & male_capital_income > 0,
  female_capital_income / (female_capital_income + male_capital_income),
  NA_real_
)]
message("  Capital-income interior N: ", dt[!is.na(z_capital), .N],
        " (small/noisy — capital income has no wealth-stock denominator in ACS; ",
        "directional evidence only, per plan caveat)")

t1_long <- rbindlist(list(
  dt[!is.na(z_labor),   .(YEAR, HHWT, z = z_labor,   measure = "(a) Labor earnings")],
  dt[!is.na(z_total),   .(YEAR, HHWT, z = z_total,   measure = "(b) Total income")],
  dt[!is.na(z_capital), .(YEAR, HHWT, z = z_capital, measure = "(c) Capital/non-labor income")]
))
t1_long[, z_bin := round(z * 100) / 100]

t1_density <- t1_long[, .(wt = sum(HHWT)), by = .(measure, z_bin)]
t1_density[, share := wt / sum(wt), by = measure]

p_t1 <- ggplot(t1_density[z_bin > 0.05 & z_bin < 0.95],
               aes(x = z_bin, y = share * 100)) +
  geom_col(width = 0.009, fill = "steelblue4", alpha = 0.85) +
  geom_vline(xintercept = 0.5, color = "red", linewidth = 0.8, linetype = "dashed") +
  facet_wrap(~measure, ncol = 3) +
  labs(
    title    = "T1: does the 0.5 cliff survive broadening the income concept?",
    subtitle = paste0(
      "Prediction: sharp cliff in (a) labor earnings, attenuated in (b) total income, ",
      "absent in (c) capital income.\n",
      "If true, the norm bites on market effort, not total household resources — ",
      "a share-specific pattern assortative matching alone (Binder & Lam) can't produce."
    ),
    x = "Wife's income share", y = "Share of observations (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))
save_plot("bkp_augmented_t1_income_share_decomposition.png", { print(p_t1) }, width = 2400, height = 1000)

t1_ratios <- rbindlist(lapply(c("(a) Labor earnings", "(b) Total income", "(c) Capital/non-labor income"),
  function(m) {
    r <- below_above_ratio(t1_long[measure == m], "z", "HHWT")
    r[, measure := m]
    r
  }
))
message("T1 below/above ratios by income concept (donut-excluded [0.40,0.48) vs (0.52,0.60]):")
print(t1_ratios[, .(measure, below_share = round(below_share, 4),
                     above_share = round(above_share, 4), ratio = round(ratio, 3))])
fwrite(t1_ratios, file.path(results_dir, "bkp_augmented_t1_ratios.csv"))

# ── 3) T2: hourly-wage decomposition, horse race vs. total income ──────────

message("T2: hourly-wage decomposition ...")

dt[, female_hourly_wage := fifelse(female_annual_hours > 0,
                                    female_income_wage_nonneg / female_annual_hours, NA_real_)]
dt[, male_hourly_wage   := fifelse(male_annual_hours > 0,
                                    male_income_wage_nonneg / male_annual_hours, NA_real_)]

t2_dt <- dt[
  !is.na(male_hourly_wage) & male_hourly_wage > 0 &
  !is.na(male_income_total_nonneg) & male_income_total_nonneg > 0 &
  !is.na(female_weekly_hours)
]
t2_dt[, ln_husb_hourly := log(male_hourly_wage)]
t2_dt[, ln_husb_total  := log(male_income_total_nonneg)]
t2_dt[, year_f := factor(YEAR)]

# Horse race: both measures in the same model. If the hourly wage is the more
# stable "threat" object, its coefficient should be larger/more robust than
# the noisier total-income measure once both compete for the same variation.
fit_horserace <- lm(
  female_weekly_hours ~ ln_husb_hourly + ln_husb_total + year_f,
  data = t2_dt, weights = HHWT
)
s <- summary(fit_horserace)$coefficients
t2_results <- data.table(
  term     = c("ln(husband hourly wage)", "ln(husband total income)"),
  estimate = round(s[c("ln_husb_hourly", "ln_husb_total"), "Estimate"], 4),
  se       = round(s[c("ln_husb_hourly", "ln_husb_total"), "Std. Error"], 4),
  p_value  = round(s[c("ln_husb_hourly", "ln_husb_total"), "Pr(>|t|)"], 4),
  n_obs    = nobs(fit_horserace)
)
message("T2 horse race — wife weekly hours ~ ln(husband hourly wage) + ln(husband total income) + year FE:")
message("  Caveats: division bias (hourly wage built from reported hours), top-coding, noisy self-employment hours.")
print(t2_results)
fwrite(t2_results, file.path(results_dir, "bkp_augmented_t2_horserace_results.csv"))

p_t2 <- ggplot(t2_dt[female_hourly_wage > 0 & female_hourly_wage < 150 &
                     male_hourly_wage > 0 & male_hourly_wage < 150],
               aes(x = male_hourly_wage, y = female_weekly_hours)) +
  geom_bin2d(bins = 40) +
  scale_fill_viridis_c(name = "N", trans = "log10") +
  geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 0.9, formula = y ~ x) +
  labs(
    title    = "T2: wife's weekly hours vs. husband's hourly wage",
    subtitle = "Hourly wage = labor earnings / (weekly hours × weeks worked); positive-hours sample",
    x = "Husband's hourly wage ($)", y = "Wife's weekly hours"
  ) +
  theme_minimal(base_size = 11)
save_plot("bkp_augmented_t2_hourly_wage_scatter.png", { print(p_t2) }, width = 1800, height = 1200)

# ── 4) T3: stratify T1/T2 by political lean and frontier status ────────────

message("T3: cultural/income-elastic heterogeneity ...")

pairs_file_grp <- file.path(panel_dir, "ipums_married_oppositesex_spouse_pairs_with_groups.csv")
if (!file.exists(pairs_file_grp)) {
  message("  Skipping T3: ", pairs_file_grp, " not found (run ipums-county-household-analysis.R ",
          "Section 7 / ipums-rdd-breadwinner-norm.R first).")
} else {
  cols_grp <- c("YEAR", "HHWT", "fips", "vote_margin",
                "female_income_wage_nonneg", "male_income_wage_nonneg",
                "female_annual_hours", "female_weekly_hours",
                "male_income_total_nonneg")
  avail <- names(fread(pairs_file_grp, nrows = 0))
  grp_dt <- fread(pairs_file_grp, select = intersect(cols_grp, avail), showProgress = FALSE)
  grp_dt <- grp_dt[YEAR %in% t3_years & !is.na(vote_margin)]
  grp_dt[, political := fcase(
    vote_margin >  0.05, "Democratic-majority",
    vote_margin < -0.05, "Republican-majority",
    default = NA_character_
  )]

  frontier_lu_file <- file.path(panel_dir, "bazzi_frontier_indicators.csv")
  has_frontier <- file.exists(frontier_lu_file)
  if (has_frontier) {
    frontier_lu <- fread(frontier_lu_file, select = c("fips", "is_frontier"))
    frontier_lu[, fips := as.character(fips)]
    grp_dt[, fips := as.character(fips)]
    grp_dt <- merge(grp_dt, frontier_lu, by = "fips", all.x = TRUE)
  } else {
    message("  ", frontier_lu_file, " not found — T3 will report political stratification only (no frontier).")
    grp_dt[, is_frontier := NA_integer_]
  }

  grp_dt[, z_labor := fifelse(
    female_income_wage_nonneg > 0 & male_income_wage_nonneg > 0,
    female_income_wage_nonneg / (female_income_wage_nonneg + male_income_wage_nonneg),
    NA_real_
  )]

  grp_dt[!is.na(political) & !is.na(z_labor),
         strat_group := if (has_frontier) paste(political, fifelse(is_frontier == 1, "frontier", "non-frontier"))
                         else political]

  t3_ratios <- rbindlist(lapply(unique(na.omit(grp_dt$strat_group)), function(g) {
    sub <- grp_dt[strat_group == g & !is.na(z_labor)]
    r <- below_above_ratio(sub, "z_labor", "HHWT")
    r[, group := g]
    r[, n_obs := nrow(sub)]
    r
  }))
  message("T3: T1(a) labor-share below/above ratio by political × frontier group:")
  print(t3_ratios[, .(group, ratio = round(ratio, 3), n_obs)])
  fwrite(t3_ratios, file.path(results_dir, "bkp_augmented_t3_ratios_by_group.csv"))

  # T2's hourly-wage horse race is NOT re-stratified here: the with_groups
  # panel doesn't carry male_annual_hours (only the base with_kids panel does),
  # so a correct male_hourly_wage can't be built on this file. T3 reports the
  # T1 density/ratio stratification only — see claude/future-extensions.md for
  # the follow-up (add male_annual_hours to the with_groups panel).
  message("  Note: T3 stratifies T1 (income-share cliff) by political × frontier group only. ",
          "T2's hourly-wage horse race is not re-stratified here (with_groups panel lacks ",
          "male_annual_hours) — see claude/future-extensions.md.")

  p_t3 <- ggplot(t3_ratios, aes(x = reorder(group, ratio), y = ratio)) +
    geom_col(fill = "steelblue4", alpha = 0.85) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    coord_flip() +
    labs(
      title    = "T3: labor-income-share below/above ratio by political × frontier group",
      subtitle = "Higher ratio = more couples bunched just below the 0.5 threshold (stronger avoidance)",
      x = NULL, y = "Below / above ratio"
    ) +
    theme_minimal(base_size = 11)
  save_plot("bkp_augmented_t3_ratio_by_group.png", { print(p_t3) }, width = 1800, height = 1100)
}

message("\nBKP augmented tests complete.")
message("Outputs: data/graphs/bkp_augmented_*.png, data/processed/results/bkp_augmented_*.csv")
