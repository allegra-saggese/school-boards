# -----------------------------------------------------------------------------
# NAMING — read this before using "T1/T2/T3" anywhere near this project.
#
# At PROJECT level the three parts are:
#     T1 = the BKP replication            (ipums-bkp-pure-replication.R + this file)
#     T2 = the empirical culture x wealth quadrant (ipums-t2-empirical-quadrant.R)
#     T3 = the theoretical household utility model
#
# The decompositions in THIS file are supporting evidence inside T1. They were
# once also called T1/T2/T3, which collided with the project-level names and
# caused real confusion; they are A/B/C here now. Do not reintroduce T-labels
# in this file.
# -----------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(DBI)
library(RSQLite)

source("functions.R")
source("R/paths.R")

# =========================================================
# BKP replication: supporting decompositions (A-C)
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
# A — income-share decomposition: wife's share of (a) labor earnings,
#   (b) total income, (c) capital/non-labor income. If the cliff is about
#   effort/market-work (identity norm) rather than total household resources,
#   it should be sharp in (a), attenuated in (b), and absent in (c) — a direct
#   test against Binder & Lam, who can't distinguish these three shares under
#   an assortative-matching-only story.
#
# B — hourly-wage decomposition: horse-race the husband's hourly wage rate
#   (his "market price," harder to game by working fewer hours) against his
#   total income (easier for either spouse to distort/misreport) as predictors
#   of the wife's labor supply. If the identity threat is about the wife's
#   income exceeding a stable measure of the husband's earning power (not just
#   noisy reported income), the hourly-wage version should do more work.
#
# C — cultural/income-elastic heterogeneity: stratify A/B by county
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
c_years      <- 2010:2020   # window with political-group + frontier coverage

panel_dir   <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
ensure_dir(results_dir)

sqlite_path <- data_path("interim", "ipums_bkp.sqlite")
# Both sides now share ONE data vintage: the pair panel was rebuilt from this
# same database by ipums-county-household-analysis.R, so the capital-income
# columns pulled here and the panel's own income columns come from IPUMS
# extract 4 throughout. (Earlier this script paired a new-extract pull with an
# old-extract panel; that is no longer the case.)

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
  "female_income_labor", "male_income_labor",
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

message("A: building three-way income-share decomposition ...")

# (a) LABOR earnings share = wage + self-employment, BKP's actual income
# concept. The rebuilt panel carries this directly (female/male_income_labor);
# previously this line used wage-only because the old extract had no
# self-employment income. Including it matters: it brings in couples whose
# earnings are mostly business or farm income, who previously looked like
# non-earners and were dropped from the interior sample entirely.
#
# TWO SAMPLES, deliberately:
#
#  z_labor          — INTERIOR only (both spouses earning). This is BKP's
#                     construction and the one the cliff/ratio statistics need,
#                     because the share is degenerate for a one-earner couple:
#                     it sits at exactly 0 or 1 and says nothing about behaviour
#                     near 0.5.
#
#  z_labor_incl_zero — INCLUDES one-earner couples, coding a male-breadwinner
#                     couple as 0 and a female-breadwinner couple as 1. This is
#                     the sample OUR question needs. A norm that pushes wives
#                     out of paid work entirely does not bend the interior
#                     distribution — it moves mass to zero, which the interior
#                     sample discards by construction. Restricting to
#                     both-earner couples therefore conditions on the very
#                     margin the income-elastic-norm story predicts, and would
#                     understate the norm exactly where it binds hardest.
dt[, z_labor := fifelse(
  female_income_labor > 0 & male_income_labor > 0,
  female_income_labor / (female_income_labor + male_income_labor),
  NA_real_
)]
dt[, z_labor_incl_zero := fifelse(
  (female_income_labor + male_income_labor) > 0,
  female_income_labor / (female_income_labor + male_income_labor),
  NA_real_
)]
dt[, earner_type := fcase(
  female_income_labor > 0 & male_income_labor > 0, "dual earner",
  female_income_labor <= 0 & male_income_labor > 0, "male sole earner",
  female_income_labor > 0 & male_income_labor <= 0, "female sole earner",
  default = "neither earning"
)]
message("  Earner composition (weighted):")
print(dt[!is.na(earner_type), .(share = round(sum(HHWT) / dt[, sum(HHWT)], 4)),
         by = earner_type][order(-share)])
message("  -> the interior (dual-earner) sample used for the cliff statistics ",
        "excludes the sole-earner couples above; see z_labor_incl_zero for the ",
        "extension sample that keeps them.")

# (b) total income share (pollutes the labor-effort signal with transfers/
# capital by construction — BKP's Canada-LAD robustness check mirrors this)
dt[, z_total := fifelse(
  female_income_total_nonneg > 0 & male_income_total_nonneg > 0,
  female_income_total_nonneg / (female_income_total_nonneg + male_income_total_nonneg),
  NA_real_
)]

# (c) capital/non-labor income share — needs INCINVST/INCOTHER, which aren't in
# the shared panel file, so they come from a supplementary DB pull.
#
# MEMORY NOTE: process one year at a time and reduce immediately. Pulling all
# years at once means ~83M person-rows in memory merged twice against an 11.6M
# -row panel (~5GB peak) — enough to swap-thrash this machine. Per year, we
# keep only (YEAR, HHWT, z_capital), which is small.
message("  Pulling supplementary capital-income (INCINVST+INCOTHER) year by year ...")
con <- dbConnect(SQLite(), sqlite_path)
year_rowid_ranges <- dbGetQuery(
  con, "SELECT YEAR, MIN(rowid) AS lo, MAX(rowid) AS hi FROM ipums_table GROUP BY YEAR")
years_needed <- sort(unique(dt$YEAR))

z_capital_list <- lapply(years_needed, function(yr) {
  # Sequential rowid-range scan, not an index seek — see the performance note
  # in ipums-bkp-pure-replication.R (measured ~290x on this database).
  rr <- year_rowid_ranges[year_rowid_ranges$YEAR == as.integer(yr), ]
  cap <- setDT(dbGetQuery(con, paste0(
    "SELECT SERIAL, PERNUM, ",
    "  MAX(COALESCE(INCINVST,0),0) + MAX(COALESCE(INCOTHER,0),0) AS cap_inc ",
    "FROM ipums_table NOT INDEXED WHERE rowid BETWEEN ", rr$lo, " AND ", rr$hi,
    " AND YEAR = ", yr
  )))
  yr_dt <- dt[YEAR == yr, .(YEAR, SERIAL, HHWT, female_pernum, male_pernum)]

  yr_dt <- merge(yr_dt, cap, by.x = c("SERIAL", "female_pernum"),
                 by.y = c("SERIAL", "PERNUM"), all.x = TRUE)
  setnames(yr_dt, "cap_inc", "f_cap")
  yr_dt <- merge(yr_dt, cap, by.x = c("SERIAL", "male_pernum"),
                 by.y = c("SERIAL", "PERNUM"), all.x = TRUE)
  setnames(yr_dt, "cap_inc", "m_cap")
  rm(cap)

  out <- yr_dt[!is.na(f_cap) & !is.na(m_cap) & f_cap > 0 & m_cap > 0,
               .(YEAR, HHWT, z_capital = f_cap / (f_cap + m_cap))]
  rm(yr_dt); gc(verbose = FALSE)
  message("    ", yr, ": ", nrow(out), " couples with both capital income > 0")
  out
})
dbDisconnect(con)

z_capital_dt <- rbindlist(z_capital_list)
rm(z_capital_list); gc(verbose = FALSE)
message("  Capital-income interior N: ", nrow(z_capital_dt),
        " (small/noisy — capital income has no wealth-stock denominator in ACS; ",
        "directional evidence only, per plan caveat)")

a_long <- rbindlist(list(
  dt[!is.na(z_labor), .(YEAR, HHWT, z = z_labor, measure = "(a) Labor earnings")],
  dt[!is.na(z_total), .(YEAR, HHWT, z = z_total, measure = "(b) Total income")],
  z_capital_dt[, .(YEAR, HHWT, z = z_capital, measure = "(c) Capital/non-labor income")]
))
a_long[, z_bin := round(z * 100) / 100]

a_density <- a_long[, .(wt = sum(HHWT)), by = .(measure, z_bin)]
a_density[, share := wt / sum(wt), by = measure]

p_t1 <- ggplot(a_density[z_bin > 0.05 & z_bin < 0.95],
               aes(x = z_bin, y = share * 100)) +
  geom_col(width = 0.009, fill = "steelblue4", alpha = 0.85) +
  geom_vline(xintercept = 0.5, color = "red", linewidth = 0.8, linetype = "dashed") +
  facet_wrap(~measure, ncol = 3) +
  labs(
    title    = "A: does the 0.5 cliff survive broadening the income concept?",
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
save_plot("bkp_augmented_a_income_share_decomposition.png", { print(p_t1) }, width = 2400, height = 1000)

a_ratios <- rbindlist(lapply(c("(a) Labor earnings", "(b) Total income", "(c) Capital/non-labor income"),
  function(m) {
    r <- below_above_ratio(a_long[measure == m], "z", "HHWT")
    r[, measure := m]
    r
  }
))
message("A below/above ratios by income concept (donut-excluded [0.40,0.48) vs (0.52,0.60]):")
print(a_ratios[, .(measure, below_share = round(below_share, 4),
                     above_share = round(above_share, 4), ratio = round(ratio, 3))])
fwrite(a_ratios, file.path(results_dir, "bkp_augmented_a_ratios.csv"))

# ── 3) T2: hourly-wage decomposition, horse race vs. total income ──────────

message("B: hourly-wage decomposition ...")

dt[, female_hourly_wage := fifelse(female_annual_hours > 0,
                                    female_income_wage_nonneg / female_annual_hours, NA_real_)]
dt[, male_hourly_wage   := fifelse(male_annual_hours > 0,
                                    male_income_wage_nonneg / male_annual_hours, NA_real_)]

b_dt <- dt[
  !is.na(male_hourly_wage) & male_hourly_wage > 0 &
  !is.na(male_income_total_nonneg) & male_income_total_nonneg > 0 &
  !is.na(female_weekly_hours)
]
b_dt[, ln_husb_hourly := log(male_hourly_wage)]
b_dt[, ln_husb_total  := log(male_income_total_nonneg)]
b_dt[, year_f := factor(YEAR)]

# Horse race: both measures in the same model. If the hourly wage is the more
# stable "threat" object, its coefficient should be larger/more robust than
# the noisier total-income measure once both compete for the same variation.
fit_horserace <- lm(
  female_weekly_hours ~ ln_husb_hourly + ln_husb_total + year_f,
  data = b_dt, weights = HHWT
)
s <- summary(fit_horserace)$coefficients
b_results <- data.table(
  term     = c("ln(husband hourly wage)", "ln(husband total income)"),
  estimate = round(s[c("ln_husb_hourly", "ln_husb_total"), "Estimate"], 4),
  se       = round(s[c("ln_husb_hourly", "ln_husb_total"), "Std. Error"], 4),
  p_value  = round(s[c("ln_husb_hourly", "ln_husb_total"), "Pr(>|t|)"], 4),
  n_obs    = nobs(fit_horserace)
)
message("B horse race — wife weekly hours ~ ln(husband hourly wage) + ln(husband total income) + year FE:")
message("  Caveats: division bias (hourly wage built from reported hours), top-coding, noisy self-employment hours.")
print(b_results)
fwrite(b_results, file.path(results_dir, "bkp_augmented_b_horserace_results.csv"))

# PLOTTING NOTE: geom_smooth(method="loess") does NOT subsample, and LOESS on
# millions of points is pathologically slow — it ran for over an hour here on
# 7.5M rows at full CPU. The binning is cheap; the smoother was the problem.
# Fix: draw the density from a large random subsample (visually identical for a
# 40x40 hex grid) and overlay a binned conditional mean computed on the FULL
# data, which is both exact and O(n). Nothing is estimated from the subsample.
b_plot_dt <- b_dt[female_hourly_wage > 0 & female_hourly_wage < 150 &
                    male_hourly_wage   > 0 & male_hourly_wage   < 150]
set.seed(42)
b_plot_sample <- if (nrow(b_plot_dt) > 500000L)
  b_plot_dt[sample(.N, 500000L)] else b_plot_dt

b_trend <- b_plot_dt[, .(female_weekly_hours = weighted.mean(female_weekly_hours, HHWT, na.rm = TRUE)),
                       by = .(male_hourly_wage = round(male_hourly_wage))][order(male_hourly_wage)]

p_t2 <- ggplot(b_plot_sample,
               aes(x = male_hourly_wage, y = female_weekly_hours)) +
  geom_bin2d(bins = 40) +
  scale_fill_viridis_c(name = "N", trans = "log10") +
  geom_line(data = b_trend, color = "red", linewidth = 0.9) +
  labs(
    title    = "B: wife's weekly hours vs. husband's hourly wage",
    subtitle = paste0("Hourly wage = labor earnings / (weekly hours x weeks worked); positive-hours sample.\n",
                      "Density from a 500k random subsample; red line = weighted mean over ALL ",
                      format(nrow(b_plot_dt), big.mark = ","), " couples."),
    x = "Husband's hourly wage ($)", y = "Wife's weekly hours"
  ) +
  theme_minimal(base_size = 11)
save_plot("bkp_augmented_b_hourly_wage_scatter.png", { print(p_t2) }, width = 1800, height = 1200)

# ── 4) T3: stratify A/B by political lean and frontier status ────────────

message("C: cultural/income-elastic heterogeneity ...")

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
  grp_dt <- grp_dt[YEAR %in% c_years & !is.na(vote_margin)]
  grp_dt[, political := fcase(
    vote_margin >  0.05, "Democratic-majority",
    vote_margin < -0.05, "Republican-majority",
    default = NA_character_
  )]

  frontier_lu_file <- file.path(panel_dir, "bazzi_frontier_indicators.csv")
  has_frontier <- file.exists(frontier_lu_file)
  if (has_frontier) {
    frontier_lu <- fread(frontier_lu_file, select = c("fips", "is_frontier"))
    # Same leading-zero hazard as the political merge: fread types these codes
    # as integer, so as.character() would give "6037" against the panel's
    # "06037" and silently drop every state numbered 01-09.
    frontier_lu[, fips := pad_fips(fips)]
    grp_dt[, fips := pad_fips(fips)]
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
         # fifelse(is_frontier == 1, ...) returns NA when is_frontier is NA
         # (counties absent from the Bazzi lookup), producing labels like
         # "Republican-majority NA". Those counties have unknown frontier
         # status, not a known one — label them explicitly so they are visibly
         # a coverage gap rather than silently mixed into a real category.
         strat_group := if (has_frontier) paste(political, fcase(
             is.na(is_frontier), "frontier status unknown",
             is_frontier == 1,   "frontier",
             default =           "non-frontier"))
                         else political]

  c_ratios <- rbindlist(lapply(unique(na.omit(grp_dt$strat_group)), function(g) {
    sub <- grp_dt[strat_group == g & !is.na(z_labor)]
    r <- below_above_ratio(sub, "z_labor", "HHWT")
    r[, group := g]
    r[, n_obs := nrow(sub)]
    r
  }))
  message("C: T1(a) labor-share below/above ratio by political × frontier group:")
  print(c_ratios[, .(group, ratio = round(ratio, 3), n_obs)])
  fwrite(c_ratios, file.path(results_dir, "bkp_augmented_c_ratios_by_group.csv"))

  # B's hourly-wage horse race is NOT re-stratified here: the with_groups
  # panel doesn't carry male_annual_hours (only the base with_kids panel does),
  # so a correct male_hourly_wage can't be built on this file. T3 reports the
  # T1 density/ratio stratification only — see claude/future-extensions.md for
  # the follow-up (add male_annual_hours to the with_groups panel).
  message("  Note: C stratifies A (income-share cliff) by political × frontier group only. ",
          "B's hourly-wage horse race is not re-stratified here (with_groups panel lacks ",
          "male_annual_hours) — see claude/future-extensions.md.")

  p_t3 <- ggplot(c_ratios, aes(x = reorder(group, ratio), y = ratio)) +
    geom_col(fill = "steelblue4", alpha = 0.85) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    coord_flip() +
    labs(
      title    = "C: labor-income-share below/above ratio by political × frontier group",
      subtitle = "Higher ratio = more couples bunched just below the 0.5 threshold (stronger avoidance)",
      x = NULL, y = "Below / above ratio"
    ) +
    theme_minimal(base_size = 11)
  save_plot("bkp_augmented_c_ratio_by_group.png", { print(p_t3) }, width = 1800, height = 1100)
}

message("\nBKP augmented tests complete.")
message("Outputs: data/graphs/bkp_augmented_*.png, data/processed/results/bkp_augmented_*.csv")
