library(data.table)
library(ggplot2)

source(here::here("_setup.R"))

# =========================================================
# BKP (Bertrand, Kamenica & Pan, QJE 2015) Replication — APPROXIMATE / LEGACY
#
# NOTE (2026-08-27): This script predates a corrected read of BKP's actual sample
# construction (verified against the NBER w19023 working paper text). It is NOT
# built on BKP's sample: it uses the shared pair-builder from
# ipums-county-household-analysis.R (ages 25-64, exactly-two-adults household
# filter, county filter) rather than BKP's young-couple (22-31/24-33) or 18-65
# restrictions, no county filter, and looser household composition. It also uses
# a raw histogram + donut instead of BKP's triangular-kernel recode of the exact-
# 0.5 mass, and its "Table 3 equivalent" is a naive D(share>0.5) regression, not
# BKP's actual Table 2/3 specification (an LPM on PrWifeEarnsMore, a demographic-
# cell potential-income measure).

# Key differences from our donut RDD (ipums-rdd-breadwinner-norm.R):
#
#   BKP                             | Our design
#   --------------------------------|-------------------------------
#   incwage only (wages/salaries)   | inctot - SS - welfare
#   at-least-one earner > 0         | both spouses earned income > 0
#   years 1970-2011 (we use 1980+)  | 1980-2023
#   no donut                        | ±2pp donut around 0.50
#
# This script produces:
#   1) BKP-style density histogram (Figure 1 equivalent)
#   2) Overlay: BKP running variable vs. our running variable
#   3) Labor supply by income share bin (Table 3 / Figure 2 equivalent)
#   4) Regression table: outcome ~ D(wife > husband) + year FEs
#   5) Density discontinuity test (McCrary/rddensity) at 0.50
# =========================================================

panel_dir   <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
ensure_dir(results_dir)

bkp_era_end <- 2011L   # last year in BKP's sample; we replicate over 1980-2011 first

# ── 0) Load pair file ─────────────────────────────────────────────────────────
cols <- c("YEAR", "HHWT", "STATEICP", "COUNTYICP",
          "female_income_wage_nonneg", "male_income_wage_nonneg",
          "female_income_no_transfers", "male_income_no_transfers",
          "female_weekly_hours", "male_weekly_hours",
          "female_annual_hours", "male_annual_hours",
          "female_empstat", "male_empstat")

message("Reading pair file ...")
dt <- fread(
  file.path(panel_dir, "ipums_married_oppositesex_spouse_pairs_with_kids.csv"),
  select = cols, showProgress = FALSE
)

# ── 1) Running variables ───────────────────────────────────────────────────────

# BKP running variable: wife wage share of couple wage income
# Sample: couple_wage > 0 (at least one spouse has positive wage income)
# z_wage = 0 when wife has zero wages (male-only earner)
# z_wage = 1 when husband has zero wages (female-only earner)
dt[, f_wage := female_income_wage_nonneg]
dt[, m_wage := male_income_wage_nonneg]
dt[, couple_wage := f_wage + m_wage]
dt[, z_wage := fifelse(couple_wage > 0, f_wage / couple_wage, NA_real_)]

# Our running variable (ipums-rdd-breadwinner-norm.R): wife earned-income share
# Sample: both spouses have earned income > 0 (excludes z=0 and z=1 mass points)
dt[, z_earned := fifelse(
  female_income_no_transfers > 0 & male_income_no_transfers > 0,
  female_income_no_transfers / (female_income_no_transfers + male_income_no_transfers),
  NA_real_
)]

# 1pp bins
dt[, z_wage_bin   := round(z_wage   * 100) / 100]
dt[, z_earned_bin := round(z_earned * 100) / 100]

n_bkp   <- dt[!is.na(z_wage)   & YEAR <= bkp_era_end, .N]
n_ours  <- dt[!is.na(z_earned) & YEAR <= bkp_era_end, .N]
n_at_0  <- dt[!is.na(z_wage)   & YEAR <= bkp_era_end & z_wage == 0, .N]
n_at_1  <- dt[!is.na(z_wage)   & YEAR <= bkp_era_end & z_wage == 1, .N]

message("BKP sample (1980-", bkp_era_end, ", couple_wage > 0): ", n_bkp,
        "  [z=0: ", n_at_0, "  z=1: ", n_at_1, "]")
message("Our sample (1980-", bkp_era_end, ", both earned > 0): ", n_ours)
message("Both-earner share in BKP sample: ",
        round(dt[!is.na(z_wage) & YEAR <= bkp_era_end & z_wage > 0 & z_wage < 1, .N] / n_bkp * 100, 1), "%")

# ── 2) BKP-style density histogram (Figure 1 equivalent) ─────────────────────
# Full distribution including mass at 0 and 1; interior = both have positive wages.
# BKP Figure 1 shows a clear drop immediately above 0.50 in the interior.

bkp_era <- dt[!is.na(z_wage) & YEAR <= bkp_era_end]
d_bkp_full <- bkp_era[, .(wt = sum(HHWT)), by = z_wage_bin][order(z_wage_bin)]
d_bkp_full[, share := wt / sum(wt)]

# Interior only: exclude the mass points at 0 and 1 to mirror BKP's visual focus
d_bkp_interior <- d_bkp_full[z_wage_bin > 0 & z_wage_bin < 1]

p_bkp_density <- ggplot(d_bkp_interior,
                        aes(x = z_wage_bin, y = share * 100)) +
  geom_col(width = 0.009, fill = "steelblue4", alpha = 0.85) +
  geom_vline(xintercept = 0.5, color = "red", linewidth = 0.9, linetype = "dashed") +
  annotate("text", x = 0.52, y = max(d_bkp_interior$share * 100) * 0.92,
           label = "Wife earns more\nthan husband",
           hjust = 0, size = 3.0, color = "red") +
  annotate("text", x = 0.48, y = max(d_bkp_interior$share * 100) * 0.92,
           label = "Husband earns\nmore than wife",
           hjust = 1, size = 3.0, color = "navy") +
  labs(
    title    = "Distribution of wife's wage income share (BKP replication, Figure 1 equivalent)",
    subtitle = paste0(
      "Running variable: wife incwage / couple incwage; both spouses with positive wages; ",
      "IPUMS 1980-", bkp_era_end, "\n",
      "BKP uses CPS 1970-2011; same underlying data"
    ),
    x = "Wife's share of couple wage income",
    y = "Share of weighted observations (%)"
  ) +
  theme_minimal(base_size = 11)

save_plot("bkp_replication_density_wage_income_share.png",
          { print(p_bkp_density) }, width = 1800, height = 1100)

# Also produce the full distribution (with mass at 0/1 visible)
p_bkp_full <- ggplot(d_bkp_full, aes(x = z_wage_bin, y = share * 100)) +
  geom_col(width = 0.009, fill = "steelblue4", alpha = 0.85) +
  geom_vline(xintercept = 0.5, color = "red", linewidth = 0.9, linetype = "dashed") +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0\n(wife: $0)", "0.25", "0.50", "0.75", "1\n(husband: $0)")
  ) +
  labs(
    title    = "Full distribution of wife's wage income share (including zero-earner mass points)",
    subtitle = paste0("At-least-one-earner sample; IPUMS 1980-", bkp_era_end),
    x = "Wife's share of couple wage income",
    y = "Share of weighted observations (%)"
  ) +
  theme_minimal(base_size = 11)

save_plot("bkp_replication_density_wage_income_share_full.png",
          { print(p_bkp_full) }, width = 1800, height = 1100)

# ── 3) Overlay: BKP running variable vs. our running variable ─────────────────
# Interior of both distributions on the same axis, 1980-2011, 2pp bins.
d_cmp_bkp <- bkp_era[z_wage > 0 & z_wage < 1,
                      .(wt = sum(HHWT)),
                      by = .(bin = round(z_wage * 50) / 50)][order(bin)]
d_cmp_bkp[, share := wt / sum(wt)]
d_cmp_bkp[, series := "BKP (wage income, at-least-one-earner)"]

d_cmp_ours <- dt[!is.na(z_earned) & YEAR <= bkp_era_end,
                 .(wt = sum(HHWT)),
                 by = .(bin = round(z_earned * 50) / 50)][order(bin)]
d_cmp_ours[, share := wt / sum(wt)]
d_cmp_ours[, series := "Our design (no-transfer income, both-earner)"]

d_cmp <- rbindlist(list(d_cmp_bkp, d_cmp_ours))

p_overlay <- ggplot(d_cmp[bin > 0.05 & bin < 0.95],
                    aes(x = bin, y = share * 100, fill = series, alpha = series)) +
  geom_col(position = "dodge", width = 0.018) +
  geom_vline(xintercept = 0.5, color = "black", linewidth = 0.8, linetype = "dashed") +
  scale_fill_manual(values = c(
    "BKP (wage income, at-least-one-earner)"       = "steelblue4",
    "Our design (no-transfer income, both-earner)"  = "#d73027"
  )) +
  scale_alpha_manual(values = c(
    "BKP (wage income, at-least-one-earner)"       = 0.85,
    "Our design (no-transfer income, both-earner)"  = 0.75
  )) +
  labs(
    title    = "Overlay: BKP running variable vs. our running variable (1980-2011, 2pp bins)",
    subtitle = "BKP = wage income share, at-least-one-earner | Ours = no-transfer income share, both-earner",
    x        = "Wife's share of couple income",
    y        = "Share of observations (%)",
    fill     = NULL, alpha = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

save_plot("bkp_replication_overlay_bkp_vs_ours.png",
          { print(p_overlay) }, width = 1900, height = 1200)

# ── 4) Below/above ratio and kink quantification ──────────────────────────────
# For the interior distribution (both spouses have positive wages),
# compute the weighted share in [0.40, 0.50) vs (0.50, 0.60] as a summary
# of the bunching below the threshold.

for (era_label in c("1980-2011", "1980-2023")) {
  yr_max <- if (era_label == "1980-2011") 2011L else 2023L
  sub <- dt[!is.na(z_wage) & z_wage > 0 & z_wage < 1 & YEAR <= yr_max]
  below <- sub[z_wage_bin >= 0.40 & z_wage_bin < 0.50, sum(HHWT)]
  above <- sub[z_wage_bin >  0.50 & z_wage_bin <= 0.60, sum(HHWT)]
  total <- sub[, sum(HHWT)]
  message(era_label, " | BKP sample | [0.40, 0.50): ",
          round(below / total * 100, 2), "%  (0.50, 0.60]: ",
          round(above / total * 100, 2), "%  ratio: ",
          round(below / above, 3))
}

# ── 5) Labor supply by income share bin (BKP Table 3 / Figure 2 equivalent) ───
# BKP shows wife's annual hours and LFPR in fine income-share bins around 0.5.
# We use 2pp bins; outcomes: wife's weekly hours, wife employed (EMPSTAT=1).
# Both eras; BKP sample (couple_wage > 0 interior) for the replication figure.

make_labor_supply_plot <- function(data, z_col, bin_width_inv = 50L,
                                    era_label, income_label) {
  bw <- 1 / bin_width_inv
  data[, bin_var := round(get(z_col) * bin_width_inv) / bin_width_inv]

  agg <- data[
    !is.na(bin_var) & bin_var > 0.1 & bin_var < 0.9,
    .(
      hours_mean = weighted.mean(female_weekly_hours, HHWT, na.rm = TRUE),
      lfpr_mean  = weighted.mean(as.integer(female_empstat %in% c(1, 2)), HHWT, na.rm = TRUE),
      emp_mean   = weighted.mean(as.integer(female_empstat == 1), HHWT, na.rm = TRUE),
      n          = .N
    ),
    by = bin_var
  ][order(bin_var)]

  agg[, above_half := bin_var > 0.5]

  p_hours <- ggplot(agg[n >= 30], aes(x = bin_var, y = hours_mean)) +
    geom_point(aes(size = n), color = "steelblue4", alpha = 0.75) +
    geom_smooth(data = agg[bin_var <= 0.5 & n >= 30],
                method = "lm", formula = y ~ x,
                color = "#4575b4", se = TRUE, linewidth = 1.0) +
    geom_smooth(data = agg[bin_var > 0.5 & n >= 30],
                method = "lm", formula = y ~ x,
                color = "#d73027", se = TRUE, linewidth = 1.0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", linewidth = 0.8) +
    scale_size_continuous(range = c(1, 4), guide = "none") +
    labs(
      title    = paste0("Wife's weekly hours by income share bin (", era_label, ")"),
      subtitle = paste0("Income measure: ", income_label,
                        "; 2pp bins; blue = fit below 0.5, red = fit above"),
      x        = "Wife's share of couple income",
      y        = "Wife's mean weekly hours"
    ) +
    theme_minimal(base_size = 11)

  p_lfpr <- ggplot(agg[n >= 30], aes(x = bin_var, y = lfpr_mean * 100)) +
    geom_point(aes(size = n), color = "#2ca02c", alpha = 0.75) +
    geom_smooth(data = agg[bin_var <= 0.5 & n >= 30],
                method = "lm", formula = y ~ x,
                color = "#4575b4", se = TRUE, linewidth = 1.0) +
    geom_smooth(data = agg[bin_var > 0.5 & n >= 30],
                method = "lm", formula = y ~ x,
                color = "#d73027", se = TRUE, linewidth = 1.0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", linewidth = 0.8) +
    scale_size_continuous(range = c(1, 4), guide = "none") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title    = paste0("Wife's LFPR by income share bin (", era_label, ")"),
      subtitle = paste0("Income measure: ", income_label,
                        "; 2pp bins; LFPR = EMPSTAT in (1,2)"),
      x        = "Wife's share of couple income",
      y        = "Wife's LFPR (%)"
    ) +
    theme_minimal(base_size = 11)

  list(hours = p_hours, lfpr = p_lfpr, agg = agg)
}

# BKP replication: wage income, at-least-one-earner, interior only
bkp_int <- dt[!is.na(z_wage) & z_wage > 0 & z_wage < 1 & YEAR <= bkp_era_end]
ls_bkp <- make_labor_supply_plot(copy(bkp_int), "z_wage",
                                  era_label = "1980-2011, BKP income definition",
                                  income_label = "wage income (incwage)")

save_plot("bkp_replication_wife_hours_by_wage_share.png",
          { print(ls_bkp$hours) }, width = 1800, height = 1200)
save_plot("bkp_replication_wife_lfpr_by_wage_share.png",
          { print(ls_bkp$lfpr) }, width = 1800, height = 1200)

# Full era (1980-2023) with our income definition for direct comparison
ours_full <- dt[!is.na(z_earned) & YEAR >= 1980]
ls_ours <- make_labor_supply_plot(copy(ours_full), "z_earned",
                                   era_label = "1980-2023, our income definition",
                                   income_label = "no-transfer income (inctot - SS - welfare)")

save_plot("bkp_replication_wife_hours_by_earned_share_full_era.png",
          { print(ls_ours$hours) }, width = 1800, height = 1200)
save_plot("bkp_replication_wife_lfpr_by_earned_share_full_era.png",
          { print(ls_ours$lfpr) }, width = 1800, height = 1200)

# ── 6) BKP Table 3 equivalent: regression D(wife > husband) ───────────────────
# Outcome ~ D + year_FE, weighted by HHWT.
# D = 1 if wife's income share > 0.5 (wife earns more).
# BKP Table 3 shows this with and without controls; we do the bare version here.

reg_dt <- dt[!is.na(z_wage) & z_wage > 0 & z_wage < 1 & YEAR <= bkp_era_end]
reg_dt[, D_wife_more := as.integer(z_wage > 0.5)]
reg_dt[, year_f      := factor(YEAR)]

fit_hours_bkp   <- lm(female_weekly_hours             ~ D_wife_more + year_f, data = reg_dt, weights = HHWT)
fit_lfpr_bkp    <- lm(as.integer(female_empstat %in% c(1, 2)) ~ D_wife_more + year_f, data = reg_dt, weights = HHWT)
fit_emp_bkp     <- lm(as.integer(female_empstat == 1) ~ D_wife_more + year_f, data = reg_dt, weights = HHWT)
fit_husb_bkp    <- lm(male_weekly_hours               ~ D_wife_more + year_f, data = reg_dt, weights = HHWT)

extract_D <- function(fit, label) {
  s <- summary(fit)$coefficients
  data.table(
    outcome  = label,
    D_est    = round(s["D_wife_more", "Estimate"],   3),
    D_se     = round(s["D_wife_more", "Std. Error"], 3),
    D_p      = round(s["D_wife_more", "Pr(>|t|)"],  4),
    n_obs    = nobs(fit)
  )
}

reg_results <- rbindlist(list(
  extract_D(fit_hours_bkp,  "Wife weekly hours"),
  extract_D(fit_lfpr_bkp,   "Wife LFPR (empstat in 1,2)"),
  extract_D(fit_emp_bkp,    "Wife employed (empstat=1)"),
  extract_D(fit_husb_bkp,   "Husband weekly hours")
))
reg_results[, sig := fcase(D_p < 0.001, "***", D_p < 0.01, "**",
                           D_p < 0.05, "*", default = "")]

message("\nBKP Table 3 equivalent — D(wife earns more) coefficient:")
print(reg_results)
fwrite(reg_results, file.path(results_dir, "bkp_replication_reg_D_wife_more.csv"))

# ── 7) Density discontinuity test (McCrary) at 0.50 ──────────────────────────
# Uses the rddensity package (Cattaneo, Jansson & Ma). If not installed, skip.
if (requireNamespace("rddensity", quietly = TRUE)) {
  library(rddensity)

  # BKP sample, interior (both wage earners > 0), 1980-2011
  z_vec <- reg_dt[, rep(z_wage, round(HHWT / median(HHWT, na.rm = TRUE)))]
  rdd_test <- tryCatch(
    rddensity(X = z_vec, c = 0.5),
    error = function(e) { message("rddensity error: ", e$message); NULL }
  )
  if (!is.null(rdd_test)) {
    message("\nMcCrary density discontinuity test at 0.50 (BKP wage sample, 1980-2011):")
    print(summary(rdd_test))
  }
} else {
  message("rddensity not installed — skipping McCrary test. Install with: install.packages('rddensity')")
}

# ── 8) By-year density: has the kink weakened 1980-2023? ─────────────────────
anchor_years <- c(1980, 1990, 2005, 2010, 2015, 2020, 2023)

d_yr_bkp <- dt[
  !is.na(z_wage) & z_wage > 0 & z_wage < 1 & YEAR %in% anchor_years,
  .(wt = sum(HHWT)),
  by = .(YEAR, bin = round(z_wage * 50) / 50)
][order(YEAR, bin)]
d_yr_bkp[, share := wt / sum(wt), by = YEAR]

p_yr_bkp <- ggplot(d_yr_bkp[bin > 0.1 & bin < 0.9],
                   aes(x = bin, y = share * 100)) +
  geom_col(width = 0.018, fill = "steelblue4", alpha = 0.8) +
  geom_vline(xintercept = 0.5, color = "red", linewidth = 0.7, linetype = "dashed") +
  facet_wrap(~YEAR, ncol = 4) +
  labs(
    title    = "Wife's wage income share distribution by year (BKP running variable)",
    subtitle = "Interior sample (both wage earners > 0); 2pp bins; has the kink weakened over time?",
    x        = "Wife's share of couple wage income",
    y        = "Share of observations (%)"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"))

save_plot("bkp_replication_density_by_year.png",
          { print(p_yr_bkp) }, width = 2400, height = 1400)

# ── 9) Era comparison: BKP period (1980-2011) vs. post-BKP (2012-2023) ────────

dt[, era := fcase(
  YEAR <= bkp_era_end, paste0("BKP era (1980-", bkp_era_end, ")"),
  YEAR >  bkp_era_end, "Post-BKP (2012-2023)",
  default = NA_character_
)]

# 9a) Density side-by-side by era
d_era <- dt[
  !is.na(z_wage) & z_wage > 0 & z_wage < 1 & !is.na(era),
  .(wt = sum(HHWT)),
  by = .(era, bin = round(z_wage * 50) / 50)
][order(era, bin)]
d_era[, share := wt / sum(wt), by = era]

p_era_density <- ggplot(d_era[bin > 0.1 & bin < 0.9],
                        aes(x = bin, y = share * 100)) +
  geom_col(width = 0.018, fill = "steelblue4", alpha = 0.8) +
  geom_vline(xintercept = 0.5, color = "red", linewidth = 0.8, linetype = "dashed") +
  facet_wrap(~era, ncol = 2) +
  labs(
    title    = "Wife's wage income share: BKP era vs. post-BKP era",
    subtitle = "Interior sample (both wage earners > 0); 2pp bins; has the kink weakened?",
    x        = "Wife's share of couple wage income",
    y        = "Share of observations (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

save_plot("bkp_era_comparison_density.png",
          { print(p_era_density) }, width = 2000, height = 1100)

# 9b) Below/above ratio by year — trend line showing norm weakening over time
ratio_by_year <- dt[
  !is.na(z_wage) & z_wage > 0 & z_wage < 1,
  .(
    below = sum(HHWT[z_wage_bin >= 0.40 & z_wage_bin < 0.50]),
    above = sum(HHWT[z_wage_bin >  0.50 & z_wage_bin <= 0.60]),
    total = sum(HHWT)
  ),
  by = YEAR
][order(YEAR)]
ratio_by_year[, ratio := below / above]
ratio_by_year[, era   := fifelse(YEAR <= bkp_era_end, "BKP era", "Post-BKP")]

message("\nBelow/above ratio by year (interior, [0.40-0.50) vs (0.50-0.60]):")
print(ratio_by_year[, .(YEAR, below_pct = round(below/total*100,2),
                         above_pct = round(above/total*100,2), ratio = round(ratio,3), era)])

p_ratio_trend <- ggplot(ratio_by_year, aes(x = YEAR, y = ratio, color = era)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = bkp_era_end + 0.5, linetype = "dotted",
             color = "black", linewidth = 0.8) +
  annotate("text", x = bkp_era_end + 1, y = max(ratio_by_year$ratio) * 0.98,
           label = "BKP\npublished", hjust = 0, size = 3.0, color = "black") +
  scale_color_manual(values = c("BKP era" = "steelblue4", "Post-BKP" = "#d73027"),
                     name = NULL) +
  scale_y_continuous(limits = c(1, NA)) +
  labs(
    title    = "Below/above ratio at 0.50 threshold over time",
    subtitle = "Ratio > 1 = more couples just below than just above; ratio → 1 = norm weakening\nWindow: wife wage share in [0.40, 0.50) vs (0.50, 0.60]",
    x        = "Year",
    y        = "Weighted obs below 0.5 / above 0.5"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

save_plot("bkp_era_comparison_ratio_trend.png",
          { print(p_ratio_trend) }, width = 1800, height = 1100)

# 9c) Labor supply regression by era — does the D(wife > husband) effect shrink?
run_era_regs <- function(era_label, yr_min, yr_max) {
  sub <- dt[
    !is.na(z_wage) & z_wage > 0 & z_wage < 1 &
    YEAR >= yr_min & YEAR <= yr_max
  ][, `:=`(D_wife_more = as.integer(z_wage > 0.5), year_f = factor(YEAR))]

  fits <- list(
    "Wife weekly hours"          = lm(female_weekly_hours ~ D_wife_more + year_f, data = sub, weights = HHWT),
    "Wife LFPR (empstat in 1,2)" = lm(as.integer(female_empstat %in% c(1,2)) ~ D_wife_more + year_f, data = sub, weights = HHWT),
    "Husband weekly hours"       = lm(male_weekly_hours ~ D_wife_more + year_f, data = sub, weights = HHWT)
  )
  rbindlist(lapply(names(fits), function(nm) {
    s <- summary(fits[[nm]])$coefficients
    data.table(
      era     = era_label,
      outcome = nm,
      D_est   = round(s["D_wife_more", "Estimate"],   3),
      D_se    = round(s["D_wife_more", "Std. Error"], 3),
      D_p     = round(s["D_wife_more", "Pr(>|t|)"],  4),
      n_obs   = nobs(fits[[nm]])
    )
  }))
}

era_regs <- rbindlist(list(
  run_era_regs(paste0("BKP era (1980-", bkp_era_end, ")"), 1980L, bkp_era_end),
  run_era_regs("Post-BKP (2012-2023)", 2012L, 2023L)
))
era_regs[, sig := fcase(D_p < 0.001, "***", D_p < 0.01, "**",
                        D_p < 0.05, "*", default = "")]

message("\nD(wife earns more) coefficient by era:")
print(era_regs[, .(era, outcome, D_est, D_se, sig, n_obs)])
fwrite(era_regs, file.path(results_dir, "bkp_era_comparison_reg_results.csv"))

# 9d) Coefficient plot: D estimate ± 2SE by era and outcome
era_regs[, outcome_short := fcase(
  outcome == "Wife weekly hours",          "Wife hours",
  outcome == "Wife LFPR (empstat in 1,2)", "Wife LFPR",
  outcome == "Husband weekly hours",       "Husband hours"
)]

p_coef <- ggplot(era_regs,
                 aes(x = outcome_short, y = D_est,
                     ymin = D_est - 2 * D_se, ymax = D_est + 2 * D_se,
                     color = era, group = era)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.4), size = 0.8) +
  scale_color_manual(values = c(
    "BKP era (1980-2011)" = "steelblue4",
    "Post-BKP (2012-2023)" = "#d73027"
  ), name = NULL) +
  labs(
    title    = "Effect of wife out-earning husband (D coefficient) by era",
    subtitle = "OLS: outcome ~ D(wife wage share > 0.5) + year FEs; ±2 SE bars\nPositive = wife works more / husband works less when wife earns more",
    x        = NULL,
    y        = "Coefficient on D(wife earns more)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

save_plot("bkp_era_comparison_coef_plot.png",
          { print(p_coef) }, width = 1600, height = 1200)

# ── 10) Density and labor supply by decade ────────────────────────────────────
# Note: IPUMS extract has only 1980 and 1990 as single anchor years.
# Decades are labelled to reflect actual coverage.
dt[, decade := fcase(
  YEAR == 1980,              "1980 (single year)",
  YEAR == 1990,              "1990 (single year)",
  YEAR %in% 2005:2009,       "2000s (2005-2009)",
  YEAR %in% 2010:2023,       "2010-2023 (post-BKP)",
  default = NA_character_
)]
dt[, decade := factor(decade, levels = c(
  "1980 (single year)", "1990 (single year)",
  "2000s (2005-2009)", "2010-2023 (post-BKP)"
))]

# 10a) Density by decade
d_decade <- dt[
  !is.na(z_wage) & z_wage > 0 & z_wage < 1 & !is.na(decade),
  .(wt = sum(HHWT)),
  by = .(decade, bin = round(z_wage * 50) / 50)
][order(decade, bin)]
d_decade[, share := wt / sum(wt), by = decade]

p_decade_density <- ggplot(d_decade[bin > 0.1 & bin < 0.9],
                           aes(x = bin, y = share * 100)) +
  geom_col(width = 0.018, fill = "steelblue4", alpha = 0.8) +
  geom_vline(xintercept = 0.5, color = "red", linewidth = 0.8, linetype = "dashed") +
  facet_wrap(~decade, ncol = 4) +
  labs(
    title    = "Wife's wage income share distribution by decade (BKP running variable)",
    subtitle = "Interior sample (both wage earners > 0); 2pp bins; 1980/1990 are single anchor years",
    x        = "Wife's share of couple wage income",
    y        = "Share of observations (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

save_plot("bkp_decade_density.png",
          { print(p_decade_density) }, width = 2400, height = 1000)

# 10b) Below/above ratio by decade
decade_summary <- dt[
  !is.na(z_wage) & z_wage > 0 & z_wage < 1 & !is.na(decade),
  .(
    below = sum(HHWT[z_wage_bin >= 0.40 & z_wage_bin < 0.50]),
    above = sum(HHWT[z_wage_bin >  0.50 & z_wage_bin <= 0.60]),
    n     = .N
  ),
  by = decade
][order(decade)]
decade_summary[, ratio := round(below / above, 3)]

message("\nBelow/above ratio by decade:")
print(decade_summary[, .(decade, ratio, n)])

p_decade_ratio <- ggplot(decade_summary,
                         aes(x = as.integer(decade), y = ratio)) +
  geom_line(linewidth = 1.1, color = "steelblue4") +
  geom_point(size = 3.5, color = "steelblue4") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  scale_x_continuous(breaks = seq_along(levels(decade_summary$decade)),
                     labels = levels(decade_summary$decade)) +
  labs(
    title    = "Below/above ratio at the 0.50 threshold by decade",
    subtitle = "Ratio of weighted couples in [0.40, 0.50) vs (0.50, 0.60]; ratio = 1 means no bunching",
    x        = NULL, y = "Below / above ratio"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

save_plot("bkp_decade_ratio.png",
          { print(p_decade_ratio) }, width = 1600, height = 1000)

message("BKP replication complete.")
