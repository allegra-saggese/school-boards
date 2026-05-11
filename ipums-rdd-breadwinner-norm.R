library(data.table)
library(ggplot2)

source("functions.R")
source("R/paths.R")

# =========================================================
# RDD: breadwinner norm threshold (female earned-income share = 0.5)
#
# Running variable: z_earned = female_income_no_transfers /
#                              (female_income_no_transfers + male_income_no_transfers)
# where female_income_no_transfers = pmax(female_inctot - female_incss - female_incwelfr, 0)
# and   male_income_no_transfers   = pmax(male_inctot   - male_incss   - male_incwelfr,   0)
#
# Restricted to couples where BOTH have earned income > 0 (z_earned in (0,1)).
# This avoids the denominator-collapse problem: we sum the two spouses' individual
# earned incomes, not the household total, so the denominator cannot be driven to
# zero by transfers from other household members.
#
# Donut design: exclude [0.5 - donut_w, 0.5 + donut_w] from the RDD regression.
# The mass point at exactly 0.50 is partially mechanical (couples reporting identical
# earnings figures) and not informative about avoidance behavior. The donut removes
# this artifact without discarding the broader distributional evidence.
# Primary donut width: 0.02 (±2pp). Robustness: 0.01 and 0.03.
# =========================================================

# ── 0) Config ────────────────────────────────────────────────────────────────
anchor_years <- c(1980, 1990, 2005, 2010, 2015, 2020, 2023)
rdd_bw       <- 0.20   # bandwidth on each side of threshold
donut_primary <- 0.02  # primary donut half-width (drops [0.48, 0.52])
donut_grid    <- c(0.01, 0.02, 0.03)  # robustness bandwidths

panel_dir   <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
ensure_dir(results_dir)

# ── 1) Load pair file, compute earned-income running variable ─────────────────
cols <- c("YEAR", "HHWT", "STATEICP", "COUNTYICP",
          "female_income_no_transfers", "male_income_no_transfers",
          "female_weekly_hours", "male_weekly_hours",
          "female_empstat", "income_quintile")

message("Reading pair file ...")
dt <- fread(
  file.path(panel_dir, "ipums_married_oppositesex_spouse_pairs_with_kids.csv"),
  select = cols, showProgress = FALSE
)

# Earned income: inctot minus individual SS and welfare (already computed in pair file)
# The pair file stores these as non-negative values clipped at zero.
dt[, f_earn := female_income_no_transfers]
dt[, m_earn := male_income_no_transfers]
dt[, couple_earn := f_earn + m_earn]

# z_earned: valid only where both spouses have positive earned income
dt[, z_earned := fifelse(f_earn > 0 & m_earn > 0,
                         f_earn / couple_earn,
                         NA_real_)]

# 1pp bins for density plots
dt[, z_bin := round(z_earned * 100) / 100]

n_valid <- dt[!is.na(z_earned), .N]
message("Valid z_earned obs (both earned income > 0): ", n_valid,
        " (", round(n_valid / nrow(dt) * 100, 1), "% of all spouse pairs)")
message("Std dev z_earned: ", round(sd(dt$z_earned, na.rm = TRUE), 3))

# Mass at exactly 0.50
mass_at_half <- dt[!is.na(z_earned), .(at_half = sum(HHWT[z_bin == 0.50]) / sum(HHWT))]
message("Weighted share at z_bin == 0.50: ", round(mass_at_half$at_half * 100, 2), "%")

# ── 2) Density: full distribution with donut region annotated ─────────────────
d_all <- dt[!is.na(z_earned), .(wt = sum(HHWT)), by = z_bin][order(z_bin)]
d_all[, share := wt / sum(wt)]
d_all[, in_donut := abs(z_bin - 0.50) < donut_primary]

p_full <- ggplot(d_all[z_bin > 0.03 & z_bin < 0.97],
                 aes(x = z_bin, y = share * 100,
                     fill = in_donut, alpha = in_donut)) +
  geom_col(width = 0.009) +
  geom_vline(xintercept = 0.5, color = "red", linewidth = 0.9, linetype = "dashed") +
  annotate("rect",
           xmin = 0.5 - donut_primary, xmax = 0.5 + donut_primary,
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.08) +
  annotate("text", x = 0.53, y = max(d_all$share * 100) * 0.90,
           label = sprintf("Donut hole\n[%.2f, %.2f]\nexcluded from RDD",
                           0.5 - donut_primary, 0.5 + donut_primary),
           hjust = 0, size = 3.0, color = "red") +
  scale_fill_manual(values  = c("FALSE" = "steelblue4", "TRUE" = "firebrick3"),
                    guide   = "none") +
  scale_alpha_manual(values = c("FALSE" = 0.85, "TRUE" = 0.5), guide = "none") +
  labs(
    title    = "Distribution of wife's earned income share — donut RDD design",
    subtitle = paste0(
      "Earned income = inctot minus individual SS & welfare; both spouses > 0; IPUMS 1980-2023\n",
      "Red shaded region (±", donut_primary * 100, "pp around 0.50) excluded from regression — ",
      "mechanical mass point"
    ),
    x = "Wife's share of couple earned income (z_earned)",
    y = "Share of weighted observations (%)"
  ) +
  theme_minimal(base_size = 11)
save_plot("rdd_donut_density_earned_income_share_all_years.png",
          { print(p_full) }, width = 1800, height = 1100)

# ── 3) Density by anchor year ─────────────────────────────────────────────────
d_yr <- dt[!is.na(z_earned) & YEAR %in% anchor_years,
           .(wt = sum(HHWT)), by = .(YEAR, z_bin)][order(YEAR, z_bin)]
d_yr[, share := wt / sum(wt), by = YEAR]

p_yr <- ggplot(d_yr[z_bin > 0.05 & z_bin < 0.95],
               aes(x = z_bin, y = share * 100)) +
  geom_col(width = 0.009, fill = "steelblue4", alpha = 0.8) +
  geom_vline(xintercept = 0.5, color = "red", linewidth = 0.7, linetype = "dashed") +
  annotate("rect",
           xmin = 0.5 - donut_primary, xmax = 0.5 + donut_primary,
           ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.08) +
  facet_wrap(~YEAR, ncol = 4) +
  labs(
    title    = "Wife's earned income share distribution by year",
    subtitle = "Has the drop above 0.5 (avoidance) weakened over time? Red band = donut region",
    x = "Wife's share of couple earned income",
    y = "Share of observations (%)"
  ) +
  theme_minimal(base_size = 10)
save_plot("rdd_donut_density_earned_income_share_by_year.png",
          { print(p_yr) }, width = 2400, height = 1400)

# ── 4) Dem vs Rep (2010-2020) ─────────────────────────────────────────────────
xwalk <- data.table(
  STATEICP  = c(1,2,3,4,5,6,11,12,13,14,21,22,23,24,25,
                31,32,33,34,35,36,37,40,41,42,43,44,45,46,47,48,49,
                51,52,53,54,56,61,62,63,64,65,66,67,68,71,72,73,81,82,98),
  state_fips = c(9,23,25,33,44,50,10,34,36,42,17,18,26,39,55,
                 19,20,27,29,31,38,46,51,1,5,12,13,22,28,37,45,48,
                 21,24,40,47,54,4,8,16,30,32,35,49,56,6,41,53,2,15,11)
)

pol_dt <- dt[!is.na(z_earned) & YEAR %in% 2010:2020 & COUNTYICP > 0]
pol_dt <- merge(pol_dt, xwalk, by = "STATEICP", all.x = TRUE)
pol_dt[, fips := fifelse(!is.na(state_fips),
  sprintf("%02d%03d", state_fips, as.integer(floor(COUNTYICP / 10))),
  NA_character_)]

panel_files <- list.files(panel_dir,
  pattern = "_lfpr_panel_with_groups[.]csv$", full.names = TRUE)
pol_panel <- fread(sort(panel_files)[length(panel_files)],
                   select = c("fips", "year", "vote_margin"))
pol_panel[, fips := as.character(fips)]
setnames(pol_panel, "year", "YEAR")
pol_dt <- merge(pol_dt, pol_panel, by = c("fips", "YEAR"), all.x = TRUE)
pol_dt <- pol_dt[!is.na(vote_margin)]
pol_dt[, political := fifelse(vote_margin > 0,
                              "Democratic-majority", "Republican-majority")]
message("Political-matched earned-income obs: ", nrow(pol_dt))

d_pol <- pol_dt[, .(wt = sum(HHWT)), by = .(political, z_bin)][order(political, z_bin)]
d_pol[, share := wt / sum(wt), by = political]

# Below/above ratio EXCLUDING the donut
for (g in c("Democratic-majority", "Republican-majority")) {
  below <- d_pol[political == g & z_bin >= 0.40 & z_bin < (0.5 - donut_primary),  sum(share)]
  above <- d_pol[political == g & z_bin >  (0.5 + donut_primary) & z_bin <= 0.60, sum(share)]
  message(g, " (donut excluded) | [0.40, ", 0.5 - donut_primary, "): ",
          round(below * 100, 2), "% | (", 0.5 + donut_primary, ", 0.60]: ",
          round(above * 100, 2), "% | ratio: ", round(below / above, 3))
}

p_pol <- ggplot(d_pol[z_bin > 0.05 & z_bin < 0.95],
                aes(x = z_bin, y = share * 100, fill = political)) +
  geom_col(width = 0.009, alpha = 0.85) +
  geom_vline(xintercept = 0.5, linewidth = 0.8, linetype = "dashed") +
  annotate("rect",
           xmin = 0.5 - donut_primary, xmax = 0.5 + donut_primary,
           ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.15) +
  scale_fill_manual(values = c("Democratic-majority" = "#4575b4",
                                "Republican-majority"  = "#d73027")) +
  facet_wrap(~political, ncol = 1) +
  labs(
    title    = "Wife's earned income share: Dem vs Rep counties (2010-2020)",
    subtitle = paste0(
      "Donut region ±", donut_primary * 100, "pp shaded — excluded from ratio and regression\n",
      "Higher below/above ratio in Rep counties = stronger breadwinner-status avoidance"
    ),
    x = "Wife's share of couple earned income",
    y = "Share of observations (%)",
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")
save_plot("rdd_donut_density_earned_income_share_dem_vs_rep.png",
          { print(p_pol) }, width = 1800, height = 1400)

# ── 5) Donut kink RDD: outcome = wife's weekly hours ────────────────────────
# Sample: |z - 0.5| in (donut_primary, rdd_bw] — outside donut, inside bandwidth
rdd_dt <- dt[
  !is.na(z_earned) & !is.na(female_weekly_hours) &
  abs(z_earned - 0.5) > donut_primary &
  abs(z_earned - 0.5) <= rdd_bw &
  YEAR %in% c(1980, 1990, 2005:2023)
]
rdd_dt[, z_c  := z_earned - 0.5]
rdd_dt[, D    := as.integer(z_c >= 0)]
rdd_dt[, D_zc := D * z_c]
rdd_dt[, year_f := factor(YEAR)]

message("\nDonut RDD sample: ", nrow(rdd_dt), " obs (donut ±",
        donut_primary * 100, "pp, bandwidth ±", rdd_bw * 100, "pp)")

fit_hours   <- lm(female_weekly_hours          ~ z_c + D + D_zc + year_f, data = rdd_dt, weights = HHWT)
fit_empstat <- lm(as.integer(female_empstat==1) ~ z_c + D + D_zc + year_f, data = rdd_dt, weights = HHWT)
fit_husb    <- lm(male_weekly_hours             ~ z_c + D + D_zc + year_f, data = rdd_dt, weights = HHWT)

extract_rdd <- function(fit, label) {
  s <- summary(fit)$coefficients
  data.table(
    outcome  = label,
    term     = c("Slope below (z_c)", "Jump at 0.5 (D)", "Kink (D×z_c)"),
    estimate = s[c("z_c","D","D_zc"), "Estimate"],
    se       = s[c("z_c","D","D_zc"), "Std. Error"],
    p_value  = s[c("z_c","D","D_zc"), "Pr(>|t|)"],
    donut_w  = donut_primary,
    bw       = rdd_bw
  )
}

results_primary <- rbindlist(list(
  extract_rdd(fit_hours,   "Wife weekly hours"),
  extract_rdd(fit_empstat, "Wife employed (0/1)"),
  extract_rdd(fit_husb,    "Husband weekly hours")
))
results_primary[, sig := fcase(p_value < 0.001, "***",
                               p_value < 0.01,  "**",
                               p_value < 0.05,  "*",
                               default          = "")]

message("\nPrimary donut RDD results (earned income, donut ±",
        donut_primary * 100, "pp, bandwidth ±", rdd_bw * 100, "pp):")
print(results_primary[, .(outcome, term,
  estimate = round(estimate, 3), se = round(se, 3),
  p_value  = round(p_value, 4), sig)])

# ── 6) Robustness: donut width sensitivity ───────────────────────────────────
message("\nDonut-width robustness (wife weekly hours, D coefficient only):")
rob <- rbindlist(lapply(donut_grid, function(dw) {
  sub <- dt[
    !is.na(z_earned) & !is.na(female_weekly_hours) &
    abs(z_earned - 0.5) > dw &
    abs(z_earned - 0.5) <= rdd_bw &
    YEAR %in% c(1980, 1990, 2005:2023)
  ][, `:=`(z_c = z_earned - 0.5, D = as.integer(z_earned >= 0.5),
            year_f = factor(YEAR))]
  sub[, D_zc := D * z_c]
  fit <- lm(female_weekly_hours ~ z_c + D + D_zc + year_f, data = sub, weights = HHWT)
  s <- summary(fit)$coefficients
  data.table(
    donut_w  = dw,
    n_obs    = nrow(sub),
    D_est    = round(s["D","Estimate"], 3),
    D_se     = round(s["D","Std. Error"], 3),
    D_p      = round(s["D","Pr(>|t|)"], 4),
    kink_est = round(s["D_zc","Estimate"], 3),
    kink_p   = round(s["D_zc","Pr(>|t|)"], 4)
  )
}))
print(rob)

# ── 7) Visual: donut kink (wife hours, 2pp bins, 2010-2020) ──────────────────
vis_dt <- dt[
  !is.na(z_earned) & YEAR %in% 2010:2020 &
  abs(z_earned - 0.5) <= rdd_bw
]
vis_dt[, z_bin2 := round(z_earned * 50) / 50]  # 2pp bins
vis_agg <- vis_dt[,
  .(hours_mean = weighted.mean(female_weekly_hours, HHWT, na.rm = TRUE),
    n = .N),
  by = z_bin2][order(z_bin2)]

# Mark which bins fall in the donut
vis_agg[, donut := abs(z_bin2 - 0.5) < donut_primary]

p_kink <- ggplot(vis_agg[n >= 50 & !donut],
                 aes(x = z_bin2, y = hours_mean)) +
  geom_point(aes(size = n), color = "steelblue4", alpha = 0.75) +
  geom_smooth(data = vis_agg[z_bin2 < (0.5 - donut_primary) & n >= 50],
              method = "lm", formula = y ~ x,
              color = "#4575b4", se = TRUE, linewidth = 1.1) +
  geom_smooth(data = vis_agg[z_bin2 > (0.5 + donut_primary) & n >= 50],
              method = "lm", formula = y ~ x,
              color = "#d73027", se = TRUE, linewidth = 1.1) +
  annotate("rect",
           xmin = 0.5 - donut_primary, xmax = 0.5 + donut_primary,
           ymin = -Inf, ymax = Inf, fill = "grey40", alpha = 0.12) +
  annotate("text", x = 0.5, y = min(vis_agg$hours_mean, na.rm = TRUE) + 0.3,
           label = "donut", hjust = 0.5, size = 2.8, color = "grey40") +
  geom_vline(xintercept = 0.5, linetype = "dashed", linewidth = 0.8) +
  scale_size_continuous(range = c(1, 4.5), guide = "none") +
  labs(
    title    = "Donut kink RDD: wife's hours around the breadwinner threshold",
    subtitle = paste0(
      "Earned income share running variable; 2pp bins; 2010-2020\n",
      "Grey band = excluded donut (±", donut_primary * 100, "pp); ",
      "blue = fit below, red = fit above threshold"
    ),
    x = "Wife's earned income share (z_earned)",
    y = "Wife's mean weekly hours"
  ) +
  theme_minimal(base_size = 11)
save_plot("rdd_donut_kink_wife_hours_earned_income.png",
          { print(p_kink) }, width = 1800, height = 1200)

# ── 8) Save all regression results ───────────────────────────────────────────
fwrite(results_primary,
  file.path(results_dir, "rdd_donut_breadwinner_norm_results.csv"))

message("\nDone. All outputs in data/graphs/ and data/processed/results/.")
message("Key result files:")
message("  rdd_donut_density_earned_income_share_all_years.png")
message("  rdd_donut_density_earned_income_share_dem_vs_rep.png")
message("  rdd_donut_kink_wife_hours_earned_income.png")
message("  rdd_donut_breadwinner_norm_results.csv")
