library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(broom)

source("R/paths.R")
source("functions.R")

# =========================================================
# 0) Configuration
# =========================================================
# All regressions are descriptive OLS — no causal claims.
# SEs clustered at the county (FIPS) level.
# Goal: verify that descriptive patterns are robust to controls
# and get effect sizes to anchor the intra-HH bargaining model.
#
# Regressions:
#   1. County-level: female_lfpr ~ log_income * vote_margin + year + state FEs
#   2. HH-level (IPUMS): wife_weekly_hours ~ income_quintile * conservative + year FE
#   3. HH-level, conditional on wife working
#   4. HH-level: female work share ~ income_quintile * conservative + year FE + nchild
#
# Required inputs:
#   - lfpr_panel_with_groups (from lfpr-groupings.R)
#   - ipums_married_oppositesex_spouse_pairs_with_groups (from ipums-county-household-analysis.R Section 7)

panel_dir   <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
ensure_dir(results_dir)

cluster_se <- function(model, cluster_var) {
  # Heteroskedasticity- and cluster-robust SEs using sandwich estimator.
  # Requires: sandwich, lmtest packages (install if needed).
  if (!requireNamespace("sandwich", quietly = TRUE) ||
      !requireNamespace("lmtest", quietly = TRUE)) {
    message("Install 'sandwich' and 'lmtest' for clustered SEs. Returning OLS SEs.")
    return(tidy(model))
  }
  library(sandwich)
  library(lmtest)
  coeftest(model, vcov = vcovCL(model, cluster = cluster_var)) %>%
    tidy()
}

# =========================================================
# 1) County-level OLS: female LFPR ~ log_income × vote_margin
# =========================================================
panel_files <- list.files(panel_dir, pattern = "_lfpr_panel_with_groups\\.csv$", full.names = TRUE)
if (length(panel_files) == 0) stop("No lfpr_panel_with_groups file. Run lfpr-groupings.R first.")
panel <- read_csv(sort(panel_files)[length(panel_files)], show_col_types = FALSE) %>%
  filter(!is.na(lfpr_female), !is.na(log_income), !is.na(vote_margin), !is.na(state))

# Normalize vote_margin to [-1, 1] (already approximately there; positive = more Dem)
panel <- panel %>%
  mutate(
    year_fac  = factor(year),
    state_fac = factor(state),
    conservative = as.numeric(vote_margin < 0)  # 1 = Republican-majority county
  )

# Model 1a: linear income × vote_margin interaction
m1a <- lm(lfpr_female ~ log_income * vote_margin + year_fac + state_fac, data = panel)

# Model 1b: quadratic income × vote_margin interaction
m1b <- lm(lfpr_female ~ (log_income + I(log_income^2)) * vote_margin + year_fac + state_fac,
          data = panel)

# Model 1c: binary conservative × income quintile interaction
m1c <- lm(lfpr_female ~ factor(income_quintile_national) * conservative + year_fac + state_fac,
          data = panel)

results_1a <- cluster_se(m1a, panel[complete.cases(panel[, c("log_income","vote_margin","state","year")]), "fips"])
results_1b <- cluster_se(m1b, panel[complete.cases(panel[, c("log_income","vote_margin","state","year")]), "fips"])
results_1c <- cluster_se(m1c, panel[complete.cases(panel[, c("income_quintile_national","conservative","state","year")]), "fips"])

# Save county-level results
county_ols_out <- bind_rows(
  results_1a %>% mutate(model = "1a_linear_income_x_votemargin"),
  results_1b %>% mutate(model = "1b_quad_income_x_votemargin"),
  results_1c %>% mutate(model = "1c_quintile_x_conservative")
) %>%
  filter(!grepl("^year_fac|^state_fac", term))  # drop FE rows for readability

write_csv(county_ols_out, file.path(results_dir, "ols_county_female_lfpr_results.csv"))
message("County OLS results written.")

# =========================================================
# 2) Household-level OLS (IPUMS): wife's hours ~ quintile × conservative
# =========================================================
merged_file <- file.path(panel_dir, "ipums_married_oppositesex_spouse_pairs_with_groups.csv")
if (!file.exists(merged_file)) {
  message("Skipping HH-level regressions: merged political file not found.")
  message("Run Section 7 of ipums-county-household-analysis.R first.")
  quit(save = "no")
}

hh <- read_csv(merged_file, show_col_types = FALSE) %>%
  filter(!is.na(income_quintile), !is.na(vote_margin)) %>%
  mutate(
    conservative   = as.numeric(vote_margin < 0),
    year_fac       = factor(YEAR),
    quintile_fac   = factor(income_quintile),
    female_working = as.numeric(female_empstat == 1)
  )

# Model 2a: wife hours ~ quintile * conservative + year FE + nchild
m2a <- lm(female_weekly_hours ~ quintile_fac * conservative + year_fac + nchild,
          data = hh, weights = HHWT)

# Model 2b: wife work share (extensive margin) ~ quintile * conservative + year FE + nchild
m2b <- lm(female_working ~ quintile_fac * conservative + year_fac + nchild,
          data = hh, weights = HHWT)

# Model 2c: conditional on wife working — intensive margin only
hh_working <- hh %>% filter(female_empstat == 1)
m2c <- lm(female_weekly_hours ~ quintile_fac * conservative + year_fac + nchild,
          data = hh_working, weights = HHWT)

# Model 2d: hours gap (male - female) ~ quintile * conservative + year FE + nchild
m2d <- lm(I(male_weekly_hours - female_weekly_hours) ~ quintile_fac * conservative + year_fac + nchild,
          data = hh, weights = HHWT)

results_2a <- tidy(m2a) %>% mutate(model = "2a_hours_quintile_x_conservative")
results_2b <- tidy(m2b) %>% mutate(model = "2b_work_share_quintile_x_conservative")
results_2c <- tidy(m2c) %>% mutate(model = "2c_hours_conditional_working")
results_2d <- tidy(m2d) %>% mutate(model = "2d_hours_gap_quintile_x_conservative")

hh_ols_out <- bind_rows(results_2a, results_2b, results_2c, results_2d) %>%
  filter(!grepl("^year_fac", term))  # drop year FE rows

write_csv(hh_ols_out, file.path(results_dir, "ols_hh_hours_results.csv"))
message("Household OLS results written.")

# =========================================================
# 3) Coefficient plot: quintile × conservative interaction (Model 2b)
# =========================================================
# Key coefficient: does the income-quintile gradient in female work share differ
# by political direction? If norm effect is income-elastic, Q5 × conservative
# should be the largest (most negative) coefficient.

interaction_terms <- results_2b %>%
  filter(grepl("quintile_fac.*conservative|conservative.*quintile_fac", term)) %>%
  mutate(
    quintile = as.integer(gsub(".*quintile_fac(\\d).*", "\\1", term))
  )

if (nrow(interaction_terms) > 0) {
  p_coef <- ggplot(interaction_terms, aes(x = quintile, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error),
                  width = 0.15, color = "#d73027") +
    geom_point(size = 3, color = "#d73027") +
    scale_x_continuous(breaks = 2:5,
                       labels = c("Q2×Rep", "Q3×Rep", "Q4×Rep", "Q5×Rep")) +
    labs(
      title    = "Interaction: income quintile × conservative county on female work share",
      subtitle = "Model 2b; baseline = Q1 × Democratic-majority; bars = 95% CI",
      x        = "Income quintile (Republican-majority county interaction)",
      y        = "Coefficient on female work probability"
    ) +
    theme_minimal(base_size = 12)
  save_plot("ols_coef_quintile_conservative_interaction.png",
            { print(p_coef) }, width = 1600, height = 1100)
}

message("OLS regression script complete.")
message("County results: ", file.path(results_dir, "ols_county_female_lfpr_results.csv"))
message("HH results:    ", file.path(results_dir, "ols_hh_hours_results.csv"))
