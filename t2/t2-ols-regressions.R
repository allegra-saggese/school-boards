library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(broom)

source(here::here("_setup.R"))

# T2 — descriptive OLS, county and household level. No causal claims.
#
# Every model is reported TWICE: once with classical OLS SEs and once with
# cluster-robust SEs, stacked in the output with an se_type column. The
# political treatment (vote_margin / conservative) varies at the COUNTY level,
# so classical SEs understate uncertainty; the clustered rows are the ones to
# report. Household models are additionally clustered at the state level, which
# is the more conservative choice if the vote margin is spatially correlated
# across counties within a state.
#
# Inputs : data/processed/panel/*_lfpr_panel_with_groups.csv
#          data/processed/panel/ipums_married_oppositesex_spouse_pairs_with_groups.csv
# Outputs: data/processed/results/ols_county_female_lfpr_results.csv
#          data/processed/results/ols_hh_hours_results.csv

# =========================================================
# 0) Configuration
# =========================================================
# All regressions are descriptive OLS — no causal claims.
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

suppressPackageStartupMessages({library(sandwich); library(lmtest)})

# Cluster ids for the rows the model ACTUALLY used.
#
# lm() drops rows with NA in any model variable, so a cluster vector taken
# straight from the source data can be longer than the fitted sample and
# silently misalign every observation with the wrong cluster. Reading the row
# indices back off the model frame is the only way to guarantee they match.
cluster_ids <- function(model, data, var) {
  idx <- as.integer(rownames(model.frame(model)))
  stopifnot(length(idx) == nobs(model))
  data[[var]][idx]
}

# Cluster-robust SEs, keeping the point estimates from the fitted model.
cluster_se <- function(model, data, var) {
  tidy(coeftest(model, vcov = vcovCL(model, cluster = cluster_ids(model, data, var))))
}

# One model, both SE flavours, stacked and labelled.
both_ses <- function(model, data, model_name, cluster_vars = "fips") {
  out <- list(tidy(model) %>% mutate(se_type = "classical"))
  for (v in cluster_vars) {
    out[[length(out) + 1]] <- cluster_se(model, data, v) %>%
      mutate(se_type = paste0("clustered_", v))
  }
  bind_rows(out) %>% mutate(model = model_name)
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

# state_fac is already absorbed as fixed effects here, so clustering is on
# county only; a state cluster would be collinear with the state FEs' own level.
county_ols_out <- bind_rows(
  both_ses(m1a, panel, "1a_linear_income_x_votemargin"),
  both_ses(m1b, panel, "1b_quad_income_x_votemargin"),
  both_ses(m1c, panel, "1c_quintile_x_conservative")
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

hh_working <- hh %>% filter(female_empstat == 1)

# Clustered on county (the level the political treatment varies at) and on
# state (more conservative, if the vote margin is spatially correlated across
# counties within a state).
hh_clusters <- c("fips", "state_fips")

# Fitted ONE AT A TIME and discarded after summarising. At 14.1M rows each lm
# object carries a QR factorisation of several GB, and vcovCL builds an
# estimating-functions matrix of comparable size; holding all four at once
# exhausts memory on this machine. Only the small tidy summaries are kept.
hh_specs <- list(
  list(name = "2a_hours_quintile_x_conservative",
       f    = female_weekly_hours ~ quintile_fac * conservative + year_fac + nchild,
       data = "hh"),
  list(name = "2b_work_share_quintile_x_conservative",
       f    = female_working ~ quintile_fac * conservative + year_fac + nchild,
       data = "hh"),
  list(name = "2c_hours_conditional_working",
       f    = female_weekly_hours ~ quintile_fac * conservative + year_fac + nchild,
       data = "hh_working"),
  list(name = "2d_hours_gap_quintile_x_conservative",
       f    = I(male_weekly_hours - female_weekly_hours) ~ quintile_fac * conservative +
              year_fac + nchild,
       data = "hh")
)

hh_results <- lapply(hh_specs, function(s) {
  message("  fitting ", s$name, " ...")
  d   <- get(s$data)
  # weights must be named as a COLUMN of `data`: lm evaluates it in the
  # formula's environment (global here), so a local object is not visible.
  fit <- lm(s$f, data = d, weights = HHWT)
  out <- both_ses(fit, d, s$name, hh_clusters)
  rm(fit); invisible(gc(verbose = FALSE))
  out
})

results_2b  <- hh_results[[2]]           # kept for the coefficient plot below
hh_ols_out  <- bind_rows(hh_results) %>%
  filter(!grepl("^year_fac", term))      # drop year FE rows

write_csv(hh_ols_out, file.path(results_dir, "ols_hh_hours_results.csv"))
message("Household OLS results written.")

# =========================================================
# 3) Coefficient plot: quintile × conservative interaction (Model 2b)
# =========================================================
# Key coefficient: does the income-quintile gradient in female work share differ
# by political direction? If norm effect is income-elastic, Q5 × conservative
# should be the largest (most negative) coefficient.

# County-clustered SEs for the error bars — results_2b now holds three SE
# flavours per term, and the classical ones would draw misleadingly tight bars.
interaction_terms <- results_2b %>%
  filter(se_type == "clustered_fips",
         grepl("quintile_fac.*conservative|conservative.*quintile_fac", term)) %>%
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
      subtitle = paste("Model 2b; baseline = Q1 × Democratic-majority;",
                       "bars = 95% CI, SEs clustered by county"),
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
