library(data.table)
library(ggplot2)

source("functions.R")
source("R/paths.R")

# =========================================================
# Joint household utility model with income-elastic breadwinner norm
#
# Model:
#   max_{h_H, h_W >= 0}  log(c)
#                        - [kappa_H / (1 + 1/eps)] * h_H^(1 + 1/eps)
#                        - [kappa_W / (1 + 1/eps)] * h_W^(1 + 1/eps)
#                        - theta(tau, y) * 1[z_W >= 0.5]
#
#   s.t.  c   = w_H*h_H + w_W*h_W + y0      (pooled consumption)
#         z_W = w_W*h_W / (w_H*h_H + w_W*h_W) (wife's income share)
#
#   Norm penalty: theta(tau, y) = tau * lambda * (y / y_bar)
#     tau   = norm intensity proxy (0 = fully Dem, 1 = fully Rep)
#     lambda = scale parameter to calibrate
#     y_bar  = median household income in sample (normalizes income units)
#
# Mechanism: at low y, theta is small regardless of tau -> no gap.
#            at high y, theta is large in high-tau counties -> wife bunches
#            at z_W = 0.5 rather than paying the norm penalty -> gap opens.
#
# Solver strategy: the indicator creates a kink at z_W = 0.5. We solve
# three candidates and take the utility-maximizing one:
#   (A) Unconstrained below threshold (norm does not activate)
#   (B) Constrained at z_W = 0.5 exactly (bunching at threshold)
#   (C) Unconstrained above threshold (norm activated, penalty paid)
#
# Calibration targets (from ipums-rdd-breadwinner-norm.R outputs):
#   M1: female work share gap Q5 Dem vs Rep = 2.35pp  [primary norm target]
#   M2: female work share gap Q1 Dem vs Rep = 0pp     [income-elasticity shape]
#   M3: wife hours gap D10 Rep minus Dem   = 1.9 hrs  [intensive margin]
#   M4: kink in wife hours at z=0.5        = -25.2 hrs/pp
#   M5: BKP avoidance ratio Dem            = 1.423
#   M6: BKP avoidance ratio Rep            = 1.499
#
# Free parameters: lambda (norm scale), kappa_W (wife hours cost).
# Fixed: eps = 0.5 (Frisch elasticity, standard), kappa_H = 1 (normalised).
# =========================================================

today       <- format(Sys.Date(), "%Y-%m-%d")
panel_dir   <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
ensure_dir(results_dir)
# graphs_dir() is already defined in R/paths.R — do not shadow it

# ── 1) Parameters ─────────────────────────────────────────────────────────────

params <- list(
  eps     = 1.0,      # Frisch elasticity — fix at 1 (standard macro labour supply)
  kappa_H = 6.25e-4,  # husband hours disutility scale — pinned from FOC at avg hrs
                      # derivation: w_H/c = kappa * h_H at (w=2000, c=80000, h=40) -> kappa=6.25e-4
  kappa_W = 9.00e-4,  # wife starting value — higher = fewer hours, calibrated below
  lambda  = 0.02,     # norm penalty scale — starting value informed by ΔU analysis:
                      # income-elastic window is [0.0097, 0.0486] at median wages
                      # below 0.0097: norm never binds; above 0.0486: binds even at Q1
  h_max   = 100,      # weekly hours ceiling (~96 hrs/wk)
  tau_Dem = 0.0,      # norm intensity in Dem counties (baseline = 0)
  tau_Rep = 1.0,      # norm intensity in Rep counties (normalised)
  y_bar   = NA_real_  # median HH income — filled from data in Section 3
)
# All hours in weekly units throughout (h in [0, 100]).
# Wages w_j = annual income / weekly hours = annual $ per weekly-hour.
# Budget: c = w_H*h_H + w_W*h_W + y0 is in annual $, because
#   (annual $ / weekly-hour) × (weekly hours) = annual $.
# This keeps kappa O(1e-4) instead of O(1e-10) that annual hours would require.

# ── 2) Core functions ──────────────────────────────────────────────────────────

# Hours disutility: V(h) = kappa / (1 + 1/eps) * h^(1 + 1/eps)
disutility <- function(h, kappa, eps) {
  kappa / (1 + 1 / eps) * pmax(h, 0)^(1 + 1 / eps)
}

# Norm penalty: theta(tau, y) = tau * lambda * (y / y_bar)
norm_penalty <- function(tau, y, lambda, y_bar) {
  tau * lambda * (y / y_bar)
}

# Household utility given (h_H, h_W) and household characteristics
hh_utility <- function(h_H, h_W, w_H, w_W, y0, tau, y,
                        kappa_H, kappa_W, eps, lambda, y_bar) {
  h_H <- pmax(h_H, 0); h_W <- pmax(h_W, 0)
  earned <- w_H * h_H + w_W * h_W
  c      <- earned + y0
  if (c <= 0) return(-Inf)

  z_W    <- if (earned > 0) w_W * h_W / earned else 0
  theta  <- norm_penalty(tau, y, lambda, y_bar) * as.numeric(z_W >= 0.5)

  log(c) - disutility(h_H, kappa_H, eps) - disutility(h_W, kappa_W, eps) - theta
}

# Solve case (A/C): unconstrained optimum ignoring the threshold indicator.
# Returns (h_H, h_W, utility). Uses optim with L-BFGS-B (box constrained,
# handles h >= 0 without penalty functions).
solve_unconstrained <- function(w_H, w_W, y0, tau, y, norm_active,
                                kappa_H, kappa_W, eps, lambda, y_bar, h_max) {
  tau_eff <- if (norm_active) tau else 0  # above threshold uses actual tau
  obj <- function(h) {
    -hh_utility(h[1], h[2], w_H, w_W, y0, tau_eff, y,
                kappa_H, kappa_W, eps, lambda, y_bar)
  }
  # Analytical starting point from FOC approximation (ignoring y0)
  c_guess <- max(w_H + w_W, y0 + 1)
  h0 <- c(max((w_H / (kappa_H * c_guess))^eps, 1),
           max((w_W / (kappa_W * c_guess))^eps, 1))
  h0 <- pmin(h0, h_max / 2)

  fit <- tryCatch(
    optim(h0, obj, method = "L-BFGS-B",
          lower = c(0, 0), upper = c(h_max, h_max),
          control = list(factr = 1e7)),
    error = function(e) NULL
  )
  if (is.null(fit) || fit$convergence > 1) return(NULL)

  h_H <- fit$par[1]; h_W <- fit$par[2]
  list(h_H = h_H, h_W = h_W,
       util = hh_utility(h_H, h_W, w_H, w_W, y0, tau, y,
                         kappa_H, kappa_W, eps, lambda, y_bar))
}

# Solve case (B): constrained at z_W = 0.5 exactly.
# At z_W = 0.5: w_W * h_W = w_H * h_H  ->  h_W = (w_H / w_W) * h_H.
# Reduces to 1-D optimisation over h_H.
solve_at_threshold <- function(w_H, w_W, y0, tau, y,
                               kappa_H, kappa_W, eps, lambda, y_bar, h_max) {
  r <- w_H / w_W  # h_W = r * h_H at z_W = 0.5
  obj_1d <- function(h_H) {
    h_H <- max(h_H, 0)
    h_W <- r * h_H
    -hh_utility(h_H, h_W, w_H, w_W, y0, 0, y,  # tau=0: no penalty at threshold
                kappa_H, kappa_W, eps, lambda, y_bar)
  }
  h0 <- max((w_H / (kappa_H * (w_H + w_H + y0 / 10)))^eps, 1)
  h0 <- min(h0, h_max / 2)

  fit <- tryCatch(
    optim(h0, obj_1d, method = "L-BFGS-B",
          lower = 0, upper = h_max,
          control = list(factr = 1e7)),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)

  h_H <- fit$par[1]; h_W <- r * h_H
  list(h_H = h_H, h_W = h_W,
       util = hh_utility(h_H, h_W, w_H, w_W, y0, 0, y,
                         kappa_H, kappa_W, eps, lambda, y_bar))
}

# Main solver: returns optimal (h_H, h_W) and the regime chosen.
# Regime: "below" | "threshold" | "above"
solve_household <- function(w_H, w_W, y0, tau, y, params) {
  kappa_H <- params$kappa_H; kappa_W <- params$kappa_W
  eps     <- params$eps;     lambda  <- params$lambda
  y_bar   <- params$y_bar;   h_max   <- params$h_max

  # (A) unconstrained with no norm
  sol_A <- solve_unconstrained(w_H, w_W, y0, tau, y, FALSE,
                               kappa_H, kappa_W, eps, lambda, y_bar, h_max)
  # if unconstrained optimum is below threshold, norm never activates
  if (!is.null(sol_A)) {
    earned_A <- w_H * sol_A$h_H + w_W * sol_A$h_W
    z_A <- if (earned_A > 0) w_W * sol_A$h_W / earned_A else 0
    if (z_A < 0.5)
      return(c(sol_A, list(regime = "below")))
  }

  # Unconstrained optimum puts wife above 0.5 — compare bunching vs paying penalty
  # (B) constrained at z_W = 0.5
  sol_B <- solve_at_threshold(w_H, w_W, y0, tau, y,
                              kappa_H, kappa_W, eps, lambda, y_bar, h_max)
  # (C) unconstrained above threshold (pays norm penalty)
  sol_C <- solve_unconstrained(w_H, w_W, y0, tau, y, TRUE,
                               kappa_H, kappa_W, eps, lambda, y_bar, h_max)

  candidates <- list(
    list(sol = sol_B, regime = "threshold"),
    list(sol = sol_C, regime = "above"),
    list(sol = sol_A, regime = "below")
  )
  candidates <- Filter(function(x) !is.null(x$sol), candidates)
  if (length(candidates) == 0) return(NULL)

  best <- candidates[[which.max(sapply(candidates, function(x) x$sol$util))]]
  c(best$sol, list(regime = best$regime))
}

# ── 3) Load data and compute empirical moments ─────────────────────────────────

message("Loading IPUMS pairs for model calibration ...")

pairs_file <- file.path(panel_dir,
  "ipums_married_oppositesex_spouse_pairs_with_groups.csv")

load_cols <- c("YEAR", "HHWT", "fips",
               "female_income_no_transfers", "male_income_no_transfers",
               "female_weekly_hours", "male_weekly_hours",
               "female_empstat", "male_empstat",
               "hhincome_nominal", "vote_margin",
               "income_quintile_national", "income_quintile")

avail <- names(fread(pairs_file, nrows = 0))
pairs <- fread(pairs_file, select = intersect(load_cols, avail),
               showProgress = FALSE)
pairs <- pairs[YEAR %in% 2010:2020 & !is.na(vote_margin) & !is.na(hhincome_nominal)]

# Political group: Dem if vote_margin > 0.05, Rep if < -0.05
pairs[, political := fcase(
  vote_margin >  0.05, "Dem",
  vote_margin < -0.05, "Rep",
  default = NA_character_
)]
pairs <- pairs[!is.na(political)]

# Hours in weekly units (the model unit) — cap at 96 hrs/wk
pairs[, h_W := pmin(female_weekly_hours, 96)]
pairs[, h_H := pmin(male_weekly_hours,   96)]

# Wages for dual-earner households (both earning > 0, both with hours > 0)
pairs_dual <- pairs[
  female_income_no_transfers > 0 & male_income_no_transfers > 0 &
  female_weekly_hours > 0 & male_weekly_hours > 0
]
# w = annual income / weekly hours = annual $ per weekly-hour
# so budget c = w_H*h_H + w_W*h_W recovers annual household income
pairs_dual[, w_W := female_income_no_transfers / h_W]
pairs_dual[, w_H := male_income_no_transfers   / h_H]
pairs_dual[, y0  := pmax(hhincome_nominal - female_income_no_transfers
                         - male_income_no_transfers, 0)]
pairs_dual[, y   := hhincome_nominal]
pairs_dual[, tau := fifelse(political == "Rep", 1.0, 0.0)]

# Income share running variable (same as z_earned in RDD script)
pairs_dual[, z_W := female_income_no_transfers /
             (female_income_no_transfers + male_income_no_transfers)]

# Income decile (within year)
pairs_dual[, income_decile := cut(
  hhincome_nominal,
  breaks = quantile(hhincome_nominal, probs = seq(0,1,0.1), na.rm = TRUE),
  labels = 1:10, include.lowest = TRUE
), by = YEAR]
pairs_dual[, income_decile := as.integer(as.character(income_decile))]

# Set y_bar from median HH income in dual-earner sample
params$y_bar <- median(pairs_dual$hhincome_nominal, na.rm = TRUE)
message(sprintf("y_bar (median HH income, dual-earner 2010-2020): $%s",
                format(round(params$y_bar), big.mark = ",")))

# ── Empirical moments ──────────────────────────────────────────────────────────
# M1-M2 use the DUAL-EARNER sample: wife hours gap by income decile × political.
# These replace the work-share moments (which require the full married sample
# and non-employed wage imputation — deferred to a later model extension).
# M3-M6 are intensive-margin / density moments, all identified on dual-earner sample.
emp_moments <- list(
  hours_gap_Q5      =  2.20,   # M1: wife hrs Dem - Rep at income D9-D10 (hrs/wk)
  hours_gap_Q1      =  0.00,   # M2: same at D1-D3 (income-elasticity shape constraint)
  hours_gap_D10     =  1.90,   # M3: wife hrs Dem - Rep at D10 specifically
  kink_hrs_per_pp   = -25.2,   # M4: kink in wife hours at z=0.5 (hrs per pp z_W)
  bkp_ratio_Dem     =  1.423,  # M5: below/above z=0.5 density ratio, Dem
  bkp_ratio_Rep     =  1.499   # M6: same, Rep
)
message("Empirical moments loaded.")

# ── 4) Simulate model for a given parameter vector ────────────────────────────

# Draw a stratified sample by income decile (political group no longer needed
# since we solve each household under both tau=0 and tau=1 counterfactually)
set.seed(2026)
N_per_decile <- 400  # households per decile
pairs_sample <- pairs_dual[
  !is.na(income_decile) & !is.na(w_W) & !is.na(w_H),
  .SD[sample(.N, min(.N, N_per_decile))],
  by = income_decile
]
message(sprintf("Simulation sample: %d households across %d income deciles",
                nrow(pairs_sample), uniqueN(pairs_sample$income_decile)))

# Simulate using within-household counterfactuals.
# For each household solve tau=0 (no norm) and tau=1 (full norm) separately.
# norm_effect = h_W(tau=0) - h_W(tau=1) isolates the causal norm effect,
# removing the compositional confound from Dem/Rep counties having different
# underlying wage distributions.
simulate_model <- function(p, sample_dt) {
  params_run         <- params
  params_run$lambda  <- p["lambda"]
  params_run$kappa_W <- p["kappa_W"]

  n           <- nrow(sample_dt)
  h_W_tau0    <- numeric(n)
  h_W_tau1    <- numeric(n)
  h_H_tau0    <- numeric(n)
  regime_tau1 <- character(n)

  for (i in seq_len(n)) {
    row <- sample_dt[i]
    s0  <- solve_household(row$w_H, row$w_W, row$y0, 0.0, row$y, params_run)
    s1  <- solve_household(row$w_H, row$w_W, row$y0, 1.0, row$y, params_run)
    h_W_tau0[i]    <- if (!is.null(s0)) s0$h_W else NA_real_
    h_W_tau1[i]    <- if (!is.null(s1)) s1$h_W else NA_real_
    h_H_tau0[i]    <- if (!is.null(s0)) s0$h_H else NA_real_
    regime_tau1[i] <- if (!is.null(s1)) s1$regime else "failed"
  }

  result <- copy(sample_dt)
  result[, `:=`(
    h_W_tau0    = h_W_tau0,
    h_W_tau1    = h_W_tau1,
    h_H_tau0    = h_H_tau0,
    norm_effect = h_W_tau0 - h_W_tau1,
    regime      = regime_tau1,
    w_hrs_W_mod = h_W_tau0   # tau=0 baseline for level plots
  )]
  result[, z_W_mod := fifelse(
    w_H * h_H_tau0 + w_W * h_W_tau0 > 0,
    w_W * h_W_tau0 / (w_H * h_H_tau0 + w_W * h_W_tau0),
    0
  )]
  result
}

# ── 5) Compute model moments from simulated data ───────────────────────────────

compute_model_moments <- function(sim_dt) {
  # All gap moments use within-household norm_effect = h_W(tau=0) - h_W(tau=1)
  # This is the causal norm effect, free of compositional differences

  # M1: mean norm effect at top income (D9-D10)
  gap_Q5 <- sim_dt[income_decile %in% 9:10 & !is.na(norm_effect),
    weighted.mean(norm_effect, HHWT, na.rm = TRUE)]
  if (length(gap_Q5) == 0) gap_Q5 <- NA_real_

  # M2: mean norm effect at bottom income (D1-D3) — should be ~0
  gap_Q1 <- sim_dt[income_decile %in% 1:3 & !is.na(norm_effect),
    weighted.mean(norm_effect, HHWT, na.rm = TRUE)]
  if (length(gap_Q1) == 0) gap_Q1 <- NA_real_

  # M3: norm effect at D10 specifically
  hrs_gap_D10 <- sim_dt[income_decile == 10 & !is.na(norm_effect),
    weighted.mean(norm_effect, HHWT, na.rm = TRUE)]
  if (length(hrs_gap_D10) == 0) hrs_gap_D10 <- NA_real_

  # M4: kink at z_W = 0.5 — slope of wife hours vs z_W, below vs above threshold
  rdd_dt <- sim_dt[abs(z_W_mod - 0.5) > 0.02 & abs(z_W_mod - 0.5) <= 0.20 &
                   !is.na(z_W_mod) & !is.na(w_hrs_W_mod)]
  if (nrow(rdd_dt) < 20) {
    kink_mod <- NA_real_
  } else {
    rdd_dt[, `:=`(z_c = z_W_mod - 0.5, D = as.integer(z_W_mod >= 0.5),
                  D_zc = as.integer(z_W_mod >= 0.5) * (z_W_mod - 0.5))]
    fit_mod   <- lm(w_hrs_W_mod ~ z_c + D + D_zc, data = rdd_dt, weights = HHWT)
    kink_mod  <- coef(fit_mod)["D_zc"]
  }

  # M5 & M6: BKP avoidance ratio (density below / above z=0.5, donut excluded)
  bkp_ratio <- function(grp) {
    sub <- sim_dt[political == grp & abs(z_W_mod - 0.5) > 0.02]
    tot <- sum(sub$HHWT, na.rm = TRUE)
    if (tot == 0) return(NA_real_)
    below <- sum(sub[z_W_mod >= 0.40 & z_W_mod < 0.48, HHWT], na.rm = TRUE) / tot
    above <- sum(sub[z_W_mod >  0.52 & z_W_mod <= 0.60, HHWT], na.rm = TRUE) / tot
    if (above == 0) NA_real_ else below / above
  }

  list(
    hours_gap_Q5    = gap_Q5,
    hours_gap_Q1    = gap_Q1,
    hours_gap_D10   = hrs_gap_D10,
    kink_hrs_per_pp = if (is.na(kink_mod)) NA_real_ else kink_mod * 100,
    bkp_ratio_Dem   = bkp_ratio("Dem"),
    bkp_ratio_Rep   = bkp_ratio("Rep")
  )
}

# ── 6) SMM calibration ─────────────────────────────────────────────────────────
# Two-step approach following the SMM pattern in assgt4_smm.R:
#
# Step 1: Pin kappa_W analytically from the average wife hours FOC.
#   FOC (eps=1, no norm): w_W/c = kappa_W * h_W
#   At D5-D7 (norm barely active): h_W_avg ≈ data mean, w_W and c from data.
#   kappa_W = mean(w_W / (c * h_W)) across D5-D7 Dem households.
#
# Step 2: Grid search over lambda (1-D SMM). Produces a visible moment curve
#   (same structure as the gamma estimation in assgt4_smm.R).
#   Primary target: hours_gap_D10 = 1.90 hrs/wk (most directly identified by lambda).

message("\nStep 1: Set kappa_W ...")
# Set kappa_W = kappa_H (symmetric hours preferences). This is the correct
# first-pass assumption: wage differences alone drive h_W < h_H and z_W < 0.5
# for most couples (since w_W < w_H on average in the data).
# Pinning kappa_W from the FOC on the dual-earner sample produces a downward-biased
# kappa_W (κ_W < κ_H), which incorrectly makes wives want to work more hours than
# husbands, creating spurious norm-activation at low incomes.
params$kappa_W <- params$kappa_H
message(sprintf("  kappa_W = kappa_H = %.6f (symmetric preferences)", params$kappa_W))

# Verification: with symmetric kappa, z_W* = w_W^2 / (w_H^2 + w_W^2)
# Norm fires when z_W* > 0.5, i.e. w_W > w_H (wife has higher hourly wage)
pct_zW_above_half <- pairs_dual[, mean(w_W > w_H, na.rm = TRUE)]
message(sprintf("  Share of dual-earner couples with w_W > w_H: %.1f%%",
                100 * pct_zW_above_half))

message("\nStep 2: Grid search over lambda ...")
message("(Running on stratified sample of ", nrow(pairs_sample), " households)")

# Recompute hours_gap_D10 target on the DUAL-EARNER sample (consistent with simulation)
hrs_d10_data <- pairs_dual[income_decile == 10 & political %in% c("Dem","Rep"),
  .(mean_hrs = weighted.mean(h_W, HHWT, na.rm = TRUE)), by = political]
if (all(c("Dem","Rep") %in% hrs_d10_data$political)) {
  emp_moments$hours_gap_D10 <- hrs_d10_data[political=="Dem", mean_hrs] -
                                hrs_d10_data[political=="Rep", mean_hrs]
  message(sprintf("  Recomputed hours_gap_D10 on dual-earner sample: %.3f hrs/wk",
                  emp_moments$hours_gap_D10))
}

lambda_grid <- seq(0.001, 0.08, by = 0.002)  # extended lower to find target crossing

run_moment_given_lambda <- function(lam, sample_dt, par_base) {
  p <- c(lambda = lam, kappa_W = par_base$kappa_W)
  sim <- tryCatch(simulate_model(p, sample_dt), error = function(e) NULL)
  if (is.null(sim)) return(NA_real_)
  m <- compute_model_moments(sim)
  m$hours_gap_D10  # primary target moment
}

set.seed(2026)
moment_curve <- sapply(lambda_grid, function(lam) {
  m <- run_moment_given_lambda(lam, pairs_sample, params)
  cat(sprintf("  lambda=%.3f  hours_gap_D10=%.3f\n", lam, m))
  m
})

# lambda_hat: closest to target
target_D10 <- emp_moments$hours_gap_D10
deviations <- (moment_curve - target_D10)^2
lambda_hat <- lambda_grid[which.min(deviations)]
params$lambda <- lambda_hat

message(sprintf("\nCalibrated parameters:"))
message(sprintf("  lambda  = %.4f  (norm penalty scale)", params$lambda))
message(sprintf("  kappa_W = %.6f  (wife hours disutility scale)", params$kappa_W))
message(sprintf("  Model hours_gap_D10 at lambda_hat: %.3f  (target: %.3f)",
                moment_curve[which.min(deviations)], target_D10))

# Plot moment curve (rho(lambda) equivalent from assgt4_smm.R)
png(file.path(graphs_dir(), paste0(today, "_model_smm_moment_curve.png")),
    width = 1400, height = 900, res = 150)
plot(lambda_grid, moment_curve, type = "l", lwd = 2, col = "steelblue4",
     xlab = expression(lambda ~ "(norm penalty scale)"),
     ylab = "Model: wife hours gap at D10 (Dem - Rep, hrs/wk)",
     main = expression("SMM moment curve: hours_gap_D10(" * lambda * ")"))
abline(h = target_D10, col = "firebrick", lty = 2, lwd = 1.5)
abline(v = lambda_hat,  col = "grey40",    lty = 3, lwd = 1.5)
points(lambda_hat, moment_curve[which.min(deviations)],
       pch = 19, col = "firebrick", cex = 1.5)
legend("topleft",
       legend = c("Model moment curve", sprintf("Target = %.2f hrs/wk", target_D10),
                  sprintf("lambda_hat = %.3f", lambda_hat)),
       col = c("steelblue4","firebrick","grey40"),
       lwd = c(2, 1.5, 1.5), lty = c(1, 2, 3), bty = "n")
grid()
dev.off()
message(sprintf("  Saved: %s_model_smm_moment_curve.png", today))

# Final simulation at calibrated parameters
moment_names  <- names(emp_moments)
p_cal <- c(lambda = params$lambda, kappa_W = params$kappa_W)
sim_final <- simulate_model(p_cal, pairs_sample)
mod_moments_final <- compute_model_moments(sim_final)

message("\nMoment fit at calibrated parameters:")
moment_table <- data.table(
  moment    = moment_names,
  data      = unlist(emp_moments[moment_names]),
  model     = unlist(mod_moments_final[moment_names])
)
moment_table[, pct_error := round(100 * (model - data) / (abs(data) + 1e-6), 1)]
print(moment_table)
fwrite(moment_table,
       file.path(results_dir, paste0(today, "_model_moment_fit.csv")))

# ── 7) Comparison plots ────────────────────────────────────────────────────────

message("\nGenerating comparison plots ...")

pol_colors <- c("Dem" = "#4575b4", "Rep" = "#d73027")

# 7a) Wife hours by income decile: model vs data
hrs_decile_mod <- sim_final[!is.na(income_decile) & !is.na(w_hrs_W_mod),
  .(hrs_W_model = weighted.mean(w_hrs_W_mod, HHWT, na.rm = TRUE)),
  by = .(political, income_decile)
][, source := "Model"]

hrs_decile_dat <- pairs_dual[!is.na(income_decile),
  .(hrs_W_model = weighted.mean(female_weekly_hours, HHWT, na.rm = TRUE)),
  by = .(political, income_decile)
][, source := "Data"]

hrs_compare <- rbind(hrs_decile_mod, hrs_decile_dat)

p7a <- ggplot(hrs_compare[!is.na(income_decile)],
              aes(x = income_decile, y = hrs_W_model,
                  color = political, linetype = source,
                  group = interaction(political, source))) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.0) +
  scale_x_continuous(breaks = 1:10, labels = paste0("D", 1:10)) +
  scale_color_manual(values = pol_colors, name = NULL) +
  scale_linetype_manual(values = c("Data" = "solid", "Model" = "dashed"), name = NULL) +
  labs(
    title    = "Wife's weekly hours by income decile: model vs data",
    subtitle = sprintf("lambda=%.3f, kappa_W=%.3f, eps=%.1f",
                       params$lambda, params$kappa_W, params$eps),
    x = "Household income decile", y = "Mean weekly hours (wife)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
save_plot(paste0(today, "_model_vs_data_wife_hours_decile.png"),
          { print(p7a) }, width = 1800, height = 1200)

# 7b) z_W density: model vs data (Dem and Rep)
donut_w <- 0.02
z_bins_mod <- sim_final[abs(z_W_mod - 0.5) > donut_w & !is.na(z_W_mod),
  .(wt = sum(HHWT)), by = .(political, z_bin = round(z_W_mod, 2))
][, share := wt / sum(wt), by = political][, source := "Model"]

z_bins_dat <- pairs_dual[abs(z_W - 0.5) > donut_w & !is.na(z_W),
  .(wt = sum(HHWT)), by = .(political, z_bin = round(z_W, 2))
][, share := wt / sum(wt), by = political][, source := "Data"]

z_compare <- rbind(z_bins_mod, z_bins_dat)

p7b <- ggplot(z_compare[z_bin > 0.1 & z_bin < 0.9],
              aes(x = z_bin, y = share * 100,
                  fill = political, alpha = source)) +
  geom_col(position = "identity", width = 0.009) +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  annotate("rect", xmin = 0.5 - donut_w, xmax = 0.5 + donut_w,
           ymin = -Inf, ymax = Inf, fill = "grey40", alpha = 0.12) +
  scale_fill_manual(values = pol_colors, name = NULL) +
  scale_alpha_manual(values = c("Data" = 0.7, "Model" = 0.4), name = NULL) +
  facet_wrap(~political, ncol = 1) +
  labs(
    title    = "Wife's income share density: model vs data",
    subtitle = "Donut ±2pp excluded; model bunching at z=0.5 driven by norm penalty",
    x = "Wife's share of couple earned income (z_W)",
    y = "Share of observations (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
save_plot(paste0(today, "_model_vs_data_z_density.png"),
          { print(p7b) }, width = 1800, height = 1400)

# 7c) Regime distribution by income decile
regime_summary <- sim_final[!is.na(income_decile) & regime != "failed",
  .(n = .N, wt = sum(HHWT)),
  by = .(political, income_decile, regime)
][, share := wt / sum(wt), by = .(political, income_decile)]

p7c <- ggplot(regime_summary,
              aes(x = income_decile, y = share, fill = regime)) +
  geom_col(position = "stack") +
  facet_wrap(~political) +
  scale_x_continuous(breaks = 1:10, labels = paste0("D", 1:10)) +
  scale_fill_manual(
    values = c("below" = "#4575b4", "threshold" = "#fee090", "above" = "#d73027"),
    name = "Norm regime"
  ) +
  labs(
    title    = "Model regime allocation by income decile",
    subtitle = "Threshold bunching (yellow) increases with income — the income-elastic norm mechanism",
    x = "Household income decile", y = "Share of households"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())
save_plot(paste0(today, "_model_regime_by_decile.png"),
          { print(p7c) }, width = 2000, height = 1200)

message("\nDone. Outputs in data/graphs/ and data/processed/results/.")
message(sprintf("  Calibrated lambda  = %.4f", params$lambda))
message(sprintf("  Calibrated kappa_W = %.4f", params$kappa_W))
message(sprintf("  %s_model_moment_fit.csv", today))
message(sprintf("  %s_model_vs_data_wife_hours_decile.png", today))
message(sprintf("  %s_model_vs_data_z_density.png", today))
message(sprintf("  %s_model_regime_by_decile.png", today))
