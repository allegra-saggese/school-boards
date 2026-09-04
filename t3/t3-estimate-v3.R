# =============================================================================
# T3 — estimation with preference heterogeneity: (alpha, f, sigma_kappa)
#
# Input  : data/processed/panel/model_input_households.csv
# Outputs: data/processed/results/YYYY-MM-DD_t3_estimates_v3_by_year.csv
#
# THE PROBLEM. With a single kappa for every household, participation is a
# DETERMINISTIC function of (w_m, w_f, y0). Log utility then implies a strong
# income effect and a steeply rising corner gradient across the husband's wage
# distribution, where the data are flat and mildly U-shaped. 2003, for example:
#       data              Q1 0.262  Q3 0.227  Q5 0.301
#       homogeneous kappa Q1 0.132  Q3 0.225  Q5 0.420
# The gradient breaks worst at the BOTTOM: the fixed cost F was meant to push
# low-income wives out of the market and is not strong enough to.
#
# THE FIX. Draw kappa from a distribution:
#       kappa_i = kappa_bar * exp(sigma*e_i - sigma^2/2),   e_i ~ N(0,1)
# The -sigma^2/2 term holds E[kappa] = kappa_bar, adding dispersion without
# shifting the mean. This flattens the gradient from both ends at once:
# high-kappa draws put corners in at the bottom, low-kappa draws take them out
# at the top.
#
# ONE DISTRIBUTION FOR BOTH SPOUSES — same kappa_bar, same sigma. Letting women
# draw a higher kappa would assume a stronger female taste for home production,
# which is the thing the norm is meant to explain, and would leave alpha
# unidentified. All gender asymmetry must come from the wage gap and alpha.
#
# COMMON RANDOM NUMBERS: the kappa draws are made ONCE and held fixed across
# optimiser iterations, or the objective is stochastic and Nelder-Mead chases
# simulation noise instead of the parameters.
#
# 3 PARAMETERS, 5 MOMENTS — still over-identified.
# =============================================================================

library(data.table)
source(here::here("_setup.R"))
source(here::here("t3", "t3-model-solver.R"))

panel_dir   <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
ensure_dir(results_dir)
donut    <- 0.02
n_est    <- as.integer(Sys.getenv("T3_NEST", "60000"))   # households per year
R_draw   <- as.integer(Sys.getenv("T3_RDRAW", "8"))      # kappa draws each
years_do <- as.integer(strsplit(Sys.getenv("T3_YEARS", "2019"), ",")[[1]])
set.seed(20260830)

dat <- fread(file.path(panel_dir, "model_input_households.csv"), showProgress = FALSE)
dat <- dat[is.finite(f_w) & is.finite(m_w) & f_w > 0 & m_w > 0 & is.finite(y0) &
           is.finite(f_h) & is.finite(m_h)]

cliff_ratio <- function(z, wt) {
  below <- sum(wt[z >= 0.40 & z <  0.5 - donut], na.rm = TRUE)
  above <- sum(wt[z >  0.5 + donut & z <= 0.60], na.rm = TRUE)
  if (!is.finite(above) || above <= 0) return(NA_real_)
  below / above
}
moments <- function(h_m, h_f, w_m, w_f, wt, qgrp) {
  e_m <- w_m * h_m; e_f <- w_f * h_f
  z   <- fifelse(e_m + e_f > 0, e_f / (e_m + e_f), NA_real_)
  cs  <- function(i) sum(wt[i][h_f[i] <= 0]) / sum(wt[i])
  c(cliff   = cliff_ratio(z, wt),
    corner  = sum(wt[h_f <= 0]) / sum(wt),
    cornerQ1= cs(qgrp == 1L), cornerQ3 = cs(qgrp == 3L), cornerQ5 = cs(qgrp == 5L),
    hshare  = sum(wt * h_f) / sum(wt * (h_f + h_m)),
    outearn = sum(wt[!is.na(z) & z > 0.5]) / sum(wt[!is.na(z)]))
}
TARGETS <- c("cliff", "corner", "cornerQ1", "cornerQ3", "cornerQ5")

out <- rbindlist(lapply(years_do, function(yr) {
  d <- dat[YEAR == yr]
  if (nrow(d) < 5000L) return(NULL)
  message("\n=== ", yr, "  (n = ", format(nrow(d), big.mark = ","), ") ===")

  ymed  <- median(d$y, na.rm = TRUE)
  qbrk  <- quantile(d$m_w, seq(0, 1, .2), na.rm = TRUE)
  qgrpD <- as.integer(cut(d$m_w, qbrk, labels = 1:5, include.lowest = TRUE))
  int   <- d[f_h > 0 & m_h > 0]
  Cbar  <- weighted.mean(int$m_w*int$m_h + int$f_w*int$f_h + int$y0, int$HHWT)
  kbar  <- weighted.mean(int$m_w, int$HHWT) / (Cbar * weighted.mean(int$m_h, int$HHWT))

  # DATA moments on the FULL year; only the SIMULATION is subsampled.
  md <- moments(d$m_h, d$f_h, d$m_w, d$f_w, d$HHWT, qgrpD)
  message(sprintf("  DATA  cliff %.3f corner %.3f (Q1 %.3f Q3 %.3f Q5 %.3f) hshare %.3f outearn %.3f",
                  md["cliff"], md["corner"], md["cornerQ1"], md["cornerQ3"],
                  md["cornerQ5"], md["hshare"], md["outearn"]))

  ss <- d[sample(.N, min(n_est, .N))]
  S  <- ss[rep(seq_len(nrow(ss)), each = R_draw)]         # R draws per household
  S[, wt := HHWT / R_draw]
  S[, qg := as.integer(cut(m_w, qbrk, labels = 1:5, include.lowest = TRUE))]
  # Common random numbers: drawn ONCE, reused at every parameter guess.
  em <- rnorm(nrow(S)); ef <- rnorm(nrow(S))

  sim <- function(par) {
    a <- exp(par[1]); f <- plogis(par[2]) * 0.5; sg <- exp(par[3])
    k_m <- kbar * exp(sg * em - sg^2/2)
    k_f <- kbar * exp(sg * ef - sg^2/2)
    s <- solve_household(S$m_w, S$f_w, S$y0, f * ymed, a, k_m, k_f, 0, 0)
    moments(s$h_m, s$h_f, S$m_w, S$f_w, S$wt, S$qg)
  }
  loss <- function(par) {
    ms <- sim(par)
    if (any(!is.finite(ms[TARGETS]))) return(1e6)
    sum(((ms[TARGETS] - md[TARGETS]) / pmax(abs(md[TARGETS]), 1e-6))^2)
  }
  st  <- c(log(3e-6), qlogis(0.10/0.5), log(0.6))
  fit <- optim(st, loss, method = "Nelder-Mead", control = list(maxit = 500, reltol = 1e-8))
  a <- exp(fit$par[1]); f <- plogis(fit$par[2])*0.5; sg <- exp(fit$par[3])
  ms <- sim(fit$par)

  message(sprintf("  FIT   alpha %.4e | f %.4f | sigma_kappa %.3f | loss %.5f",
                  a, f, sg, fit$value))
  message(sprintf("  MODEL cliff %.3f corner %.3f (Q1 %.3f Q3 %.3f Q5 %.3f) hshare %.3f outearn %.3f",
                  ms["cliff"], ms["corner"], ms["cornerQ1"], ms["cornerQ3"],
                  ms["cornerQ5"], ms["hshare"], ms["outearn"]))

  data.table(YEAR = yr, n = nrow(d), n_sim = nrow(S), kappa_bar = kbar,
             Cbar = Cbar, y_median = ymed,
             alpha = a, f = f, sigma_kappa = sg,
             tau_bar = a * Cbar,          # the WEDGE -- comparable across years
             loss = fit$value, converged = fit$convergence == 0,
             data_cliff = md["cliff"],   model_cliff = ms["cliff"],
             data_corner= md["corner"],  model_corner= ms["corner"],
             data_Q1 = md["cornerQ1"],   model_Q1 = ms["cornerQ1"],
             data_Q5 = md["cornerQ5"],   model_Q5 = ms["cornerQ5"],
             data_hshare= md["hshare"],  model_hshare= ms["hshare"],
             data_outearn=md["outearn"], model_outearn=ms["outearn"])
}))

if (nrow(out)) {
  print(out[, .(YEAR, alpha, tau_bar, f, sigma_kappa, loss,
                data_Q1, model_Q1, data_Q5, model_Q5)])
  fwrite(out, dated_path(results_dir, "t3_estimates_v3_by_year.csv"))
  message("\nwrote t3_estimates_v3_by_year.csv")
}
