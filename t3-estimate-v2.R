# =============================================================================
# T3 — estimation of (alpha, f) year by year
#
# THE MODEL. One norm parameter, as in the slides: the identity penalty enters
# through the wedge tau = alpha / u'(C), which under log utility is tau =
# alpha*C. It acts as a proportional subsidy on his hours and an equal
# proportional tax on hers. alpha is therefore an HOURS parameter -- it governs
# how much someone works, and its empirical signature is bunching at equal
# earnings (the cliff).
#
# WHY THERE IS NO SECOND NORM PARAMETER. An earlier draft added alpha2, a
# penalty on her participation, to make T3 speak to T2's extensive-margin
# result. That is not in the model and has been removed. It is also provably
# unnecessary for the cliff: a relative-earnings norm cannot move participation
# at all (the corner share is invariant to alpha at every value tested, because
# V = 0 at the KINK as well as at the corner, and bunching preserves her
# earnings while withdrawing does not).
#
# DIVISION OF LABOUR IN THE MODEL
#   alpha -> hours. The cliff. The intensive margin. This is the norm.
#   F     -> the corner. F is a TECHNOLOGY, the goods cost of replacing home
#            production (childcare, cooking) once a spouse enters the market.
#            It carries NO identity interpretation and is not a preference.
# T3 explains the cliff and hours. T2's participation finding is an empirical
# result the theory does not claim to generate, and should not be made to.
#
# F IS ESTIMATED, NOT ASSUMED. F = f * median(y_t), a share of that year's
# median household income -- so it needs no external calibration and deflates
# itself across a 44-year sample. An earlier run fixed f = 0.10 arbitrarily.
#
# MOMENTS (5) vs PARAMETERS (2) -- OVER-identified, so the fit is testable:
#   cliff ratio                              -> alpha
#   corner share overall                     -> f
#   corner share by husband's-wage quintile  -> tests F's FUNCTIONAL FORM. A
#     goods cost has utility burden F/C, which falls with resources, so it
#     implies a specific corner gradient. If the data's gradient is flatter or
#     steeper than a goods cost can produce, the form is wrong.
# Untargeted, held back as the out-of-sample test:
#   wife's share of couple hours, share of couples where she out-earns him
#
# Each YEAR is estimated separately: repeated cross-sections, each year a
# different t. Continuous optimiser, not a grid -- an earlier grid version had
# alpha alternating between two adjacent grid points across eleven years, so
# apparent time variation was quantisation rather than signal.
# =============================================================================

library(data.table)
source("t3-model-solver.R")
source("functions.R")
source("R/paths.R")

panel_dir   <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
ensure_dir(results_dir)
donut    <- 0.02
years_do <- as.integer(strsplit(Sys.getenv("T3_YEARS", "2019"), ",")[[1]])
# Simulate on a random subsample; compute DATA moments on the full year. The
# decennial files are 1.8-2.4M households and Nelder-Mead evaluates ~200 times,
# which is ~14 hours for the series. Subsampling the SIMULATION only is standard
# SMM practice -- it introduces Monte Carlo error, negligible at this size, and
# leaves the data moments exact.
n_sim_max <- as.integer(Sys.getenv("T3_NSIM", "200000"))
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
# qgrp: husband's-wage quintile, fixed on the DATA so model and data are
# compared within the same cells.
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

  ymed <- median(d$y, na.rm = TRUE)
  qgrp <- as.integer(cut(d$m_w, quantile(d$m_w, seq(0, 1, .2), na.rm = TRUE),
                         labels = 1:5, include.lowest = TRUE))
  # kappa SYMMETRIC across spouses -- gender asymmetry must come from the wage
  # gap and the norm, never from assumed preferences, or alpha is unidentified.
  int  <- d[f_h > 0 & m_h > 0]
  Cbar <- weighted.mean(int$m_w*int$m_h + int$f_w*int$f_h + int$y0, int$HHWT)
  kap  <- weighted.mean(int$m_w, int$HHWT) / (Cbar * weighted.mean(int$m_h, int$HHWT))
  k_m  <- rep(kap, nrow(d)); k_f <- rep(kap, nrow(d))

  md <- moments(d$m_h, d$f_h, d$m_w, d$f_w, d$HHWT, qgrp)
  message(sprintf("  DATA  cliff %.3f corner %.3f (Q1 %.3f Q3 %.3f Q5 %.3f) hshare %.3f outearn %.3f",
                  md["cliff"], md["corner"], md["cornerQ1"], md["cornerQ3"],
                  md["cornerQ5"], md["hshare"], md["outearn"]))

  # Fixed simulation subsample, drawn once so the objective is not stochastic
  # across optimiser iterations.
  si   <- if (nrow(d) > n_sim_max) sort(sample.int(nrow(d), n_sim_max)) else seq_len(nrow(d))
  dS   <- d[si]; qS <- qgrp[si]
  kS_m <- k_m[si]; kS_f <- k_f[si]
  sim <- function(par) {
    a1 <- exp(par[1]); f <- plogis(par[2]) * 0.5
    s  <- solve_household(dS$m_w, dS$f_w, dS$y0, f * ymed, a1, kS_m, kS_f, 0, 0)
    moments(s$h_m, s$h_f, dS$m_w, dS$f_w, dS$HHWT, qS)
  }
  # Percentage deviations so moments on different scales are comparable.
  loss <- function(par) {
    ms <- sim(par)
    if (any(!is.finite(ms[TARGETS]))) return(1e6)
    sum(((ms[TARGETS] - md[TARGETS]) / pmax(abs(md[TARGETS]), 1e-6))^2)
  }
  st  <- c(log(1e-6), qlogis(0.10 / 0.5))
  fit <- optim(st, loss, method = "Nelder-Mead",
               control = list(maxit = 400, reltol = 1e-8))
  a1 <- exp(fit$par[1]); f <- plogis(fit$par[2]) * 0.5
  ms <- sim(fit$par)

  message(sprintf("  FIT   alpha %.4e | f %.4f (F = $%s) | loss %.5f",
                  a1, f, format(round(f*ymed), big.mark=","), fit$value))
  message(sprintf("  MODEL cliff %.3f corner %.3f (Q1 %.3f Q3 %.3f Q5 %.3f) hshare %.3f outearn %.3f",
                  ms["cliff"], ms["corner"], ms["cornerQ1"], ms["cornerQ3"],
                  ms["cornerQ5"], ms["hshare"], ms["outearn"]))
  message("        hshare & outearn are UNTARGETED -- the out-of-sample test.")

  data.table(YEAR = yr, n = nrow(d), kappa = kap, y_median = ymed,
             alpha = a1, f = f, F_dollars = f * ymed,
             loss = fit$value, converged = fit$convergence == 0,
             as.data.table(as.list(md))[, paste0("data_", names(md)) := as.list(md)][, .SD, .SDcols = patterns("^data_")],
             as.data.table(as.list(ms))[, paste0("model_", names(ms)) := as.list(ms)][, .SD, .SDcols = patterns("^model_")])
}))

if (nrow(out)) {
  print(out[, .(YEAR, alpha, f, loss, converged,
                data_cliff, model_cliff, data_corner, model_corner,
                data_hshare, model_hshare)])
  fwrite(out, dated_path(results_dir, "t3_estimates_v2_by_year.csv"))
  message("\nwrote t3_estimates_v2_by_year.csv")
}
