# =============================================================================
# T3 — compute the norm WEDGE tau properly from consumption
#
# tau_j = alpha / u'(C_j) = alpha * C_j under log utility. It is the object that
# is comparable across years: alpha alone falls 89% from 1980-2024, but most of
# that is nominal income quadrupling, not the norm weakening.
#
# Because tau is LINEAR in C, the mean wedge is exactly alpha * mean(C) -- no
# Jensen correction needed. So this requires no re-estimation, only ONE solve
# per year at the already-fitted parameters.
#
# THREE measures, because they answer different questions:
#   tau_data    alpha * Cbar using OBSERVED hours (what kappa was calibrated on)
#   tau_model   alpha * Cbar at the model's own solution (internally consistent)
#   tau_binding alpha * Cbar among households where the norm actually BINDS
#               (regimes II and III). Averaging over households the norm never
#               touches dilutes it; this is the wedge actually being applied.
# =============================================================================
source(here::here("_setup.R"))

# Inputs : data/processed/results/*_t3_estimates_v2_by_year.csv
#          data/processed/panel/model_input_households.csv
# Output : data/processed/results/YYYY-MM-DD_t3_tau_series.csv
suppressMessages({library(data.table); source(here::here("t3", "t3-model-solver.R"))})

panel_dir   <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")

est <- read_newest(results_dir, "t3_estimates_v2_by_year.csv$")
dat <- fread(file.path(panel_dir, "model_input_households.csv"), showProgress = FALSE)
dat <- dat[is.finite(f_w) & is.finite(m_w) & f_w > 0 & m_w > 0 & is.finite(y0) &
           is.finite(f_h) & is.finite(m_h)]

out <- rbindlist(lapply(seq_len(nrow(est)), function(i) {
  yr <- est$YEAR[i]; a <- est$alpha[i]; F <- est$F_dollars[i]
  d  <- dat[YEAR == yr]
  int  <- d[f_h > 0 & m_h > 0]
  Cbar_data <- weighted.mean(int$m_w*int$m_h + int$f_w*int$f_h + int$y0, int$HHWT)
  kap  <- weighted.mean(int$m_w, int$HHWT) / (Cbar_data * weighted.mean(int$m_h, int$HHWT))
  k    <- rep(kap, nrow(d))
  s    <- solve_household(d$m_w, d$f_w, d$y0, F, a, k, k)
  Cbar_model <- weighted.mean(s$C, d$HHWT)
  bind <- s$regime %in% c(2L, 3L)           # norm binding: interior-binding or kink
  Cbar_bind  <- if (any(bind)) weighted.mean(s$C[bind], d$HHWT[bind]) else NA_real_
  data.table(YEAR = yr, alpha = a,
             Cbar_data = Cbar_data, Cbar_model = Cbar_model, Cbar_binding = Cbar_bind,
             pct_binding = 100 * weighted.mean(bind, d$HHWT),
             tau_data = a * Cbar_data, tau_model = a * Cbar_model,
             tau_binding = a * Cbar_bind,
             tau_approx_ymed = a * median(d$y, na.rm = TRUE))
}))

cat("=== the norm wedge tau, computed properly ===\n\n")
print(out[, .(YEAR, alpha = signif(alpha,3),
              Cbar_model = round(Cbar_model),
              tau_model = round(tau_model,3),
              tau_binding = round(tau_binding,3),
              pct_bind = round(pct_binding,1),
              tau_approx = round(tau_approx_ymed,3))], nrows = 30)

first <- out[1]; last <- out[.N]
cat("\n=== 1980 -> 2024 ===\n")
for (v in c("alpha","tau_model","tau_binding","tau_approx_ymed")) {
  cat(sprintf("  %-16s %10.4g -> %-10.4g  %+6.0f%%\n", v,
      first[[v]], last[[v]], 100*(last[[v]]/first[[v]] - 1)))
}
cat("\n  (tau_approx_ymed is the median-income approximation reported earlier;\n")
cat("   compare it with tau_model to see how far off the shortcut was.)\n")
fwrite(out, dated_path(results_dir, "t3_tau_series.csv"))
message("\nwrote t3_tau_series.csv")
