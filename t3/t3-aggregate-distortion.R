# =============================================================================
# T3 — the AGGREGATE DISTORTION: hours lost to the norm, by year
#
# The counterfactual: solve each year at the fitted (alpha_hat, f_hat), then
# re-solve with alpha = 0 holding F and kappa FIXED. Only the norm is switched
# off, so the difference is the norm's own effect and not a reparameterisation.
#
#   hours lost (her)   = h_f(alpha = 0) - h_f(alpha_hat)      >= 0
#   hours gained (him) = h_m(alpha_hat) - h_m(alpha = 0)      >= 0
#
# Both are expected: the wedge tau is a proportional TAX on her earnings and an
# equal proportional SUBSIDY on his, so the norm reallocates market work from
# her to him. Reporting only her loss overstates the net effect on household
# labour supply, so his gain is reported alongside it.
#
# The reported quantity is hours, integrated over the whole distribution rather
# than approximated at the mean. HHWT scales the sample to the population, so
# the totals are national.
#
# NOTE the distortion is purely INTENSIVE. The corner share is invariant to
# alpha (established separately), so switching the norm off moves nobody into
# or out of the labour force -- it only changes hours among those already
# working. That is the same hours-only property, showing up in the aggregate.
# =============================================================================
source(here::here("_setup.R"))

# Inputs : data/processed/results/*_t3_estimates_v2_by_year.csv
#          data/processed/panel/model_input_households.csv
# Output : data/processed/results/YYYY-MM-DD_t3_aggregate_distortion.csv
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
  Cbar <- weighted.mean(int$m_w*int$m_h + int$f_w*int$f_h + int$y0, int$HHWT)
  kap  <- weighted.mean(int$m_w, int$HHWT) / (Cbar * weighted.mean(int$m_h, int$HHWT))
  k    <- rep(kap, nrow(d))

  s1 <- solve_household(d$m_w, d$f_w, d$y0, F, a, k, k)   # with the norm
  s0 <- solve_household(d$m_w, d$f_w, d$y0, F, 0, k, k)   # norm switched off
  w  <- d$HHWT

  lost_f <- s0$h_f - s1$h_f            # her hours the norm removes
  gain_m <- s1$h_m - s0$h_m            # his hours the norm adds
  bind   <- s1$regime %in% c(2L, 3L)

  tot_f0 <- sum(w * s0$h_f)            # total female hours absent the norm
  data.table(
    YEAR = yr, alpha = a,
    pct_binding      = 100 * weighted.mean(bind, w),
    # national totals (HHWT scales to the population)
    hours_lost_total = sum(w * lost_f),
    hours_gain_total = sum(w * gain_m),
    hours_net_total  = sum(w * (gain_m - lost_f)),
    # intensity measures
    pct_female_hours_lost = 100 * sum(w * lost_f) / tot_f0,
    lost_per_hh           = weighted.mean(lost_f, w),
    lost_per_affected     = if (any(bind)) weighted.mean(lost_f[bind], w[bind]) else NA_real_,
    fte_lost_millions     = sum(w * lost_f) / 2000 / 1e6
  )
}))

cat("=== hours lost to the breadwinner norm, by year ===\n\n")
print(out[, .(YEAR,
              pct_bind   = round(pct_binding, 1),
              pct_lost   = round(pct_female_hours_lost, 2),
              per_hh     = round(lost_per_hh, 1),
              per_aff    = round(lost_per_affected, 1),
              FTE_mn     = round(fte_lost_millions, 3),
              his_gain_mn_hrs = round(hours_gain_total/1e6, 1),
              net_mn_hrs = round(hours_net_total/1e6, 1))], nrows = 30)

f80 <- out[1]; f24 <- out[.N]
cat("\n=== 1980 vs 2024 ===\n")
cat(sprintf("  %% of female hours lost to the norm : %.2f%%  ->  %.2f%%\n",
            f80$pct_female_hours_lost, f24$pct_female_hours_lost))
cat(sprintf("  hours lost per affected household  : %.0f   ->  %.0f\n",
            f80$lost_per_affected, f24$lost_per_affected))
cat(sprintf("  full-time-equivalent jobs lost     : %.2fM  ->  %.2fM\n",
            f80$fte_lost_millions, f24$fte_lost_millions))
cat(sprintf("  share of households norm binds on  : %.1f%%  ->  %.1f%%\n",
            f80$pct_binding, f24$pct_binding))
cat("\n  Intensity per affected household vs how many are affected:\n")
cat(sprintf("    per-affected hours lost  %+.0f%%\n",
            100*(f24$lost_per_affected/f80$lost_per_affected - 1)))
cat(sprintf("    share affected           %+.0f%%\n",
            100*(f24$pct_binding/f80$pct_binding - 1)))
cat(sprintf("    NET share of female hours lost %+.0f%%\n",
            100*(f24$pct_female_hours_lost/f80$pct_female_hours_lost - 1)))
fwrite(out, dated_path(results_dir, "t3_aggregate_distortion.csv"))
message("\nwrote t3_aggregate_distortion.csv")
