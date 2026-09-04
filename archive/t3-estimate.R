# =============================================================================
# T3 — estimate (alpha1, alpha2) year by year against real data
#
# WHAT CALIBRATION MEANS HERE, concretely:
#   1. compute a statistic in the DATA          (cliff ratio; corner share)
#   2. guess parameters, solve every household, compute the SAME statistic
#      on the SIMULATED output
#   3. search until simulated == data
# Two unknowns, two statistics.
#
# WHICH PARAMETER IS IDENTIFIED BY WHICH (verified orthogonal in testing):
#   alpha1 -> the CLIFF  (bunching just below equal earnings). Intensive margin.
#   alpha2 -> the CORNER (share of wives at zero hours).       Extensive margin.
#
# Each YEAR is estimated separately: these are repeated cross-sections, so each
# year is a different t, and F = f*median(y_t) rescales itself automatically.
# =============================================================================

library(data.table)
source("t3-model-solver.R")
source(here::here("_setup.R"))

panel_dir   <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
ensure_dir(results_dir)

f_share  <- 0.10          # F = 10% of median household income. ASSUMPTION, see sensitivity.
donut    <- 0.02
years_do <- as.integer(strsplit(Sys.getenv("T3_YEARS", "2019"), ",")[[1]])

dat <- fread(file.path(panel_dir, "model_input_households.csv"), showProgress = FALSE)
dat <- dat[is.finite(f_w) & is.finite(m_w) & f_w > 0 & m_w > 0 & is.finite(y0) &
           is.finite(f_h) & is.finite(m_h)]

# ── moments, computed IDENTICALLY on data and simulation ────────────────────
cliff_ratio <- function(z, wt) {
  below <- sum(wt[z >= 0.40 & z <  0.5 - donut], na.rm = TRUE)
  above <- sum(wt[z >  0.5 + donut & z <= 0.60], na.rm = TRUE)
  if (!is.finite(above) || above <= 0) return(NA_real_)
  below / above
}
moments <- function(h_m, h_f, w_m, w_f, wt) {
  e_m <- w_m * h_m; e_f <- w_f * h_f
  z   <- fifelse(e_m + e_f > 0, e_f / (e_m + e_f), NA_real_)
  list(cliff  = cliff_ratio(z, wt),
       corner = sum(wt[h_f <= 0]) / sum(wt),
       hshare = sum(wt * h_f) / sum(wt * (h_f + h_m)),
       outearn= sum(wt[!is.na(z) & z > 0.5]) / sum(wt[!is.na(z)]))
}

out <- rbindlist(lapply(years_do, function(yr) {
  d <- dat[YEAR == yr]
  if (nrow(d) < 5000L) return(NULL)
  message("\n=== ", yr, "  (n = ", format(nrow(d), big.mark = ","), ") ===")

  F <- f_share * median(d$y, na.rm = TRUE)
  # kappa imposed SYMMETRIC (kappa_m = kappa_f). Gender asymmetry must come from
  # the wage gap and the norm, not from assumed preferences -- otherwise alpha
  # is not identified. Pinned from the husband's Regime-I FOC at sample means,
  # he being least affected by the norm and by selection into work.
  int  <- d[f_h > 0 & m_h > 0]
  Cbar <- weighted.mean(int$m_w*int$m_h + int$f_w*int$f_h + int$y0, int$HHWT)
  kap  <- weighted.mean(int$m_w, int$HHWT) / (Cbar * weighted.mean(int$m_h, int$HHWT))
  k_m  <- rep(kap, nrow(d)); k_f <- rep(kap, nrow(d))

  md <- with(d, moments(m_h, f_h, m_w, f_w, HHWT))
  message(sprintf("  DATA   cliff %.3f | corner %.3f | wife hours share %.3f | out-earn %.3f",
                  md$cliff, md$corner, md$hshare, md$outearn))
  message(sprintf("  kappa  %.3e   Cbar $%s   F $%s", kap,
                  format(round(Cbar), big.mark=","), format(round(F), big.mark=",")))

  sim <- function(a1, a2) {
    s <- solve_household(d$m_w, d$f_w, d$y0, F, a1, k_m, k_f, 0, a2)
    moments(s$h_m, s$h_f, d$m_w, d$f_w, d$HHWT)
  }
  # alpha2 first: it drives the corner and is orthogonal to the cliff.
  a2g <- seq(0, 0.60, by = 0.01)
  cor2 <- vapply(a2g, function(a) sim(0, a)$corner, numeric(1))
  a2h  <- a2g[which.min(abs(cor2 - md$corner))]
  # then alpha1 against the cliff, holding alpha2 at its solution.
  a1g <- c(0, 10^seq(-7, -4, length.out = 25))
  cl1 <- vapply(a1g, function(a) sim(a, a2h)$cliff, numeric(1))
  a1h  <- a1g[which.min(abs(cl1 - md$cliff))]

  ms <- sim(a1h, a2h)
  message(sprintf("  FIT    alpha1 %.3e | alpha2 %.3f", a1h, a2h))
  message(sprintf("  MODEL  cliff %.3f | corner %.3f | wife hours share %.3f | out-earn %.3f",
                  ms$cliff, ms$corner, ms$hshare, ms$outearn))
  message("         (cliff & corner are TARGETED; hours share & out-earn are NOT")
  message("          -- they are the model's out-of-sample test)")

  data.table(YEAR = yr, n = nrow(d), kappa = kap, F = F,
             alpha1 = a1h, alpha2 = a2h,
             cliff_data = md$cliff,  cliff_model = ms$cliff,
             corner_data= md$corner, corner_model= ms$corner,
             hshare_data= md$hshare, hshare_model= ms$hshare,
             outearn_data=md$outearn,outearn_model=ms$outearn)
}))

if (nrow(out)) {
  print(out)
  fwrite(out, dated_path(results_dir, "t3_estimates_by_year.csv"))
  message("\nwrote t3_estimates_by_year.csv")
}
