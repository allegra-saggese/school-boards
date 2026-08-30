library(data.table)
library(ggplot2)

source("functions.R")
source("R/paths.R")

# =========================================================
# Household utility model with a CONTINUOUS breadwinner-norm penalty
#
# Implements the model in Saggese (May 2026), slides 34-40. This REPLACES the
# specification in model-simulation.R, which differs from the theory in three
# ways that change the results:
#
#   1. PENALTY SHAPE. Presentation: V(h_m,h_f) = max(w_f*h_f - w_m*h_m, 0), a
#      CONTINUOUS dollar excess. Old code: theta * 1[z_W >= 0.5], a discrete
#      indicator. These have different empirical signatures -- a continuous
#      penalty produces a KINK at the threshold, an indicator produces a JUMP.
#      The data show a kink of -25.2 hrs/pp and a jump of only -0.386 hrs, so
#      the continuous form is the one the evidence supports.
#
#   2. INCOME ELASTICITY. Presentation DERIVES it: with tau = alpha_eff/u'(C)
#      and u'' < 0,  d(tau)/dC = -alpha_eff*u''(C)/[u'(C)]^2 > 0, so the norm
#      wedge rises with consumption automatically. Old code ASSUMED it by
#      writing theta = tau*lambda*(y/y_bar). Deriving the paper's central result
#      is a much stronger claim than imposing it.
#
#   3. NORM INTENSITY. Presentation: alpha_eff = alpha(P_j) * g(delta_bar),
#      where P is an internalised prescription (education, frontier, cohort) and
#      g is a social multiplier. Old code used a Dem/Rep dummy. This is a
#      HOUSEHOLD model; political geography is evidence about heterogeneity in
#      the norm, not a primitive of it.
#
# THE MODEL
#   max_{h_m,h_f >= 0}  u(C) + v^m(T-h_m) + v^f(T-h_f) - alpha_eff*V(h_m,h_f)
#   s.t.  C = w_m*h_m + w_f*h_f + y0
#         V = max(w_f*h_f - w_m*h_m, 0)
#
# With u(C) = log C and v^i(L) = -kappa_i/(1+1/eps) * h_i^(1+1/eps), the FOCs are
#   Case I  (w_m h_m >= w_f h_f):  kappa_i h_i^(1/eps) = w_i / C
#   Case II (w_f h_f >  w_m h_m):  the norm acts as a WEDGE,
#        kappa_m h_m^(1/eps) = w_m(1 + tau)/C ,  kappa_f h_f^(1/eps) = w_f(1 - tau)/C
#     i.e. a proportional subsidy on his labour and an equal proportional tax on
#     hers, with tau = alpha_eff / u'(C) = alpha_eff * C  (since u' = 1/C).
#
# Because tau = alpha_eff * C, the wedge is mechanically increasing in
# consumption -- this IS the income elasticity, and it is derived, not imposed.
# =========================================================

set.seed(42)

panel_dir   <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
ensure_dir(results_dir)

params <- list(
  eps      = 1.0,      # Frisch elasticity, imposed (standard)
  T_endow  = 4000,     # annual hours endowment (~77 hrs/wk)
  kappa_m  = NA_real_, # calibrated from the FOC at sample means
  kappa_f  = NA_real_,
  alpha    = NA_real_  # the ONE free parameter (norm intensity)
)

# ── Utility and the household problem ─────────────────────────────────────
util <- function(h_m, h_f, w_m, w_f, y0, alpha, p) {
  C <- w_m * h_m + w_f * h_f + y0
  if (C <= 0) return(-Inf)
  V <- max(w_f * h_f - w_m * h_m, 0)         # continuous dollar excess
  log(C) -
    p$kappa_m / (1 + 1/p$eps) * h_m^(1 + 1/p$eps) -
    p$kappa_f / (1 + 1/p$eps) * h_f^(1 + 1/p$eps) -
    alpha * V
}

# Solve one household. The penalty is continuous but KINKED at equal earnings,
# so the objective is not differentiable there; we evaluate three candidates and
# take the best, exactly as the presentation's Case I / Case II split implies.
solve_hh <- function(w_m, w_f, y0, alpha, p) {
  obj <- function(h) -util(h[1], h[2], w_m, w_f, y0, alpha, p)

  # (A) interior, norm slack  (B) interior, norm binding  (C) at the kink
  startA <- c(2000, 1500)
  startB <- c(2000,  500)
  optA <- optim(startA, obj, method = "L-BFGS-B",
                lower = c(0, 0), upper = c(p$T_endow, p$T_endow))
  optB <- optim(startB, obj, method = "L-BFGS-B",
                lower = c(0, 0), upper = c(p$T_endow, p$T_endow))
  # (C) constrained exactly at equal earnings: w_f h_f = w_m h_m
  objC <- function(hm) {
    hf <- (w_m / w_f) * hm
    if (hf < 0 || hf > p$T_endow) return(Inf)
    -util(hm, hf, w_m, w_f, y0, alpha, p)
  }
  optC <- optimize(objC, interval = c(0, p$T_endow))
  # (D) corner: she does not work
  objD <- function(hm) -util(hm, 0, w_m, w_f, y0, alpha, p)
  optD <- optimize(objD, interval = c(0, p$T_endow))

  cands <- list(
    list(h_m = optA$par[1], h_f = optA$par[2], v = -optA$value),
    list(h_m = optB$par[1], h_f = optB$par[2], v = -optB$value),
    list(h_m = optC$minimum, h_f = (w_m/w_f)*optC$minimum, v = -optC$objective),
    list(h_m = optD$minimum, h_f = 0,                      v = -optD$objective)
  )
  best <- cands[[which.max(vapply(cands, function(x) x$v, numeric(1)))]]
  c(h_m = best$h_m, h_f = best$h_f)
}

# ── Load the model dataset ────────────────────────────────────────────────
dat <- fread(file.path(panel_dir, "model_input_households.csv"), showProgress = FALSE)
dat <- dat[is.finite(f_w) & is.finite(m_w) & f_w > 0 & m_w > 0 & is.finite(y0)]
message("model households with usable wages: ", format(nrow(dat), big.mark = ","))

dat[, y_dec := cut(y, quantile(y, seq(0, 1, .1), na.rm = TRUE),
                   labels = 1:10, include.lowest = TRUE), by = YEAR]
dat[, y_dec := as.integer(as.character(y_dec))]

# ── Pin kappa from the Case I FOC at sample means ─────────────────────────
# Case I with eps = 1:  kappa_i * h_i = w_i / C  =>  kappa_i = w_i / (C * h_i)
# Evaluated on INTERIOR households, where the norm is slack for most couples so
# the undistorted FOC is the right one to invert. kappa is imposed from data;
# only alpha is free.
int   <- dat[regime == "interior" & f_h > 0 & m_h > 0]
Cbar  <- int[, weighted.mean(f_w * f_h + m_w * m_h + y0, HHWT)]
params$kappa_m <- int[, weighted.mean(m_w, HHWT)] / (Cbar * int[, weighted.mean(m_h, HHWT)])
params$kappa_f <- int[, weighted.mean(f_w, HHWT)] / (Cbar * int[, weighted.mean(f_h, HHWT)])
message(sprintf("  kappa_m = %.4e   kappa_f = %.4e   (Cbar = $%s)",
                params$kappa_m, params$kappa_f, format(round(Cbar), big.mark = ",")))

# ── Moments, computed identically on data and simulation ──────────────────
cliff_ratio <- function(z, wt, donut = 0.02) {
  below <- sum(wt[z >= 0.40 & z <  0.5 - donut], na.rm = TRUE)
  above <- sum(wt[z >  0.5 + donut & z <= 0.60], na.rm = TRUE)
  if (!is.finite(above) || above <= 0) return(NA_real_)
  below / above
}
moments_of <- function(z, h_f, wt, y_dec) {
  ok <- is.finite(z)
  list(cliff        = cliff_ratio(z[ok], wt[ok]),
       corner_share = weighted.mean(h_f <= 0, wt),
       outearn      = weighted.mean(z[ok] > 0.5, wt[ok]),
       cliff_bot    = cliff_ratio(z[ok & y_dec <= 3], wt[ok & y_dec <= 3]),
       cliff_top    = cliff_ratio(z[ok & y_dec >= 9], wt[ok & y_dec >= 9]))
}
m_data <- with(dat, moments_of(z_W, f_h, HHWT, y_dec))
message("\n=== DATA moments ===")
message(sprintf("  cliff ratio (all)      : %.3f", m_data$cliff))
message(sprintf("  cliff ratio (D1-D3)    : %.3f", m_data$cliff_bot))
message(sprintf("  cliff ratio (D9-D10)   : %.3f", m_data$cliff_top))
message(sprintf("  corner share (h_f = 0) : %.3f", m_data$corner_share))
message(sprintf("  wife out-earns         : %.3f", m_data$outearn))

# ── Simulate ──────────────────────────────────────────────────────────────
simulate_alpha <- function(alpha, sdat, p) {
  hs <- vapply(seq_len(nrow(sdat)), function(i)
    solve_hh(sdat$m_w[i], sdat$f_w[i], sdat$y0[i], alpha, p), numeric(2))
  sim <- data.table(h_m = hs[1, ], h_f = hs[2, ], w_m = sdat$m_w, w_f = sdat$f_w,
                    HHWT = sdat$HHWT, y_dec = sdat$y_dec)
  sim[, earn_m := w_m * h_m][, earn_f := w_f * h_f]
  sim[, z := fifelse((earn_m + earn_f) > 0, earn_f / (earn_m + earn_f), NA_real_)]
  with(sim, moments_of(z, h_f, HHWT, y_dec))
}

n_cal <- 3000L   # households per alpha evaluation (4 optimisations each)
cal <- dat[sample(.N, min(n_cal, .N))]
alpha_grid <- c(0, 1e-7, 3e-7, 1e-6, 3e-6, 1e-5)

message("\n=== calibrating alpha  (TARGET: aggregate cliff ratio only) ===")
grid_res <- rbindlist(lapply(alpha_grid, function(a) {
  m <- simulate_alpha(a, cal, params)
  message(sprintf("  alpha=%.1e  cliff=%.3f  corner=%.3f  outearn=%.3f  cliff_bot=%.3f  cliff_top=%.3f",
                  a, m$cliff, m$corner_share, m$outearn, m$cliff_bot, m$cliff_top))
  data.table(alpha = a, cliff = m$cliff, corner = m$corner_share, outearn = m$outearn,
             cliff_bot = m$cliff_bot, cliff_top = m$cliff_top)
}))
grid_res[, data_cliff := m_data$cliff]
fwrite(grid_res, file.path(results_dir, "model_v2_alpha_grid.csv"))
message("\nwrote: model_v2_alpha_grid.csv")
message("\nUNTARGETED moments to judge the model on: cliff_bot vs cliff_top")
message("(the income gradient), corner share, and out-earn share. Only the")
message("aggregate cliff ratio was used to pick alpha.")
