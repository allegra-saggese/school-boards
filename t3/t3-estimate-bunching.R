# =============================================================================
# T3 — estimation disciplined by DIRECT BUNCHING, with free curvature gamma
#
# Inputs : data/processed/panel/model_input_households.csv
#          data/processed/results/*_t2_bunching_estimates.csv   (data tau-hat targets)
# Outputs: data/processed/results/YYYY-MM-DD_t3_bunching_fit_by_year.csv
#          data/processed/results/YYYY-MM-DD_t3_bunching_gamma_profile.csv
#          data/processed/results/YYYY-MM-DD_t3_bunching_*_table.tex
#
# WHY THIS SCRIPT EXISTS (changes relative to t3-estimate-v2.R)
#   1. THE TARGETED NORM MOMENT IS NOW THE BUNCHING WEDGE, NOT THE CLIFF RATIO.
#      v2 fitted alpha to the cliff ratio (mass in [0.40,0.48) / mass in
#      (0.52,0.60]). That ratio also reflects the slope of the wage-driven
#      density through 0.5, and it implied a wedge tau of 0.17-0.26. The
#      design-based estimator in t2/t2-bunching.R (Saez 2010, tau-hat = 2b) puts
#      the data wedge at 0.02-0.04. Here the SAME estimator is run on simulated
#      couples and alpha is chosen so the model reproduces the data's tau-hat.
#      The cliff ratio becomes an UNTARGETED test.
#   2. CURVATURE gamma IS NO LONGER FIXED AT 1. With u(C) = C^(1-gamma)/(1-gamma)
#      the wedge is tau = alpha / u'(C) = alpha * C^gamma, so d ln tau / d ln C =
#      gamma. Log utility (gamma = 1) IMPOSES a unit income elasticity of the
#      norm; the data say about 0.1 (t2-bunching.R). gamma is therefore chosen to
#      match the data's income gradient of tau-hat, so the elasticity is an
#      estimate and not an assumption.
#
# MOMENTS PER YEAR, 2 PARAMETERS (alpha, f) AT EACH FIXED gamma — exactly identified
#   tau-hat (mean over husband's-income quintiles, rounding-adjusted) -> alpha
#   corner share (wife not working)                                  -> f
# gamma is then PROFILED: for each value on a grid, (alpha_t, f_t) are fitted
# in every year, and the model's income elasticity eta(gamma) — the slope of
# log tau-hat on log group resources with year fixed effects, computed exactly as
# for the data — is compared with the data's. The reported gamma-hat is the grid
# point whose eta is closest to the data's.
#
# DATA TARGET. t2-bunching.R "round_adj" series: all ties minus placebo ties.
# This is an UPPER bound on the behavioural wedge (correlated reporting by one
# respondent survives the placebo). The lower bound (exact ties dropped) is about
# zero, so the fitted alpha should be read as an upper bound. The model has no
# rounding, so it is compared with the rounding-adjusted series.
#
# UNTARGETED (reported, not fitted): cliff ratio; wife's hours share; share of
# couples where she out-earns him; corner share by husband's-wage quintile
# Q1/Q3/Q5; the model's tau-hat by quintile.
#
# WHAT THIS DOES AND DOES NOT DO. It disciplines the norm's size and income
# elasticity with the direct bunching measure. It does not model the wife's
# participation response to the norm (extensive margin), wealth, or wage
# feedback; those are next steps (see notes/t3-figures-and-tables.md).
# =============================================================================

suppressMessages(library(data.table))
source(here::here("_setup.R"))
source(here::here("t3", "t3-model-solver.R"))

panel_dir   <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
ensure_dir(results_dir)

years_do <- as.integer(strsplit(Sys.getenv("T3B_YEARS", "2019"), ",")[[1]])
gammas   <- as.numeric(strsplit(Sys.getenv("T3B_GAMMAS", "1,0.5,0.25,0.1"), ",")[[1]])
n_sim    <- as.integer(Sys.getenv("T3B_NSIM", "200000"))
maxit    <- as.integer(Sys.getenv("T3B_MAXIT", "120"))
# EXTENSIONS (defaults reproduce the baseline: symmetric kappa, common F)
#   T3B_THETA  "1" keeps kappa_f = kappa_m; "fit" estimates theta = kappa_f/kappa_m
#              so the model's wife-share of couple hours matches the data (a
#              third moment for a third parameter; read theta as tastes PLUS any
#              diffuse norm, which the data cannot separate).
#   T3B_SIGMAF dispersion of the entry cost: F_i = F * exp(sigma*e_i - sigma^2/2),
#              e_i ~ N(0,1) fixed across optimiser iterations, so mean F is
#              unchanged. NOT fitted: reported as a sensitivity, because fitting
#              it to the corner gradient would remove the gradient as a test.
theta_in <- Sys.getenv("T3B_THETA", "1")
fit_theta <- identical(theta_in, "fit"); theta_fix <- if (fit_theta) 1 else as.numeric(theta_in)
sigF     <- as.numeric(Sys.getenv("T3B_SIGMAF", "0"))
#   T3B_EPS    Frisch elasticity of hours (default 1 = the original solver).
#              Values other than 1 use t3-model-solver-eps.R, validated against
#              the original at eps = 1 and against brute force at eps = 0.5.
#   T3B_TARGET "bunching" (default: targets the Saez wedge and the corner share) or
#              "cliff" (targets the cliff ratio and the corner share, as in
#              t3-estimate-v2.R) so the two calibrations can be compared at the
#              same elasticity.
target_mode <- Sys.getenv("T3B_TARGET", "bunching")
eps_in   <- as.numeric(Sys.getenv("T3B_EPS", "1"))
if (eps_in != 1) source(here::here("t3", "t3-model-solver-eps.R"))
solve_any <- function(w_m, w_f, y0, Fv, a, k_m, k_f, g) {
  if (eps_in == 1) {
    solve_household(w_m, w_f, y0, Fv, a, k_m, k_f, 0, 0, gamma = g)
  } else {
    solve_household_eps(w_m, w_f, y0, Fv, a, k_m, k_f, 0, gamma = g, eps = eps_in)
  }
}
set.seed(20261006)

# ── Saez (2010) band estimator, identical to t2/t2-bunching.R ────────────────
# Bins of width 0.01 on the wife's share of couple labour earnings, k = 25..75.
# h = mean density in the adjacent bands (46,47 | 53,54); B = excess mass in the
# band 48..52; tau-hat = 2 * 0.01 * B / h.
k_fit <- 25:75
band  <- 48:52; lo <- 46:47; hi <- 53:54
tau_saez <- function(ef, em, wt, grp) {
  ok <- ef > 0 & em > 0 & !is.na(grp)
  k  <- as.integer(round(100 * ef[ok] / (ef[ok] + em[ok])))
  in_fit <- k >= min(k_fit) & k <= max(k_fit)
  cnt <- data.table(g = grp[ok][in_fit], k = k[in_fit], w = wt[ok][in_fit])[, .(c = sum(w)), by = .(g, k)]
  out <- vapply(1:5, function(q) {
    v <- numeric(length(k_fit)); names(v) <- k_fit
    x <- cnt[g == q]; v[as.character(x$k)] <- x$c
    h <- (mean(v[as.character(lo)]) + mean(v[as.character(hi)])) / 2
    if (!is.finite(h) || h <= 0) return(NA_real_)
    2 * 0.01 * (sum(v[as.character(band)]) - length(band) * h) / h
  }, numeric(1))
  setNames(out, paste0("tau_Q", 1:5))
}

# Deterministic HHWT-weighted quintiles (ties broken by row order).
wq <- function(x, w, k = 5L) {
  o <- order(x, seq_along(x)); g <- integer(length(x))
  g[o] <- pmin(k, 1L + as.integer(floor(k * (cumsum(w[o]) - w[o]) / sum(w))))
  g
}

# Income elasticity: slope of log tau on log group resources, year fixed effects,
# cells with tau > 0 only (as in t2-bunching.R).
eta_fit <- function(cells) {
  e <- cells[is.finite(tau) & tau > 0]
  if (nrow(e) <= uniqueN(e$YEAR) + 1L) return(NA_real_)
  if (uniqueN(e$YEAR) > 1L) coef(lm(log(tau) ~ factor(YEAR) + log(ybar), data = e))[["log(ybar)"]]
  else coef(lm(log(tau) ~ log(ybar), data = e))[["log(ybar)"]]
}

cliff_ratio <- function(z, wt, donut = 0.02) {
  below <- sum(wt[z >= 0.40 & z <  0.5 - donut], na.rm = TRUE)
  above <- sum(wt[z >  0.5 + donut & z <= 0.60], na.rm = TRUE)
  if (!is.finite(above) || above <= 0) NA_real_ else below / above
}

# ── Data ─────────────────────────────────────────────────────────────────────
dat <- fread(file.path(panel_dir, "model_input_households.csv"),
             select = c("YEAR", "HHWT", "f_lab", "m_lab", "f_w", "m_w", "y0", "y", "f_h", "m_h"),
             showProgress = FALSE)
dat <- dat[YEAR %in% years_do & is.finite(f_w) & is.finite(m_w) & f_w > 0 & m_w > 0 &
           is.finite(y0) & is.finite(f_h) & is.finite(m_h)]

# Data tau-hat targets from the T2 bunching run (rounding-adjusted, Saez).
bun <- read_newest(results_dir, "t2_bunching_estimates.csv$")
tgt <- bun[spec == "round_adj" & estimator == "saez" & YEAR %in% years_do,
           .(YEAR, grp, tau)]
low <- bun[spec == "no_exact" & estimator == "saez" & YEAR %in% years_do,
           .(tau_lower = mean(tau)), by = YEAR]

prep <- lapply(setNames(years_do, years_do), function(yr) {
  d <- dat[YEAR == yr]
  d[, hq := NA_integer_]; d[m_lab > 0, hq := wq(m_lab, HHWT)]
  d[, qw := wq(m_w, HHWT)]
  ymed <- median(d$y, na.rm = TRUE)
  int  <- d[f_h > 0 & m_h > 0]
  Cbar <- weighted.mean(int$m_w * int$m_h + int$f_w * int$f_h + int$y0, int$HHWT)
  ybar <- d[!is.na(hq), .(ybar = weighted.mean(f_lab + m_lab + y0, HHWT)), by = .(grp = hq)]
  # data tau-hat recomputed here on this script's groups, as a check against T2's
  ef <- d$f_lab; em <- d$m_lab
  chk <- tau_saez(ef, em, d$HHWT, d$hq)
  si <- if (nrow(d) > n_sim) sort(sample.int(nrow(d), n_sim)) else seq_len(nrow(d))
  list(yr = yr, d = d, dS = d[si], ymed = ymed, Cbar = Cbar, int = int, ybar = ybar,
       data_tau = tgt[YEAR == yr][order(grp), tau], recomputed_all = chk,
       corner = weighted.mean(d$f_h <= 0, d$HHWT))
})
message("data tau-hat targets (T2 round_adj, mean over quintiles) and check:")
for (p in prep) message(sprintf("  %d  target %.4f | this script's quintiles, all ties %.4f | lower bound %.4f | corner %.3f",
                                p$yr, mean(p$data_tau), mean(p$recomputed_all, na.rm = TRUE),
                                low[YEAR == p$yr, tau_lower], p$corner))

# ── Simulation moments ────────────────────────────────────────────────────────
sim_moments <- function(s, dS, ymed) {
  e_m <- dS$m_w * s$h_m; e_f <- dS$f_w * s$h_f
  z   <- fifelse(e_m + e_f > 0, e_f / (e_m + e_f), NA_real_)
  wt  <- dS$HHWT
  cs  <- function(q) sum(wt[dS$qw == q & s$h_f <= 0]) / sum(wt[dS$qw == q])
  tq  <- tau_saez(e_f, e_m, wt, dS$hq)
  c(tau_bar = mean(tq, na.rm = TRUE), tq,
    corner = sum(wt[s$h_f <= 0]) / sum(wt),
    cliff = cliff_ratio(z, wt),
    hshare = sum(wt * s$h_f) / sum(wt * (s$h_f + s$h_m)),
    outearn = sum(wt[!is.na(z) & z > 0.5]) / sum(wt[!is.na(z)]),
    cornerQ1 = cs(1L), cornerQ3 = cs(3L), cornerQ5 = cs(5L), de_by_q(s$h_m, s$h_f, wt, dS$qw))
}
# Dual-earner hours tests by husband's-wage quintile (Q1, Q5): share where she
# works more hours than he does, and her share of the couple's hours.
de_by_q <- function(h_m, h_f, wt, qw) {
  dual <- h_f > 0 & h_m > 0
  unlist(lapply(c(1L, 5L), function(q) {
    id <- dual & qw == q
    setNames(c(sum(wt[id & h_f > h_m]) / sum(wt[id]),
               sum(wt[id] * h_f[id]) / sum(wt[id] * (h_f[id] + h_m[id]))),
             paste0(c("overhrsQ", "hshareDEQ"), q))
  }))
}
data_moments <- function(p) {
  d <- p$d; wt <- d$HHWT
  e_m <- d$m_w * d$m_h; e_f <- d$f_w * d$f_h
  z <- fifelse(e_m + e_f > 0, e_f / (e_m + e_f), NA_real_)
  cs <- function(q) sum(wt[d$qw == q & d$f_h <= 0]) / sum(wt[d$qw == q])
  c(corner = sum(wt[d$f_h <= 0]) / sum(wt), cliff = cliff_ratio(z, wt),
    hshare = sum(wt * d$f_h) / sum(wt * (d$f_h + d$m_h)),
    outearn = sum(wt[!is.na(z) & z > 0.5]) / sum(wt[!is.na(z)]),
    cornerQ1 = cs(1L), cornerQ3 = cs(3L), cornerQ5 = cs(5L), de_by_q(d$m_h, d$f_h, wt, d$qw))
}
dmom <- lapply(prep, data_moments)

# ── Estimation: profile over gamma, exactly identified at each gamma ─────────
fits <- list()
for (g in gammas) {
  for (p in prep) {
    t0 <- Sys.time()
    # kappa from his first-order condition at dual-earner means:
    #   kappa * h_m = w_m * C^(-gamma)   (reduces to v2's formula at gamma = 1)
    kap <- weighted.mean(p$int$m_w, p$int$HHWT) * p$Cbar^(-g) / weighted.mean(p$int$m_h, p$int$HHWT)^(1 / eps_in)
    dS  <- p$dS; kS <- rep(kap, nrow(dS))
    set.seed(20261006L + p$yr); eps <- rnorm(nrow(dS))
    target <- c(tau_bar = mean(p$data_tau), corner = p$corner)
    if (target_mode == "cliff")      # the earlier calibration: cliff ratio instead of the bunching wedge
      target <- c(cliff = unname(dmom[[as.character(p$yr)]]["cliff"]), corner = p$corner)
    if (fit_theta) target <- c(target, hshare = unname(dmom[[as.character(p$yr)]]["hshare"]))
    pars <- function(par) {
      f <- plogis(par[2]) * 0.5
      list(a = exp(par[1]), f = f, th = if (fit_theta) exp(par[3]) else theta_fix,
           Fv = f * p$ymed * exp(sigF * eps - sigF^2 / 2))
    }
    sim <- function(par, alpha_on = TRUE) {
      q <- pars(par)
      s <- solve_any(dS$m_w, dS$f_w, dS$y0, q$Fv, if (alpha_on) q$a else 0,
                     kS, q$th * kS, g)
      list(s = s, m = sim_moments(s, dS, p$ymed))
    }
    loss <- function(par) {
      m <- sim(par)$m[names(target)]
      if (any(!is.finite(m))) return(1e6)
      sum(((m - target) / pmax(abs(target), 1e-6))^2)
    }
    st0 <- c(log(if (target_mode == "cliff") 0.15 else 0.03) - g * log(p$Cbar), qlogis(0.10 / 0.5), if (fit_theta) log(1.5))
    fit <- optim(st0, loss, method = "Nelder-Mead",
                 control = list(maxit = maxit, reltol = 1e-8))
    q <- pars(fit$par); a <- q$a; f <- q$f
    r <- sim(fit$par); m <- r$m
    # no-norm baseline at the same f, kappa, theta, gamma, F draws
    m0 <- sim(fit$par, alpha_on = FALSE)$m
    bind <- r$s$regime %in% c(2L, 3L)
    tau_bind <- if (any(bind)) a * weighted.mean(r$s$C[bind]^g, dS$HHWT[bind]) else NA_real_
    row <- data.table(gamma = g, eps = eps_in, theta = q$th, sigmaF = sigF, YEAR = p$yr, kappa = kap, alpha = a, f = f,
                      F_dollars = f * p$ymed, loss = fit$value, converged = fit$convergence == 0,
                      tau_binding = tau_bind, pct_bound = 100 * weighted.mean(bind, dS$HHWT),
                      secs = as.numeric(difftime(Sys.time(), t0, units = "secs")))
    row <- cbind(row,
                 setNames(as.data.table(as.list(m)),  paste0("model_",  names(m))),
                 setNames(as.data.table(as.list(m0)), paste0("nonorm_", names(m0))),
                 setNames(as.data.table(as.list(dmom[[as.character(p$yr)]])), paste0("data_", names(dmom[[1]]))),
                 setNames(as.data.table(as.list(p$data_tau)), paste0("data_tau_Q", 1:5)))
    fits[[length(fits) + 1L]] <- row
    message(sprintf("gamma %.2f  %d | alpha %.3e f %.4f loss %.1e | tau_hat model %.4f data %.4f | corner %.3f/%.3f | tau_bind %.3f | %.0fs",
                    g, p$yr, a, f, fit$value, m["tau_bar"], target["tau_bar"], m["corner"],
                    target["corner"], tau_bind, row$secs))
  }
}
fits <- rbindlist(fits, fill = TRUE)

# ── Income elasticity by gamma, model vs data, same cells and same formula ───
ybar_all <- rbindlist(lapply(prep, function(p) p$ybar[, YEAR := p$yr]))
cells <- function(tau_cols, dt) {
  melt(dt[, c("YEAR", tau_cols), with = FALSE], id.vars = "YEAR",
       variable.name = "q", value.name = "tau")[
    , grp := as.integer(sub(".*Q", "", q))][ybar_all, on = .(YEAR, grp), nomatch = 0]
}
eta_data <- eta_fit(cells(paste0("data_tau_Q", 1:5), unique(fits[, c("YEAR", paste0("data_tau_Q", 1:5)), with = FALSE])))
prof <- fits[, .(eta_model = eta_fit(cells(paste0("model_tau_Q", 1:5), .SD)),
                 tau_bar_model = mean(model_tau_bar), tau_bar_data = mean(rowMeans(.SD[, paste0("data_tau_Q", 1:5), with = FALSE])),
                 corner_mae = mean(abs(model_corner - data_corner)),
                 cliff_mae = mean(abs(model_cliff - data_cliff)),
                 cliff_mae_nonorm = mean(abs(nonorm_cliff - data_cliff)),
                 outearn_mae = mean(abs(model_outearn - data_outearn)),
                 outearn_mae_nonorm = mean(abs(nonorm_outearn - data_outearn)),
                 hshare_mae = mean(abs(model_hshare - data_hshare)),
                 hshare_mae_nonorm = mean(abs(nonorm_hshare - data_hshare)),
                 cornerQ1 = mean(model_cornerQ1), cornerQ5 = mean(model_cornerQ5),
                 data_cornerQ1 = mean(data_cornerQ1), data_cornerQ5 = mean(data_cornerQ5),
                 tau_binding = mean(tau_binding), pct_bound = mean(pct_bound),
                 max_loss = max(loss)), by = gamma]
prof[, eta_data := eta_data][, gap := abs(eta_model - eta_data)]
setorder(prof, gamma)
cat("\n=== gamma profile (data eta = ", round(eta_data, 3), ") ===\n", sep = "")
print(prof[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 4) else x)])
best <- prof[which.min(gap), gamma]
message("\ngamma-hat (closest model eta to data eta) = ", best)

# tau-hat NET OF COMPOSITION: the data's tau-hat minus what the same estimator
# returns on the no-norm model (alpha = 0). The no-norm model already produces a
# positive tau-hat because the wage-driven density is not smooth at 0.5; the
# estimator cannot tell that from a norm. The net is the part of the data's
# bunching that wage composition alone does not generate.
fits[, `:=`(data_tau_bar = rowMeans(.SD),
            net_tau_bar  = rowMeans(.SD) - nonorm_tau_bar),
     .SDcols = paste0("data_tau_Q", 1:5)]
tag <- Sys.getenv("T3B_TAG", "run")
fwrite(fits, dated_path(results_dir, paste0("t3_bunching_fit_by_year_", tag, ".csv")))
fwrite(prof, dated_path(results_dir, paste0("t3_bunching_gamma_profile_", tag, ".csv")))
message("wrote t3_bunching_fit_by_year_", tag, ".csv, t3_bunching_gamma_profile_", tag, ".csv")
