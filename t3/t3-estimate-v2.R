# =============================================================================
# T3 — estimation of (alpha, f), year by year
#
# Input  : data/processed/panel/model_input_households.csv
# Outputs: data/processed/results/YYYY-MM-DD_t3_estimates_v2_by_year.csv
#
# THE MODEL. The identity penalty enters through the wedge tau = alpha / u'(C),
# which under log utility is alpha*C: a proportional subsidy on his hours and
# an equal proportional tax on hers. alpha is therefore an HOURS parameter, and
# its empirical signature is bunching at equal earnings (the cliff).
#
# ONE NORM PARAMETER, NOT TWO. A relative-earnings norm barely moves
# participation: V = 0 at the KINK as well as at the corner, and bunching
# preserves her earnings while withdrawing does not. This is NOT a theorem —
# the kink pays a second fixed cost F that the corner does not — so the norm can
# push her out only when W_III < W_IV < W_I. How many such couples exist is
# verified on the data each year (pct_f_exit_any below).
#
#   alpha -> hours; the cliff; the intensive margin. This is the norm.
#   F     -> the corner. A TECHNOLOGY: the goods cost of replacing home
#            production once a spouse enters the market. No identity content,
#            and not a preference.
#
# T3 explains the cliff and hours. T2's participation finding is an empirical
# result the theory does not claim to generate.
#
# F IS ESTIMATED, NOT ASSUMED. F = f * median(y_t), a share of that year's
# median household income, so it needs no external calibration and deflates
# itself across a 44-year sample.
#
# 2 MOMENTS, 2 PARAMETERS — exactly identified:
#   cliff ratio            -> alpha
#   corner share overall   -> f
# With as many moments as parameters the estimates do not depend on the
# weighting of the loss, and the loss at the optimum should be ~0 (reported;
# a value well above 0 means the targets are not jointly attainable). The
# previous 5-moment version also targeted the corner share in quintiles Q1/Q3/Q5
# of the husband's wage. That made the estimates depend on an arbitrary
# identity weighting, and its Q1/Q5 misfit was a failed over-identification
# test (J far above the chi2(3) critical value 7.8) that was being reported as
# fit. Those three moments are now UNTARGETED tests, as is everything below.
#
# Reported in three groups:
#   (A) TARGETED — cliff, corner. Fit is expected by construction; not evidence.
#   (B) UNTARGETED, aggregate — wife's share of couple hours, share of couples
#       where she out-earns him, corner share in Q1/Q3/Q5.
#   (C) UNTARGETED, by husband's-wage quintile — the intensive-margin tests
#       cliff_Q, overhrs_Q, hshareDE_Q, each as DATA / MODEL / NO NORM.
#
# MODEL CHECKS, per year on the full sample at the fitted (alpha, f):
#   pct_atT            share of households with h_i = T (expect 0: T binds on
#                      no one, so T = 8,760 is not doing any work)
#   pct_switch_hat     share whose participation pattern differs between
#                      alpha = 0 and alpha-hat (expect ~0: the norm is an hours
#                      mechanism)
#   pct_f_exit_any     share of wives the norm would push to the corner at ANY
#                      alpha, i.e. W_III < W_IV < W_I (see the check itself)
#
# Each year is estimated separately (repeated cross-sections), with a
# continuous optimiser rather than a grid — a grid quantises alpha into
# spurious time variation.
# =============================================================================

library(data.table)
source(here::here("_setup.R"))
source(here::here("t3", "t3-model-solver.R"))

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
  base <- c(cliff   = cliff_ratio(z, wt),
            corner  = sum(wt[h_f <= 0]) / sum(wt),
            cornerQ1= cs(qgrp == 1L), cornerQ3 = cs(qgrp == 3L), cornerQ5 = cs(qgrp == 5L),
            corner_m= sum(wt[h_m <= 0]) / sum(wt),   # husband not working
            hshare  = sum(wt * h_f) / sum(wt * (h_f + h_m)),
            outearn = sum(wt[!is.na(z) & z > 0.5]) / sum(wt[!is.na(z)]))
  # UNTARGETED intensive-margin tests by husband's-wage quintile q = 1..5.
  # The model's claim: tau = alpha*C rises with household resources, so where
  # the norm binds, wives in high-q households cut MORE hours. Three checks,
  # each computed identically on data and simulation:
  #   cliff_q   : cliff ratio within quintile q (more missing mass above 0.5
  #               => a stronger norm there)
  #   overhrs_q : share of DUAL-EARNER couples in which she works more hours
  #               than he does
  #   hshareDE_q: her share of couple hours among DUAL-EARNER couples
  dual <- h_f > 0 & h_m > 0
  byq <- unlist(lapply(1:5, function(q) {
    i  <- qgrp == q
    id <- i & dual
    setNames(c(cliff_ratio(z[i], wt[i]),
               sum(wt[id & h_f > h_m]) / sum(wt[id]),
               sum(wt[id] * h_f[id]) / sum(wt[id] * (h_f[id] + h_m[id]))),
             paste0(c("cliff_Q", "overhrs_Q", "hshareDE_Q"), q))
  }))
  c(base, byq)
}
TARGETS <- c("cliff", "corner")

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

  message(sprintf("  FIT   alpha %.4e | f %.4f (F = $%s) | loss %.2e (exactly identified: expect ~0)",
                  a1, f, format(round(f*ymed), big.mark=","), fit$value))
  # NO-NORM BASELINE. Every quintile gradient below exists even at alpha = 0,
  # because husbands in Q5 out-earn their wives mechanically (wage
  # composition). The norm's contribution in quintile q is MODEL - MODEL0;
  # the test is whether the DATA sit where MODEL puts them, not where MODEL0 does.
  s0  <- solve_household(dS$m_w, dS$f_w, dS$y0, f * ymed, 0, kS_m, kS_f, 0, 0)
  ms0 <- moments(s0$h_m, s0$h_f, dS$m_w, dS$f_w, dS$HHWT, qS)

  # ── Report: (A) targeted, (B) untargeted aggregate, (C) untargeted by quintile
  row3 <- function(nm) sprintf("    %-10s DATA %.3f | MODEL %.3f | NO NORM %.3f",
                               nm, md[nm], ms[nm], ms0[nm])
  message("  (A) TARGETED -- fit is by construction, not evidence")
  for (nm in TARGETS) message(row3(nm))
  message("  (B) UNTARGETED, aggregate")
  for (nm in c("hshare", "outearn", "cornerQ1", "cornerQ3", "cornerQ5", "corner_m")) message(row3(nm))
  message("  (C) UNTARGETED, by husband's-wage quintile (Q1..Q5); MAE = mean |x - DATA|")
  qtab <- function(m, nm) paste(sprintf("%.3f", m[paste0(nm, 1:5)]), collapse = " ")
  qmae <- function(m, nm) mean(abs(m[paste0(nm, 1:5)] - md[paste0(nm, 1:5)]), na.rm = TRUE)
  for (nm in c("cliff_Q", "overhrs_Q", "hshareDE_Q"))
    message(sprintf("    %-10s DATA %s | MODEL %s | NO NORM %s | MAE model %.3f vs no-norm %.3f",
                    nm, qtab(md, nm), qtab(ms, nm), qtab(ms0, nm),
                    qmae(ms, nm), qmae(ms0, nm)))

  # ── Model checks, on the FULL year at the fitted (alpha, f) ─────────────────
  # (i)  pct_atT: households on a face of the time constraint, h_i = T.
  # (ii) pct_switch_hat: participation pattern (who works) differs between
  #      alpha = 0 and alpha-hat.
  # (iii) pct_f_exit_any: wives the norm would push to the corner at ANY alpha.
  #      Her-working values (both-work, she-only) are nonincreasing in alpha,
  #      while the values of IV (he only) and VI (neither) do not depend on it.
  #      So once she is out she stays out as alpha rises, and the set that ever
  #      exits is the set that exits as alpha -> infinity: W_III < W_IV < W_I.
  #      That limit is approximated at alpha = 1, a wedge tau = C -- a tax of
  #      tens of thousands of percent on her marginal earnings.
  kF   <- rep(kap, nrow(d))
  sH   <- solve_household(d$m_w, d$f_w, d$y0, f * ymed, a1, kF, kF)
  sH0  <- solve_household(d$m_w, d$f_w, d$y0, f * ymed, 0,  kF, kF)
  sInf <- solve_household(d$m_w, d$f_w, d$y0, f * ymed, 1,  kF, kF)
  wt   <- d$HHWT
  pct  <- function(x) 100 * sum(wt[x]) / sum(wt)
  # The switch is split by spouse and direction. The wife can only EXIT as
  # alpha rises (see (iii)); the husband can only ENTER, because the norm
  # penalises she-only households (V = w_f*h_f, the full penalty) and so pushes
  # a non-working husband into work, to the kink or to regime II.
  chk <- c(pct_atT        = pct(sH$h_m >= T_ENDOW | sH$h_f >= T_ENDOW),
           pct_switch_hat = pct((sH$h_m > 0) != (sH0$h_m > 0) | (sH$h_f > 0) != (sH0$h_f > 0)),
           pct_f_exit_hat = pct(sH0$h_f > 0 & sH$h_f <= 0),
           pct_f_enter_hat= pct(sH0$h_f <= 0 & sH$h_f > 0),
           pct_m_enter_hat= pct(sH0$h_m <= 0 & sH$h_m > 0),
           pct_m_exit_hat = pct(sH0$h_m > 0 & sH$h_m <= 0),
           pct_f_exit_any = pct(sH0$h_f > 0 & sInf$h_f <= 0))
  message(sprintf(paste0("  CHECKS (full year) at h = T %.4f%% | participation switch 0 -> alpha-hat %.4f%%",
                         " (wife exits %.4f%%, enters %.4f%%; husband enters %.4f%%, exits %.4f%%)",
                         " | wife exits at ANY alpha %.4f%%"),
                  chk["pct_atT"], chk["pct_switch_hat"], chk["pct_f_exit_hat"],
                  chk["pct_f_enter_hat"], chk["pct_m_enter_hat"], chk["pct_m_exit_hat"],
                  chk["pct_f_exit_any"]))
  rm(sH, sH0, sInf); invisible(gc())

  data.table(YEAR = yr, n = nrow(d), kappa = kap, y_median = ymed,
             alpha = a1, f = f, F_dollars = f * ymed,
             loss = fit$value, converged = fit$convergence == 0,
             as.data.table(as.list(chk)),
             as.data.table(as.list(md))[, paste0("data_", names(md)) := as.list(md)][, .SD, .SDcols = patterns("^data_")],
             as.data.table(as.list(ms))[, paste0("model_", names(ms)) := as.list(ms)][, .SD, .SDcols = patterns("^model_")],
             as.data.table(as.list(ms0))[, paste0("nonorm_", names(ms0)) := as.list(ms0)][, .SD, .SDcols = patterns("^nonorm_")])
}))

if (nrow(out)) {
  print(out[, .(YEAR, alpha, f, loss, converged,
                data_cliff, model_cliff, data_corner, model_corner,
                data_hshare, model_hshare)])
  print(out[, .(YEAR, pct_atT, pct_switch_hat, pct_f_exit_hat, pct_m_enter_hat, pct_f_exit_any)])
  fwrite(out, dated_path(results_dir, "t3_estimates_v2_by_year.csv"))
  message("\nwrote t3_estimates_v2_by_year.csv")

  # ── LaTeX tables (booktabs tabulars, for \input{} into slides/paper) ───────
  f3 <- function(x, d = 3) formatC(x, format = "f", digits = d, big.mark = ",")

  # 1. Parameter estimates by year. alpha is in utils per dollar and falls with
  #    nominal growth -- report tau (t3-compute-tau.R) in text, alpha here only.
  write_tex_table(out[, .(Year = YEAR,
                          `$\\hat\\alpha \\times 10^{6}$` = f3(alpha * 1e6),
                          `$\\hat f$` = f3(f), `$F$ (\\$)` = f3(F_dollars, 0),
                          `$\\kappa \\times 10^{7}$` = f3(kappa * 1e7),
                          Loss = formatC(loss, format = "e", digits = 1))],
                  dated_path(results_dir, "t3_estimates_table.tex"))

  # 2. Model checks by year (percent of households, full year, at the estimates).
  write_tex_table(out[, .(Year = YEAR,
                          `At $h_i = T$` = f3(pct_atT, 2),
                          `Any switch` = f3(pct_switch_hat, 2),
                          `Wife exits` = f3(pct_f_exit_hat, 2),
                          `Husband enters` = f3(pct_m_enter_hat, 2),
                          `Wife exits, any $\\alpha$` = f3(pct_f_exit_any, 2))],
                  dated_path(results_dir, "t3_model_checks_table.tex"))

  # 3-4. Moments, pooled over the ACS years in this run (decennial years are a
  #      different sample design and are not pooled with them).
  pool <- if (any(out$YEAR > 2000)) out[YEAR > 2000] else out
  pm   <- function(pfx, nm) mean(pool[[paste0(pfx, nm)]], na.rm = TRUE)
  mrow <- function(nm, lab) data.table(Moment = lab, Data = f3(pm("data_", nm)),
                                       Model = f3(pm("model_", nm)),
                                       `No norm ($\\alpha = 0$)` = f3(pm("nonorm_", nm)))
  write_tex_table(rbind(
      mrow("cliff",    "Cliff ratio"),
      mrow("corner",   "Wife not working"),
      mrow("hshare",   "Wife's share of couple hours"),
      mrow("outearn",  "Wife out-earns husband"),
      mrow("cornerQ1", "Wife not working, husband wage Q1"),
      mrow("cornerQ3", "Wife not working, husband wage Q3"),
      mrow("cornerQ5", "Wife not working, husband wage Q5"),
      mrow("corner_m", "Husband not working")),
    dated_path(results_dir, "t3_moments_table.tex"),
    groups = list("(A) Targeted" = 2, "(B) Untargeted" = 6))

  qrow <- function(pfx, nm, lab) as.data.table(c(list(Series = lab),
            setNames(lapply(1:5, function(q) f3(pm(pfx, paste0(nm, q)))), paste0("Q", 1:5))))
  qblock <- function(nm) rbind(qrow("data_", nm, "Data"), qrow("model_", nm, "Model"),
                               qrow("nonorm_", nm, "No norm"))
  write_tex_table(rbind(qblock("cliff_Q"), qblock("overhrs_Q"), qblock("hshareDE_Q")),
    dated_path(results_dir, "t3_quintile_tests_table.tex"),
    groups = list("Cliff ratio" = 3,
                  "Dual earners: share where she works more hours" = 3,
                  "Dual earners: her share of couple hours" = 3))
}
