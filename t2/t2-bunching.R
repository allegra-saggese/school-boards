# =============================================================================
# T2 — bunching at equal earnings, by husband's income (Saez 2010)
#
# Inputs : data/processed/panel/model_input_households.csv
#          data/processed/results/*_t3_estimates_v2_by_year.csv  (section 5 only)
# Outputs: data/processed/results/YYYY-MM-DD_t2_bunching_*.csv, *_table.tex
#          data/graphs/YYYY-MM-DD_t2_bunching_*.png
#          (each output is documented in notes/t3-figures-and-tables.md)
#
# QUESTION. Does the breadwinner norm bite harder in richer households? T3
# assumes it does, with unit elasticity: tau = alpha * C under log utility. This
# script measures the norm's bite in the data, group by group, without the
# model, and then puts the model's prediction next to it.
#
# GROUPS: quintiles of the HUSBAND's annual labour income, within year, over all
# couples in which he has earnings (HHWT-weighted). His income is the reference
# point the norm is defined against, and it is what T2 (husband-decile fixed
# effects) and BKP condition on. It is not set by HER choice. Grouping by
# HOUSEHOLD income would be self-referential: a wife who cuts her hours to stay
# below him lowers household income and moves herself into a lower group.
# Caveat: his income contains his hours, which the norm can move (in T3 it
# subsidises his work). Robustness groups by his observed hourly wage instead.
#
# RUNNING VARIABLE: the wife's share of couple labour earnings
# z = e_f / (e_f + e_m), dual earners only. Bins of width 0.01 centred on
# multiples of 0.01, so the threshold bin is [0.495, 0.505). The fit uses
# z in [0.25, 0.75].
#
# ESTIMATOR (main): Saez (2010) band estimator, half-width delta = 0.02.
#   h(0.5) = mean of the observed densities in the two adjacent bands,
#            [0.46, 0.48) below and (0.52, 0.54] above (averaged)
#   B      = observed mass in [0.48, 0.52] minus 5 * h(0.5)
#   b      = 0.01 * B / h(0.5)      normalised excess mass, in units of z
# ROBUSTNESS ESTIMATOR: Chetty, Friedman, Olsen and Pistaferri (2011) -- a
# degree-7 polynomial fitted to the bin counts over [0.25, 0.75] with the band
# excluded, plus a dummy for each bin containing a simple fraction (1/3, 2/5,
# 3/5, 2/3, ...), where reported earnings heap on round numbers. The
# counterfactual is the polynomial alone.
#
# FROM BUNCHING TO THE WEDGE. In T3 a couple bunches at equal earnings iff its
# no-norm share z0 lies in [0.5, (1 + tau)/2] (Regime III is valid for
# 0 <= s <= tau, s = 2*z0 - 1). So B = h0(0.5) * tau/2 and
#       tau_hat = 2 * b.
# b is model-free; only this step uses the model, and only its quadratic
# disutility of hours (Frisch = 1) -- not log utility. Section 5 runs the
# identical estimator on model-simulated couples, where tau is known, so the
# mapping is checked rather than assumed, and data and model are compared with
# the same statistic.
#
# INCOME ELASTICITY. eta = slope of log tau_hat on log mean household resources
# (labour + capital income) across quintiles, with year fixed effects. T3
# imposes eta = 1.
#
# ROUNDING. Reported earnings sit on a coarse grid (since 2000, 99% are
# multiples of $100 and 84-93% of $1,000), so spouses tie EXACTLY far more often
# than a smooth density implies ($50k/$50k is the most common tie). The spike
# at 0.5 therefore mixes mechanical ties with any response. The rounding-adjusted
# estimate subtracts PLACEBO ties: each wife is re-paired with a random husband
# whose earnings are within +-10% of her own husband's (same year), and the
# tie rate of these placebo couples is the tie rate rounding alone produces.
#   B_adj = B(all) - placebo tie mass,  tau_adj = 2 * 0.01 * B_adj / h(0.5).
# Excess ties can still be reporting rather than behaviour (one respondent
# reporting the same figure for both spouses; Murray-Close and Heggeness 2019),
# so tau_adj is an upper bound on the behavioural wedge.
#
# SAMPLES:
#   all       all dual earners (main, unadjusted; an upper bound)
#   round_adj all dual earners, minus placebo ties (Saez only)
#   no_exact  drops couples reporting EXACTLY equal earnings (2-5% of dual
#             earners); also drops genuine ties, so a lower bound
#   no_se     drops couples in which either spouse has self-employment income:
#             coworking spouses report equal splits (Zinovyeva and Tverdostup 2021)
#   wage_q    groups by the husband's observed hourly wage instead of income
#
# INFERENCE: Poisson bootstrap over couples (multipliers on HHWT), BUNCH_BOOT
# replications (default 200). Quintile cut-offs are held at their full-sample
# values across replications.
# =============================================================================

suppressMessages({library(data.table); library(ggplot2)})
source(here::here("_setup.R"))
source(here::here("t3", "t3-model-solver.R"))
set.seed(20261005)

panel_dir   <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
n_boot <- as.integer(Sys.getenv("BUNCH_BOOT", "200"))
delta  <- 2L                       # band half-width, in 0.01 bins
k_fit  <- 25:75                    # bins used: z in [0.25, 0.75]
poly_p <- 7L
acs    <- 2001:2024
POOLED <- 0L                       # YEAR code for "ACS 2001-2024 pooled"
spec_lab <- c(all      = "All dual earners",
              round_adj = "Rounding-adjusted (placebo ties)",
              no_exact = "Excl. exactly equal earnings",
              no_se    = "Excl. self-employed couples",
              wage_q   = "Grouped by husband's wage",
              model    = "Model (T3, simulated)")

# ── 1) Data, groups, bins ────────────────────────────────────────────────────
d <- fread(file.path(panel_dir, "model_input_households.csv"),
           select = c("YEAR", "HHWT", "f_lab", "m_lab", "f_se", "m_se", "m_w_obs",
                      "f_w", "m_w", "y0", "f_h", "m_h"), showProgress = FALSE)
if (!all(c("f_se", "m_se") %in% names(d)))
  stop("model_input_households.csv has no self-employment flags; rebuild it ",
       "with the current ipums-model-data.R")
d <- d[m_lab > 0]

# HHWT-weighted quantile groups within year; ties (earnings heap on round
# numbers) are broken at random so groups have equal weight.
wq <- function(x, w, k = 5L) {
  o <- order(x, runif(length(x)))
  g <- integer(length(x))
  g[o] <- pmin(k, 1L + as.integer(floor(k * (cumsum(w[o]) - w[o]) / sum(w))))
  g
}
d[, hq := wq(m_lab, HHWT), by = YEAR]
d[, hwq := NA_integer_]
d[!is.na(m_w_obs), hwq := wq(m_w_obs, HHWT), by = YEAR]

# Group resources for the elasticity: mean labour + capital income of ALL
# couples in the group (a group-level regressor, so not self-referential).
yb_hq  <- d[, .(ybar = weighted.mean(f_lab + m_lab + y0, HHWT)), by = .(YEAR, grp = hq)]
yb_hwq <- d[!is.na(hwq), .(ybar = weighted.mean(f_lab + m_lab + y0, HHWT)), by = .(YEAR, grp = hwq)]
yb <- rbind(yb_hq[, spec := "all"], copy(yb_hq)[, spec := "round_adj"],
            copy(yb_hq)[, spec := "no_exact"],
            copy(yb_hq)[, spec := "no_se"], yb_hwq[, spec := "wage_q"],
            copy(yb_hq)[, spec := "model"])

d[, k := as.integer(round(100 * f_lab / (f_lab + m_lab)))]
bs <- d[f_lab > 0 & k %between% range(k_fit),
        .(YEAR, HHWT, hq, hwq, k, exact = f_lab == m_lab,
          se = fcoalesce(f_se, FALSE) | fcoalesce(m_se, FALSE))]
message(sprintf("dual earners with z in [0.25, 0.75]: %s couples, %d years",
                format(nrow(bs), big.mark = ","), uniqueN(bs$YEAR)))
message(sprintf("  exactly equal earnings: %.2f%% | any self-employment: %.2f%% (weighted)",
                100 * bs[, sum(HHWT[exact]) / sum(HHWT)], 100 * bs[, sum(HHWT[se]) / sum(HHWT)]))

# Placebo ties: within each year, husbands' earnings are permuted among couples
# whose husbands earn within the same 0.2-wide log bin (+-10%), n_perm times.
# p_tie is the share of permutations in which the wife ties her placebo husband.
# Only couples whose own share falls in the band can tie (z = 0.5), so the
# placebo is computed on all dual earners and kept for the band rows of bs.
n_perm  <- 5L
plac_w  <- 0.2
dd <- d[f_lab > 0, .(YEAR, f_lab, m_lab, lbin = floor(log(m_lab) / plac_w))]
dd[, p_tie := 0]
for (r in seq_len(n_perm)) {
  dd[, p_tie := p_tie + (f_lab == m_lab[sample.int(.N)]) / n_perm, by = .(YEAR, lbin)]
}
d[f_lab > 0, p_tie := dd$p_tie]
bs[, p_tie := d[f_lab > 0 & k %between% range(k_fit), p_tie]]
rm(dd); invisible(gc())
message(sprintf("  placebo tie rate %.2f%% vs actual %.2f%% of dual earners in [0.25, 0.75]",
                100 * bs[, sum(HHWT * p_tie) / sum(HHWT)], 100 * bs[, sum(HHWT[exact]) / sum(HHWT)]))

# ── 2) Estimators ─────────────────────────────────────────────────────────────
grid <- k_fit / 100
band <- abs(k_fit - 50L) <= delta                               # 48..52
lo   <- k_fit %in% (50L - 2L * delta):(50L - delta - 1L)        # 46, 47
hi   <- k_fit %in% (50L + delta + 1L):(50L + 2L * delta)        # 53, 54
heap_k <- unique(as.integer(round(100 * c(1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 4/9,
                                          5/9, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4))))
heap_k <- setdiff(heap_k, k_fit[band])

# Chetty et al. counterfactual as a fixed linear operator on the bin counts:
# counts -> fitted polynomial (heap dummies and the band excluded). Every cell
# shares the same design, so one operator serves all cells and replications.
P  <- poly(grid, poly_p)
H  <- sapply(heap_k, function(kk) as.numeric(k_fit == kk))
X  <- cbind(1, P, H)
Xf <- X[!band, , drop = FALSE]
G  <- solve(crossprod(Xf), t(Xf))
L  <- cbind(1, P) %*% G[seq_len(poly_p + 1L), , drop = FALSE]   # bins x fit-bins

# cnt: long table (spec, YEAR, grp, k, c). Returns one row per cell and estimator.
estimate <- function(cnt) {
  W <- dcast(cnt, spec + YEAR + grp ~ k, value.var = "c", fill = 0)
  for (kk in setdiff(as.character(k_fit), names(W))) set(W, j = kk, value = 0)
  M <- as.matrix(W[, as.character(k_fit), with = FALSE])
  h_s <- (rowMeans(M[, lo, drop = FALSE]) + rowMeans(M[, hi, drop = FALSE])) / 2
  B_s <- rowSums(M[, band, drop = FALSE]) - sum(band) * h_s
  Ch  <- M[, !band, drop = FALSE] %*% t(L)
  h_c <- Ch[, k_fit == 50L]
  B_c <- rowSums(M[, band, drop = FALSE] - Ch[, band, drop = FALSE])
  id  <- W[, .(spec, YEAR, grp)]
  out <- rbind(copy(id)[, `:=`(estimator = "saez",   B = B_s, h = h_s)],
               copy(id)[, `:=`(estimator = "chetty", B = B_c, h = h_c)])
  out[, `:=`(b = 0.01 * B / h, tau = 2 * 0.01 * B / h)]
  out[!(spec == "round_adj" & estimator == "chetty")]
}

# Weighted bin counts for every sample, per year and pooled over the ACS.
count_bins <- function() {
  a <- bs[, .(all = sum(w), no_exact = sum(w[!exact]), no_se = sum(w[!se])),
          by = .(YEAR, grp = hq, k)]
  a <- melt(a, id.vars = c("YEAR", "grp", "k"), variable.name = "spec",
            value.name = "c", variable.factor = FALSE)
  b <- bs[!is.na(hwq), .(c = sum(w)), by = .(YEAR, grp = hwq, k)][, spec := "wage_q"]
  # round_adj: the all-sample counts with the placebo tie mass removed from the
  # threshold bin (placebo ties can only occur at k = 50).
  pl <- bs[k == 50L, .(pl = sum(w * p_tie)), by = .(YEAR, grp = hq)]
  ra <- merge(a[spec == "all"], pl, by = c("YEAR", "grp"), all.x = TRUE)
  ra[k == 50L, c := c - fcoalesce(pl, 0)][, `:=`(pl = NULL, spec = "round_adj")]
  out <- rbind(a, ra, b, use.names = TRUE)
  rbind(out, out[YEAR %in% acs, .(YEAR = POOLED, c = sum(c)), by = .(spec, grp, k)],
        use.names = TRUE)
}

# eta: log tau_hat on log group resources with year fixed effects, per-year
# cells only. Cells with tau_hat <= 0 cannot be logged and are dropped (counted).
eta_fit <- function(est) {
  e <- merge(est[YEAR != POOLED], yb, by = c("spec", "YEAR", "grp"))
  cnt <- e[, .(n_cells = sum(tau > 0), n_dropped = sum(tau <= 0)), by = .(spec, estimator)]
  fit <- e[tau > 0, .(eta = if (.N > uniqueN(YEAR) + 1L)
                              coef(lm(log(tau) ~ factor(YEAR) + log(ybar)))[["log(ybar)"]]
                            else NA_real_), by = .(spec, estimator)]
  merge(cnt, fit, by = c("spec", "estimator"), all.x = TRUE)
}

# ── 3) Point estimates ───────────────────────────────────────────────────────
bs[, w := as.numeric(HHWT)]
pt  <- estimate(count_bins())
eta <- eta_fit(pt)

# ── 4) Bootstrap ─────────────────────────────────────────────────────────────
message("bootstrap: ", n_boot, " replications ...")
b_est <- vector("list", n_boot); b_eta <- vector("list", n_boot)
for (r in seq_len(n_boot)) {
  bs[, w := as.numeric(HHWT) * rpois(.N, 1)]
  e <- estimate(count_bins())
  b_est[[r]] <- e[, .(spec, estimator, YEAR, grp, tau)]
  b_eta[[r]] <- eta_fit(e)[, .(spec, estimator, eta)]
  if (r %% 25L == 0L) message("  ", r)
}
bs[, w := as.numeric(HHWT)]
se_tau <- rbindlist(b_est)[, .(se = sd(tau)), by = .(spec, estimator, YEAR, grp)]
se_eta <- rbindlist(b_eta)[, .(se = sd(eta, na.rm = TRUE)), by = .(spec, estimator)]
pt  <- merge(pt,  se_tau, by = c("spec", "estimator", "YEAR", "grp"))
eta <- merge(eta, se_eta, by = c("spec", "estimator"))

# ── 5) The model's counterpart: same estimator on T3-simulated couples ──────
# Each year solved at its fitted (alpha, f, kappa) for the same couples, grouped
# by the same DATA-defined husband's-income quintile. tau_true is the model's
# own wedge alpha * C among couples at the kink, i.e. the bunchers.
est3 <- read_newest(results_dir, "t3_estimates_v2_by_year.csv$")
d[, t3ok := is.finite(f_w) & is.finite(m_w) & f_w > 0 & m_w > 0 & is.finite(y0) &
            is.finite(f_h) & is.finite(m_h)]
sim <- rbindlist(lapply(est3$YEAR, function(yr) {
  e <- est3[YEAR == yr]; s <- d[YEAR == yr & t3ok == TRUE]
  kp <- rep(e$kappa, nrow(s))
  so <- solve_household(s$m_w, s$f_w, s$y0, e$F_dollars, e$alpha, kp, kp)
  data.table(YEAR = yr, HHWT = s$HHWT, grp = s$hq,
             ef = s$f_w * so$h_f, em = s$m_w * so$h_m,
             kink = so$regime %in% c(3L, 13L, 14L), C = so$C, alpha = e$alpha)
}))
sim[, k := fifelse(ef > 0 & em > 0, as.integer(round(100 * ef / (ef + em))), NA_integer_)]
sc <- sim[k %between% range(k_fit), .(c = sum(HHWT)), by = .(YEAR, grp, k)]
sc <- rbind(sc, sc[YEAR %in% acs, .(YEAR = POOLED, c = sum(c)), by = .(grp, k)],
            use.names = TRUE)[, spec := "model"]
m_est <- estimate(sc)[, se := NA_real_]
m_eta <- eta_fit(m_est)[, se := NA_real_]
tau_true <- rbind(
  sim[kink == TRUE, .(tau_true = weighted.mean(alpha * C, HHWT)), by = .(YEAR, grp)],
  sim[kink == TRUE & YEAR %in% acs, .(YEAR = POOLED, tau_true = weighted.mean(alpha * C, HHWT)),
      by = grp])
rm(sim); invisible(gc())

res     <- rbind(pt, m_est, use.names = TRUE)
res     <- merge(res, tau_true[, spec := "model"], by = c("spec", "YEAR", "grp"), all.x = TRUE)
eta_all <- rbind(eta, m_eta, use.names = TRUE)

# ── 6) Console summary ───────────────────────────────────────────────────────
f3 <- function(x, dg = 3) formatC(x, format = "f", digits = dg, big.mark = ",")
cat("\n=== tau_hat = 2b by husband's-income quintile, ACS 2001-2024 pooled (Saez) ===\n")
show <- dcast(res[YEAR == POOLED & estimator == "saez"], grp ~ spec, value.var = "tau")
print(show[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 4) else x)])
cat("\nmodel's own wedge among bunchers (tau_true), pooled:\n")
print(tau_true[YEAR == POOLED][order(grp), .(grp, tau_true = round(tau_true, 4))])
cat("\n=== income elasticity eta (year FE; T3 imposes 1) ===\n")
print(eta_all[, .(spec, estimator, eta = round(eta, 3), se = round(se, 3), n_cells, n_dropped)])

# ── 7) Outputs: CSV + LaTeX ─────────────────────────────────────────────────
fwrite(res[, YEAR_label := fifelse(YEAR == POOLED, "ACS 2001-2024", as.character(YEAR))],
       dated_path(results_dir, "t2_bunching_estimates.csv"))
fwrite(eta_all, dated_path(results_dir, "t2_bunching_eta.csv"))

cell <- function(x, s) fifelse(is.na(s), f3(x), sprintf("%s (%s)", f3(x), f3(s)))
pool <- res[YEAR == POOLED]
main_tab <- Reduce(function(x, y) merge(x, y, by = "grp"), list(
  pool[spec == "all"       & estimator == "saez", .(grp, tau = cell(tau, se))],
  pool[spec == "round_adj" & estimator == "saez", .(grp, tau_r = cell(tau, se))],
  pool[spec == "no_exact"  & estimator == "saez", .(grp, tau_x = cell(tau, se))],
  pool[spec == "model"     & estimator == "saez", .(grp, tau_m = f3(tau), tau_t = f3(tau_true))]))
write_tex_table(main_tab[order(grp), .(`Husband's income quintile` = paste0("Q", grp),
                                       `Data, all` = tau, `Data, rounding-adj.` = tau_r,
                                       `Data, excl.\\ ties` = tau_x,
                                       `Model, same estimator` = tau_m, `Model, true $\\tau$` = tau_t)],
                dated_path(results_dir, "t2_bunching_main_table.tex"))

rob_cols <- list(c("all", "saez"), c("round_adj", "saez"), c("no_exact", "saez"),
                 c("no_se", "saez"), c("wage_q", "saez"), c("all", "chetty"),
                 c("model", "saez"))
rob_names <- c("All", "Rounding-adj.", "Excl.\\ ties", "Excl.\\ self-emp.",
               "Husband's wage", "Polynomial", "Model")
rob <- data.table(` ` = c(paste0("Q", 1:5), "$\\eta$"))
for (i in seq_along(rob_cols)) {
  sp <- rob_cols[[i]][1]; es <- rob_cols[[i]][2]
  q  <- pool[spec == sp & estimator == es][order(grp)]
  et <- eta_all[spec == sp & estimator == es]
  # eta is not reported where most cells are non-positive (it would rest on a
  # small, selected subset): the excluded-ties sample.
  et_cell <- if (et$n_dropped > et$n_cells) "--" else cell(et$eta, et$se)
  rob[, (rob_names[i]) := c(cell(q$tau, q$se), et_cell)]
}
write_tex_table(rob, dated_path(results_dir, "t2_bunching_robustness_table.tex"),
                groups = list("$\\hat\\tau$, ACS 2001--2024 pooled" = 5,
                              "Income elasticity (year FE, per-year cells)" = 1),
                raw_cols = " ")

# ── 8) Figures (title, axes, legend only; see the notes document) ───────────
base_theme <- theme_minimal(base_size = 13) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "top", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))
qlab <- function(g) factor(paste0("Q", g), levels = paste0("Q", 1:5),
                           labels = c("Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)"))

# 8a. density and counterfactual, pooled ACS, main sample
save_plot("t2_bunching_density_by_quintile.png", {
  cn <- count_bins()[spec == "all" & YEAR == POOLED]
  W  <- dcast(cn, grp ~ k, value.var = "c", fill = 0)
  M  <- as.matrix(W[, as.character(k_fit), with = FALSE])
  Ch <- M[, !band, drop = FALSE] %*% t(L)
  tot <- rowSums(M)
  pd <- rbind(
    data.table(grp = rep(W$grp, each = length(k_fit)), z = rep(grid, nrow(W)),
               v = as.vector(t(100 * M / tot)), src = "Observed"),
    data.table(grp = rep(W$grp, each = length(k_fit)), z = rep(grid, nrow(W)),
               v = as.vector(t(100 * Ch / tot)), src = "Counterfactual (polynomial)"))
  pd[, q := qlab(grp)]
  print(ggplot(pd, aes(z, v, colour = src, linetype = src)) +
    annotate("rect", xmin = 0.475, xmax = 0.525, ymin = -Inf, ymax = Inf,
             fill = "grey85", alpha = 0.6) +
    geom_line(linewidth = 0.7) +
    facet_wrap(~q, nrow = 1, scales = "fixed") +
    scale_colour_manual(values = c(Observed = "#111111", `Counterfactual (polynomial)` = "#B2182B")) +
    scale_linetype_manual(values = c(Observed = "solid", `Counterfactual (polynomial)` = "22")) +
    scale_x_continuous(breaks = c(0.3, 0.5, 0.7)) +
    labs(title = "Wife's share of couple earnings, by husband's income quintile",
         x = "Wife's share of couple labor earnings", y = "% of couples per 0.01 bin",
         colour = NULL, linetype = NULL) +
    base_theme)
}, width = 2800, height = 1000)

# 8b. tau_hat by quintile: data vs model, pooled ACS
save_plot("t2_bunching_tau_by_quintile.png", {
  pd <- rbind(
    pool[spec == "all" & estimator == "saez", .(grp, v = tau, se, s = "Data")],
    pool[spec == "round_adj" & estimator == "saez", .(grp, v = tau, se, s = "Data, rounding-adjusted")],
    pool[spec == "no_exact" & estimator == "saez", .(grp, v = tau, se, s = "Data, excl. exact ties")],
    pool[spec == "model" & estimator == "saez", .(grp, v = tau, se = NA_real_, s = "Model, same estimator")],
    pool[spec == "model" & estimator == "saez", .(grp, v = tau_true, se = NA_real_, s = "Model, true wedge of bunchers")])
  pd[, s := factor(s, levels = unique(s))]
  print(ggplot(pd, aes(grp, v, colour = s, shape = s)) +
    geom_hline(yintercept = 0, colour = "grey60") +
    geom_line(linewidth = 0.8) + geom_point(size = 2.6) +
    geom_errorbar(aes(ymin = v - 1.96 * se, ymax = v + 1.96 * se), width = 0.12, na.rm = TRUE) +
    scale_colour_manual(values = c("#111111", "#4D4D4D", "#A6A6A6", "#B2182B", "#FB6A4A")) +
    scale_x_continuous(breaks = 1:5, labels = paste0("Q", 1:5)) +
    labs(title = "Norm wedge implied by bunching, by husband's income",
         x = "Husband's labor-income quintile", y = "tau-hat = 2 x normalized excess mass",
         colour = NULL, shape = NULL) +
    base_theme + theme(legend.direction = "vertical"))
}, width = 2000, height = 1350)

# 8c. tau_hat over time, Q1 / Q3 / Q5, main sample
save_plot("t2_bunching_tau_over_time.png", {
  pd <- res[spec == "round_adj" & estimator == "saez" & YEAR != POOLED & grp %in% c(1L, 3L, 5L)]
  pd[, `:=`(q = qlab(grp), era = fifelse(YEAR %in% c(1980L, 1990L, 2000L), "Decennial census", "ACS"))]
  print(ggplot(pd, aes(YEAR, tau, colour = q)) +
    geom_ribbon(aes(ymin = tau - 1.96 * se, ymax = tau + 1.96 * se, fill = q),
                alpha = 0.15, colour = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(aes(shape = era), size = 2, stroke = 0.9, fill = "white") +
    scale_shape_manual(values = c("Decennial census" = 21, ACS = 19)) +
    scale_colour_manual(values = c("#6BAED6", "#2171B5", "#08306B")) +
    scale_fill_manual(values = c("#6BAED6", "#2171B5", "#08306B")) +
    labs(title = "Norm wedge implied by bunching over time",
         x = "Year", y = "tau-hat = 2 x normalized excess mass",
         colour = "Husband's income", fill = "Husband's income", shape = "Sample") +
    base_theme)
}, width = 2200, height = 1250)

message("\nwrote t2_bunching_* (2 CSV, 2 .tex, 3 figures)")
