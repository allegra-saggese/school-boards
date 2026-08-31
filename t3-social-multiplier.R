# =============================================================================
# T3 — the SOCIAL ENFORCEMENT term g(delta_bar), explored on random draws
#
# The slides specify   alpha_eff = alpha(P_j) * g(delta_bar)
# where delta_bar is compliance in the household's reference group and g is a
# social multiplier. Everything estimated so far has been alpha_eff -- the
# PRODUCT -- with g implicitly folded in. This script adds g explicitly.
#
# WHAT g CHANGES CONCEPTUALLY
# Without g, households solve in isolation. With g, the norm's strength depends
# on how many others comply, and their compliance depends on its strength. The
# model is no longer a collection of independent problems: it must be solved as
# a FIXED POINT in aggregate behaviour.
#
#     find delta* such that  delta( alpha * g(delta*) ) = delta*
#
# Compliance is defined as the share of couples in which the husband's earnings
# are at least his wife's -- i.e. the prescription is satisfied.
#
#     g(delta) = (delta / delta_ref)^gamma
#
# gamma = 0 recovers the no-multiplier model exactly. gamma > 0 means the norm
# is enforced more strongly where more people already comply.
#
# WHY THIS IS RUN ON RANDOM DRAWS FIRST
# On real data alpha and gamma are not separately identified from one
# cross-section -- Manski's reflection problem: everyone in a reference group
# faces the same delta_bar, so the group mean is collinear with group
# membership. Before taking that to data it is worth seeing what the mechanism
# DOES: how much it amplifies, and whether it produces multiple equilibria.
# Random draws make the fundamentals controllable, so any multiplicity found is
# the mechanism rather than a feature of the data.
# =============================================================================

library(data.table)
source("t3-model-solver.R")
source("functions.R")
source("R/paths.R")
set.seed(20260830)

results_dir <- data_path("processed", "results")
ensure_dir(results_dir)

# ── draw a synthetic population ─────────────────────────────────────────────
draw_population <- function(n = 40000, kbar = 1.25e-7, sigma_k = 0.5,
                            rho = 0.4) {
  # Correlated wages: assortative matching. rho is the correlation in logs.
  z1 <- rnorm(n); z2 <- rho * z1 + sqrt(1 - rho^2) * rnorm(n)
  w_m <- exp(log(22) + 0.45 * z1)
  w_f <- exp(log(16) + 0.45 * z2)     # mean wage gap built in, as in the data
  data.table(w_m = w_m, w_f = w_f,
             y0  = pmax(rnorm(n, 3000, 4000), 0),
             k_m = kbar * exp(sigma_k * rnorm(n) - sigma_k^2/2),
             k_f = kbar * exp(sigma_k * rnorm(n) - sigma_k^2/2),
             wt  = 1)
}

compliance <- function(s, P) {
  e_m <- P$w_m * s$h_m; e_f <- P$w_f * s$h_f
  sum(P$wt[e_m >= e_f]) / sum(P$wt)
}

# ── solve the fixed point in delta ──────────────────────────────────────────
# Returns the equilibrium, plus the whole iteration path so non-convergence or
# oscillation is visible rather than hidden.
solve_equilibrium <- function(P, alpha, gamma, F, delta_ref = 0.75,
                              delta0 = 0.5, tol = 1e-5, maxit = 60) {
  d <- delta0; path <- numeric(0)
  for (i in seq_len(maxit)) {
    a_eff <- alpha * (d / delta_ref)^gamma
    s  <- solve_household(P$w_m, P$w_f, P$y0, F, a_eff, P$k_m, P$k_f)
    d1 <- compliance(s, P)
    path <- c(path, d1)
    # damped update: raw iteration can oscillate when gamma is large
    d_new <- 0.5 * d + 0.5 * d1
    if (abs(d_new - d) < tol) { d <- d_new; break }
    d <- d_new
  }
  s <- solve_household(P$w_m, P$w_f, P$y0, F,
                       alpha * (d / delta_ref)^gamma, P$k_m, P$k_f)
  list(delta = d, iters = i, converged = i < maxit, path = path, s = s,
       a_eff = alpha * (d / delta_ref)^gamma)
}

P <- draw_population()
F <- 9000

message("=== 1. HOW MUCH DOES g AMPLIFY? ===")
message("Same fundamentals, same alpha; only the multiplier gamma changes.\n")
cat(sprintf("%-8s %10s %10s %10s %10s %10s\n",
            "gamma","delta*","alpha_eff","amplif.","% corner","mean h_f"))
base <- NULL
for (gam in c(0, 0.5, 1, 2, 4)) {
  e <- solve_equilibrium(P, alpha = 4e-6, gamma = gam, F = F)
  if (gam == 0) base <- e$a_eff
  cat(sprintf("%-8.1f %10.4f %10.3e %9.2fx %9.1f%% %10.0f\n",
      gam, e$delta, e$a_eff, e$a_eff/base,
      100*weighted.mean(e$s$h_f <= 0, P$wt), weighted.mean(e$s$h_f, P$wt)))
}

message("\n=== 2. ARE THERE MULTIPLE EQUILIBRIA? ===")
message("Same parameters, different starting points. If the mechanism can")
message("sustain two social states, they will separate here.\n")
cat(sprintf("%-8s %14s %14s %14s\n","gamma","from d0=0.20","from d0=0.50","from d0=0.95"))
for (gam in c(0, 1, 2, 4, 8)) {
  ds <- vapply(c(0.20, 0.50, 0.95), function(d0)
        solve_equilibrium(P, 4e-6, gam, F, delta0 = d0)$delta, numeric(1))
  flag <- if (diff(range(ds)) > 1e-3) "  <-- MULTIPLE" else ""
  cat(sprintf("%-8.1f %14.4f %14.4f %14.4f%s\n", gam, ds[1], ds[2], ds[3], flag))
}

message("\n=== 3. DOES g CHANGE THE CROSS-SECTIONAL GRADIENT? ===")
message("The dimension where the model currently fails on real data.")
message("Corner share by husband's-wage quintile:\n")
qg <- as.integer(cut(P$w_m, quantile(P$w_m, seq(0,1,.2)), labels=1:5, include.lowest=TRUE))
cat(sprintf("%-8s %8s %8s %8s %8s %8s %12s\n","gamma","Q1","Q2","Q3","Q4","Q5","Q5-Q1"))
for (gam in c(0, 1, 2, 4)) {
  e <- solve_equilibrium(P, 4e-6, gam, F)
  cs <- vapply(1:5, function(q) 100*mean(e$s$h_f[qg==q] <= 0), numeric(1))
  cat(sprintf("%-8.1f %8.1f %8.1f %8.1f %8.1f %8.1f %12.1f\n", gam, cs[1],cs[2],cs[3],cs[4],cs[5], cs[5]-cs[1]))
}
message("\nFor reference, the real 2003 data: Q1 26.2  Q3 22.7  Q5 30.1  (Q5-Q1 = +3.9)")

# ── 4. GROUP-SPECIFIC delta_bar ─────────────────────────────────────────────
# Section 3 used a single national delta_bar, which scales the norm uniformly
# and therefore cannot move a cross-section. The reference group that could is
# an income/education group: delta_bar computed WITHIN group, so each group's
# norm strength responds to its own compliance.
#
# Prediction before running: this makes the gradient WORSE. Compliance rises
# with the husband's wage (assortative matching plus the wage gap mean
# high-earning husbands out-earn their wives easily), so a multiplier that
# strengthens the norm where compliance is high strengthens it at the TOP --
# exactly where the model already puts too many corners.
message("\n=== 4. GROUP-SPECIFIC delta_bar (income groups) ===")
message("Reference group = husband's-wage quintile. Each group's norm responds")
message("to its OWN compliance rate.\n")

solve_eq_grouped <- function(P, grp, alpha, gamma, F, delta_ref = 0.75,
                             tol = 1e-5, maxit = 60) {
  G <- sort(unique(grp)); d <- setNames(rep(0.5, length(G)), G)
  for (i in seq_len(maxit)) {
    a_eff <- alpha * (d[as.character(grp)] / delta_ref)^gamma
    s  <- solve_household(P$w_m, P$w_f, P$y0, F, a_eff, P$k_m, P$k_f)
    e_m <- P$w_m * s$h_m; e_f <- P$w_f * s$h_f
    d1 <- vapply(G, function(g) mean(e_m[grp==g] >= e_f[grp==g]), numeric(1))
    if (max(abs(0.5*d + 0.5*d1 - d)) < tol) { d <- 0.5*d + 0.5*d1; break }
    d <- 0.5*d + 0.5*d1
  }
  a_eff <- alpha * (d[as.character(grp)] / delta_ref)^gamma
  list(delta = d, s = solve_household(P$w_m, P$w_f, P$y0, F, a_eff, P$k_m, P$k_f))
}

cat(sprintf("%-8s %8s %8s %8s %8s %8s %12s\n","gamma","Q1","Q2","Q3","Q4","Q5","Q5-Q1"))
for (gam in c(0, 1, 2, 4)) {
  e  <- solve_eq_grouped(P, qg, 4e-6, gam, F)
  cs <- vapply(1:5, function(q) 100*mean(e$s$h_f[qg==q] <= 0), numeric(1))
  cat(sprintf("%-8.1f %8.1f %8.1f %8.1f %8.1f %8.1f %12.1f\n",
      gam, cs[1],cs[2],cs[3],cs[4],cs[5], cs[5]-cs[1]))
}
cat("\ncompliance by group at gamma = 2 (is it really higher at the top?):\n")
e <- solve_eq_grouped(P, qg, 4e-6, 2, F)
cat("  ", paste(sprintf("Q%d %.3f", 1:5, e$delta), collapse = "  "), "\n")
cat("\nReal 2003 data: Q1 26.2  Q3 22.7  Q5 30.1  (Q5-Q1 = +3.9)\n")
