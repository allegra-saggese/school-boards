# Validation of t3-model-solver-eps.R (not part of the pipeline).
#  (1) at eps = 1 it reproduces t3-model-solver.R
#  (2) at eps = 0.5 its optimum is never worse than a brute-force grid search
suppressMessages(library(data.table))
source(here::here("t3", "t3-model-solver-eps.R"))
set.seed(42)
mk <- function(n) data.table(
  w_m = exp(rnorm(n, log(28), 0.5)), w_f = exp(rnorm(n, log(22), 0.55)),
  y0 = rexp(n, 1 / 4000) * (runif(n) < 0.6), F = 6000)

# (1) eps = 1 equivalence, with and without a norm, scalar and vector F
d <- mk(20000); kap1 <- 25 / 70000 / 2000
for (a in c(0, 4e-7, 2e-6)) for (Fv in list(6000, d$F * exp(rnorm(nrow(d), 0, .5)))) {
  o <- solve_household(d$w_m, d$w_f, d$y0, Fv, a, rep(kap1, nrow(d)), rep(kap1, nrow(d)))
  e <- solve_household_eps(d$w_m, d$w_f, d$y0, Fv, a, kap1, kap1, eps = 1)
  cat(sprintf("eps=1  alpha %.0e: max |dh_m| %.2e, max |dh_f| %.2e, utility gap (eps solver - orig) min %.1e\n",
              a, max(abs(o$h_m - e$h_m)), max(abs(o$h_f - e$h_f)), min(e$U - o$U)))
}

# (2) eps = 0.5 against brute force
eps <- 0.5; kap <- 25 * 70000^-1 / 2000^(1 / eps)
d <- mk(300); worst <- -Inf; cnt <- 0
grid <- seq(0, 6000, by = 20)
for (a in c(0, 4e-7, 2e-6)) {
  s <- solve_household_eps(d$w_m, d$w_f, d$y0, d$F, a, kap, kap, eps = eps)
  gaps <- sapply(seq_len(nrow(d)), function(i) {
    G <- CJ(h_m = grid, h_f = grid)
    ug <- utility_eps(G$h_m, G$h_f, d$w_m[i], d$w_f[i], d$y0[i], d$F[i], a, kap, kap, 0, 1, eps)
    max(ug) - s$U[i]                                  # > 0 would mean the grid beat the solver
  })
  cat(sprintf("eps=0.5 alpha %.0e: grid - solver utility: max %.2e (negative or ~0 = solver at least as good), n beaten by > 1e-4: %d\n",
              a, max(gaps), sum(gaps > 1e-4)))
}
