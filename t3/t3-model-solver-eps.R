# Sourced by t3-estimate-bunching.R when the Frisch elasticity differs from 1.
# Defines the solver only; reads and writes nothing.

# =============================================================================
# T3 — solver with a FREE Frisch elasticity of hours, eps
#
# Generalises t3-model-solver.R (which fixes eps = 1) to
#     v(h) = kappa / (1 + 1/eps) * h^(1 + 1/eps)
# so the first-order conditions read
#     kappa_i h_i^(1/eps) = w_i * (C^-gamma -/+ alpha)   =>   h_i = (w_i m_i / kappa_i)^eps
# with m_m = C^-gamma + alpha (he is subsidised), m_f = C^-gamma - alpha (she is
# taxed), and the same candidate regimes as the eps = 1 solver. At eps = 1 this
# reproduces t3-model-solver.R exactly (checked in the validation block of
# t3/validate-solver-eps.R).
#
# Roots. Every regime still reduces to ONE equation in consumption C, found by
# bisection on log C (no optimiser):
#   slack regimes (I, IV, IX, XI) and the kink (III):   C = K * C^(-gamma*eps) + Z
#       -> crra_root(), taken from t3-model-solver.R with exponent gamma*eps
#   norm-binding regimes (II, V, X, XII):
#       g(C) = C - Z - cm*(C^-gamma + alpha)^eps - cf*(C^-gamma - alpha)^eps = 0
#       g is increasing in C, so the root is unique; gen_root() below.
# As in the eps = 1 solver, the TRUE objective is evaluated at every candidate
# and the maximum taken, so an invalid candidate cannot win.
#
# Not supported: a minimum-hours floor (h_min); the estimation never uses one.
# =============================================================================
source(here::here("t3", "t3-model-solver.R"))

utility_eps <- function(h_m, h_f, w_m, w_f, y0, F, alpha, k_m, k_f, alpha2 = 0, gamma = 1, eps = 1) {
  C <- w_m * h_m + w_f * h_f + y0 - F * ((h_m > 0) + (h_f > 0))
  V <- pmax(w_f * h_f - w_m * h_m, 0)
  Cs <- ifelse(is.finite(C) & C > 0, C, 1)
  p <- 1 + 1 / eps
  out <- u_fun(Cs, gamma) - k_m / p * h_m^p - k_f / p * h_f^p - alpha * V - alpha2 * (h_f > 0)
  out[!is.finite(C) | C <= 0] <- -Inf
  out[!is.finite(out)]        <- -Inf
  out
}

# Root of g(C) = C - Z - cm*pmax(C^-gamma + sm*alpha, 0)^eps - cf*pmax(C^-gamma + sf*alpha, 0)^eps
# (sm, sf = +1/-1/0 are the signs of the norm term in his and her marginal value).
gen_root <- function(Z, cm, cf, sm, sf, alpha, gamma, eps, iter = 55L) {
  n <- length(Z)
  g <- function(C) C - Z - cm * pmax(C^(-gamma) + sm * alpha, 0)^eps -
                           cf * pmax(C^(-gamma) + sf * alpha, 0)^eps
  lo <- rep(1e-4, n)
  hi <- pmax(Z, 1)
  for (k in 1:60) {                                   # double hi until g(hi) > 0
    bad <- is.finite(hi) & g(hi) <= 0
    if (!any(bad, na.rm = TRUE)) break
    hi[which(bad)] <- 2 * hi[which(bad)]
  }
  ok <- is.finite(Z) & is.finite(g(lo)) & g(lo) < 0 & is.finite(g(hi)) & g(hi) > 0
  out <- rep(NA_real_, n)
  if (!any(ok)) return(out)
  l <- log(lo[ok]); h <- log(hi[ok])
  Zo <- Z[ok]; cmo <- cm[ok]; cfo <- cf[ok]; ao <- alpha[ok]
  go <- function(C) C - Zo - cmo * pmax(C^(-gamma) + sm * ao, 0)^eps -
                             cfo * pmax(C^(-gamma) + sf * ao, 0)^eps
  for (it in seq_len(iter)) {
    mid <- 0.5 * (l + h)
    up  <- go(exp(mid)) > 0
    h   <- ifelse(up, mid, h)
    l   <- ifelse(up, l, mid)
  }
  out[ok] <- exp(0.5 * (l + h))
  out
}

solve_household_eps <- function(w_m, w_f, y0, F, alpha, k_m, k_f, alpha2 = 0,
                                T = T_ENDOW, gamma = 1, eps = 1) {
  n <- length(w_m)
  alpha  <- rep_len(alpha, n); alpha2 <- rep_len(alpha2, n); F <- rep_len(F, n)
  k_m <- rep_len(k_m, n); k_f <- rep_len(k_f, n)
  cm <- w_m^(1 + eps) / k_m^eps            # his and her "A" and "B" at general eps
  cf <- w_f^(1 + eps) / k_f^eps
  Y  <- y0 - 2 * F; Yc <- y0 - F
  Zm <- w_m * T + y0 - 2 * F; Zf <- w_f * T + y0 - 2 * F
  mu <- function(C) C^(-gamma)
  hrs <- function(w, k, m) (w * pmax(m, 0) / k)^eps
  ge  <- gamma * eps
  slack <- function(K, Z) {
    out <- rep(NA_real_, n); ok <- is.finite(K) & is.finite(Z) & K > 0
    out[ok] <- crra_root(K[ok], Z[ok], ge); out
  }
  zero <- rep(0, n)

  ncand <- 17L
  hm <- matrix(NA_real_, n, ncand); hf <- matrix(NA_real_, n, ncand)

  C1 <- slack(cm + cf, Y)                                        # I   both, slack
  hm[, 1] <- hrs(w_m, k_m, mu(C1)); hf[, 1] <- hrs(w_f, k_f, mu(C1))
  C2 <- gen_root(Y, cm, cf, +1, -1, alpha, gamma, eps)           # II  both, binding
  hm[, 2] <- hrs(w_m, k_m, mu(C2) + alpha); hf[, 2] <- hrs(w_f, k_f, mu(C2) - alpha)
  r  <- w_m / w_f                                                # III kink
  Kk <- k_m / w_m + k_f * r^(1 / eps) / w_f
  C3 <- slack(2 * w_m * (2 / Kk)^eps, Y)
  hm[, 3] <- (2 * mu(C3) / Kk)^eps; hf[, 3] <- r * hm[, 3]
  C4 <- slack(cm, Yc)                                            # IV  he only
  hm[, 4] <- hrs(w_m, k_m, mu(C4)); hf[, 4] <- 0
  C5 <- gen_root(Yc, zero, cf, 0, -1, alpha, gamma, eps)         # V   she only
  hm[, 5] <- 0; hf[, 5] <- hrs(w_f, k_f, mu(C5) - alpha)
  hm[, 6] <- 0; hf[, 6] <- 0                                     # VI  neither
  # 7, 8: minimum-hours candidates, unused (h_min = 0)
  C9  <- slack(cf, Zm)                                           # he at T, she slack
  hm[, 9] <- T;  hf[, 9] <- hrs(w_f, k_f, mu(C9))
  C10 <- gen_root(Zm, zero, cf, 0, -1, alpha, gamma, eps)        # he at T, she binding
  hm[, 10] <- T; hf[, 10] <- hrs(w_f, k_f, mu(C10) - alpha)
  C11 <- slack(cm, Zf)                                           # she at T, he slack
  hf[, 11] <- T; hm[, 11] <- hrs(w_m, k_m, mu(C11))
  C12 <- gen_root(Zf, cm, zero, +1, 0, alpha, gamma, eps)        # she at T, he binding
  hf[, 12] <- T; hm[, 12] <- hrs(w_m, k_m, mu(C12) + alpha)
  hm[, 13] <- T; hf[, 13] <- r * T                               # kink, he at T
  hf[, 14] <- T; hm[, 14] <- T / r                               # kink, she at T
  hm[, 15] <- T; hf[, 15] <- T                                   # both at T
  hm[, 16] <- T; hf[, 16] <- 0
  hm[, 17] <- 0; hf[, 17] <- T

  over <- hm > T | hf > T; over[is.na(over)] <- FALSE
  hm[over] <- NA_real_; hf[over] <- NA_real_
  hm[hm < 0 | !is.finite(hm)] <- NA_real_
  hf[hf < 0 | !is.finite(hf)] <- NA_real_

  U <- matrix(-Inf, n, ncand)
  for (j in seq_len(ncand)) {
    ok <- !is.na(hm[, j]) & !is.na(hf[, j])
    if (any(ok)) U[ok, j] <- utility_eps(hm[ok, j], hf[ok, j], w_m[ok], w_f[ok], y0[ok],
                                         F[ok], alpha[ok], k_m[ok], k_f[ok], alpha2[ok], gamma, eps)
  }
  best <- max.col(U, ties.method = "first"); idx <- cbind(seq_len(n), best)
  h_m <- hm[idx]; h_f <- hf[idx]; h_m[is.na(h_m)] <- 0; h_f[is.na(h_f)] <- 0
  list(h_m = h_m, h_f = h_f,
       C = w_m * h_m + w_f * h_f + y0 - F * ((h_m > 0) + (h_f > 0)),
       regime = best, U = U[idx])
}
