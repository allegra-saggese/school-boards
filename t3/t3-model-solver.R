# Sourced by every other T3 script. Defines the solver only; reads and writes
# nothing.

# =============================================================================
# T3 — closed-form solver for the static household model with an identity norm
#
# THE MODEL (static, one period, partial equilibrium)
#   max over (h_m, h_f) >= 0:
#       log(C) - kappa_m/2 * h_m^2 - kappa_f/2 * h_f^2 - alpha * V
#   s.t. C = w_m*h_m + w_f*h_f + y0 - F*(1[h_m>0] + 1[h_f>0])
#        V = max(w_f*h_f - w_m*h_m, 0)
#   with eps = 1 (Frisch), so v(h) = kappa/(1+1/eps) h^(1+1/eps) = kappa h^2 / 2.
#
# F is the goods cost of entering the market -- home production (childcare,
# cooking) must be purchased once a spouse works. Symmetric across spouses:
# the gender asymmetry in participation is a RESULT (driven by the wage gap and
# the norm), not an assumption. Making F gender-specific would leave alpha
# unidentified, since both would explain the same moment.
#
# F = f * median(y) IN THE YEAR, so it is scale-free, needs no external
# calibration, and deflates itself across the sample.
#
# WHY THERE IS NO NUMERICAL OPTIMISATION HERE
# With log utility and eps = 1 every regime reduces to a QUADRATIC in C. This
# is not value function iteration -- the model is static, there is no state and
# no continuation value, so there is no value function to iterate. Nor is it
# even the fixed-point iteration that the simultaneity tau = alpha*C might seem
# to require: substituting the FOCs into the budget constraint yields a closed
# form directly. Six candidate regimes, six quadratics, take the argmax.
#
# REGIMES (A = w_m^2/kappa_m, B = w_f^2/kappa_f, Y = y0 - 2F, Yc = y0 - F)
#   I   both work, norm slack      C^2 - Y*C - (A+B) = 0
#   II  both work, norm binding    C^2 - [Y + alpha(A-B)]*C - (A+B) = 0
#   III both work, at the kink     C^2 - Y*C - 4*w_m^2/K = 0,  K = kappa_m + kappa_f*(w_m/w_f)^2
#   IV  he works only (h_f = 0)    C^2 - Yc*C - A = 0
#   V   she works only (h_m = 0)   C^2 - (Yc - alpha*B)*C - B = 0
#   VI  neither works              C = y0
#   VII she works the MINIMUM       C^2 - Z*C - A = 0        (norm slack)
#       h_f = h_min, h_m optimal    C^2 - (Z+alpha*A)*C - A = 0  (norm binding)
#                                   Z = w_f*h_min + y0 - 2F
#   IX-XVII  one or both spouses at the time endowment h_i = T (see T_ENDOW):
#       he at T:  C^2 - Zm*C - B = 0 (slack), C^2 - (Zm - alpha*B)*C - B = 0 (binding)
#       she at T: C^2 - Zf*C - A = 0 (slack), C^2 - (Zf + alpha*A)*C - A = 0 (binding)
#       Zm = w_m*T + y0 - 2F, Zf = w_f*T + y0 - 2F; plus the determined points
#       where a face meets the kink, the other face, or the h = 0 edge.
#
# MINIMUM HOURS (h_min) -- why it is here
# Without it the norm has NO effect on participation at any alpha, because
# V = 0 at the KINK as well as at the corner: a norm-constrained wife bunches
# at equal earnings rather than withdrawing, since bunching costs her only the
# earnings ABOVE his while withdrawing costs her all of them. The kink strictly
# dominates the corner, so alpha moves hours and never participation.
#
# Restricting the choice set to h_f in {0} U [h_min, T] breaks that. The kink
# sits at h_f = (w_m/w_f)*h_m, which is LOWEST for wives with high wages
# relative to their husbands -- exactly the wives the norm binds on. For them
# the kink falls below h_min and is unreachable, so the choice becomes
# over-earn (and pay alpha*V) or withdraw. That is the channel through which
# the norm reaches the extensive margin.
#
# CAVEAT, to be stated in any write-up: the observed hours distribution shows
# NO sharp floor -- it runs smoothly to near zero, with ~8% of working wives
# under 500 annual hours. h_min is a modelling device motivated by job
# indivisibility, not a threshold visible in the data. Report sensitivity.
# =============================================================================

# TIME ENDOWMENT. h_i = T - L_i with L_i >= 0, so 0 <= h_i <= T. T is the
# full physical endowment, 24 hours x 365 days, following Ramey and Francis
# (2009, AEJ: Macro), who count personal care as leisure. It does not enter the FOCs
# (disutility is written in hours), but it bounds the feasible set, so the
# solver carries candidates on the faces h_m = T and h_f = T. Without them the
# maximum on those faces would be missing from the candidate set, and simply
# discarding points with h > T would return the wrong optimum.
T_ENDOW <- 24 * 365      # 8,760 annual hours (Ramey and Francis 2009)

# Positive root of C^2 - b*C - c = 0.
pos_root <- function(b, c) 0.5 * (b + sqrt(b * b + 4 * c))

# CONSUMPTION CURVATURE gamma. u(C) = (C^(1-gamma) - 1)/(1 - gamma), log at
# gamma = 1. gamma governs the INCOME EFFECT on hours: every FOC reads
#     kappa_i * h_i = w_i * (C^(-gamma) -/+ lambda*alpha),
# so hours fall with household consumption at elasticity gamma, and the wife
# participates iff w_f^2 > 2*kappa*F*C^gamma (approximately). gamma is therefore
# the parameter that sets how steeply non-participation rises with the
# husband's wage. It also sets the income elasticity of the norm wedge:
#     tau = alpha / u'(C) = alpha * C^gamma,   d ln tau / d ln C = gamma.
# With gamma = 1 every regime is a quadratic in C (pos_root). With gamma != 1
# every regime is  C = K*C^(-gamma) + Z  (K >= 0): the left side rises in C and
# the right side falls, so there is exactly one positive root (crra_root).
u_fun <- function(C, gamma) if (gamma == 1) log(C) else (C^(1 - gamma) - 1) / (1 - gamma)

crra_root <- function(K, Z, gamma, iter = 60L) {
  r  <- K^(1 / (1 + gamma))
  Zp <- pmax(Z, 0)
  # Bracket. hi: g(hi) >= 0 since hi - Z >= r and K*hi^-gamma <= r.
  # lo: for Z >= 0, g(max(Z, r)) <= 0; for Z < 0, g((K/(r - Z))^(1/gamma)) <= 0.
  hi <- Zp + r
  lo <- ifelse(Z >= 0, pmax(Z, r), (K / (r - Z))^(1 / gamma))
  lo <- pmin(lo, hi)
  for (it in seq_len(iter)) {                 # bisection on log C
    mid <- sqrt(lo * hi)
    up  <- (mid - K * mid^(-gamma) - Z) > 0
    hi  <- ifelse(up, mid, hi)
    lo  <- ifelse(up, lo, mid)
  }
  sqrt(lo * hi)
}

# alpha1 = RELATIVE-earnings prescription ("he should out-earn her"). Generates
#          the cliff.
# alpha2 = PARTICIPATION prescription ("wives don't work"). A flat utility cost
#          of her working at all. Set to 0 in all current estimation.
utility <- function(h_m, h_f, w_m, w_f, y0, F, alpha, k_m, k_f, alpha2 = 0, gamma = 1) {
  C <- w_m * h_m + w_f * h_f + y0 - F * ((h_m > 0) + (h_f > 0))
  V <- pmax(w_f * h_f - w_m * h_m, 0)
  Cs <- ifelse(is.finite(C) & C > 0, C, 1)    # avoid warnings; masked below
  out <- u_fun(Cs, gamma) - k_m / 2 * h_m^2 - k_f / 2 * h_f^2 - alpha * V - alpha2 * (h_f > 0)
  out[!is.finite(C) | C <= 0] <- -Inf
  out[!is.finite(out)]        <- -Inf
  out
}

# Vectorised over households. Returns h_m, h_f, C and the winning candidate.
# Each candidate: root of C = K*C^(-gamma) + Z, then hours from the FOCs.
#   kappa_m h_m = w_m (C^-g + lam*alpha),  kappa_f h_f = w_f (C^-g - lam*alpha)
solve_household <- function(w_m, w_f, y0, F, alpha, k_m, k_f, h_min = 0, alpha2 = 0,
                            T = T_ENDOW, gamma = 1) {
  n  <- length(w_m)
  alpha  <- rep_len(alpha,  n)
  alpha2 <- rep_len(alpha2, n)
  F      <- rep_len(F, n)     # F may be a scalar or one value per household
  A  <- w_m^2 / k_m
  B  <- w_f^2 / k_f
  Y  <- y0 - 2 * F
  Yc <- y0 - F
  # root of C = K*C^(-gamma) + Z; gamma = 1 is the exact quadratic
  root <- function(K, Z) {
    if (gamma == 1) return(pos_root(Z, K))
    out <- rep(NA_real_, length(K))
    ok  <- is.finite(K) & is.finite(Z) & K > 0
    out[ok] <- crra_root(K[ok], Z[ok], gamma)
    out
  }
  mu <- function(C) C^(-gamma)                # u'(C)

  ncand <- 17L
  cand_h_m <- matrix(NA_real_, n, ncand)
  cand_h_f <- matrix(NA_real_, n, ncand)

  # I -- both work, norm slack:            C = (A+B) C^-g + Y
  C1 <- root(A + B, Y)
  cand_h_m[, 1] <- w_m * mu(C1) / k_m
  cand_h_f[, 1] <- w_f * mu(C1) / k_f

  # II -- both work, norm binding:         C = (A+B) C^-g + alpha(A-B) + Y
  # tau = alpha/u'(C) = alpha C^g: proportional subsidy on his wage, tax on hers.
  C2 <- root(A + B, Y + alpha * (A - B))
  cand_h_m[, 2] <- w_m * (mu(C2) + alpha) / k_m
  cand_h_f[, 2] <- w_f * (mu(C2) - alpha) / k_f

  # III -- both work, at the kink w_f h_f = w_m h_m:
  #        C = (4 w_m^2 / K) C^-g + Y,  K = kappa_m + kappa_f (w_m/w_f)^2
  K  <- k_m + k_f * (w_m / w_f)^2
  C3 <- root(4 * w_m^2 / K, Y)
  cand_h_m[, 3] <- 2 * w_m * mu(C3) / K
  cand_h_f[, 3] <- (w_m / w_f) * cand_h_m[, 3]

  # IV -- he works only:                   C = A C^-g + (y0 - F)
  C4 <- root(A, Yc)
  cand_h_m[, 4] <- w_m * mu(C4) / k_m
  cand_h_f[, 4] <- 0

  # V -- she works only (V = w_f h_f):     C = B C^-g + (y0 - F) - alpha B
  C5 <- root(B, Yc - alpha * B)
  cand_h_m[, 5] <- 0
  cand_h_f[, 5] <- w_f * (mu(C5) - alpha) / k_f

  # VI -- neither works
  cand_h_m[, 6] <- 0
  cand_h_f[, 6] <- 0

  # VII / VIII -- she at h_min, he optimal (slack / binding). Off when h_min = 0.
  Z  <- w_f * h_min + y0 - 2 * F
  C7 <- root(A, Z)
  cand_h_m[, 7] <- w_m * mu(C7) / k_m
  cand_h_f[, 7] <- h_min
  C8 <- root(A, Z + alpha * A)
  cand_h_m[, 8] <- w_m * (mu(C8) + alpha) / k_m
  cand_h_f[, 8] <- h_min

  # IX-XVII -- faces of the time constraint h_i = T (KKT multiplier mu_i > 0).
  Zm <- w_m * T + y0 - 2 * F                 # he at T, both work
  Zf <- w_f * T + y0 - 2 * F                 # she at T, both work
  C9  <- root(B, Zm)                         # he at T, she slack
  cand_h_m[, 9]  <- T;  cand_h_f[, 9]  <- w_f * mu(C9) / k_f
  C10 <- root(B, Zm - alpha * B)             # he at T, she binding
  cand_h_m[, 10] <- T;  cand_h_f[, 10] <- w_f * (mu(C10) - alpha) / k_f
  C11 <- root(A, Zf)                         # she at T, he slack
  cand_h_f[, 11] <- T;  cand_h_m[, 11] <- w_m * mu(C11) / k_m
  C12 <- root(A, Zf + alpha * A)             # she at T, he binding
  cand_h_f[, 12] <- T;  cand_h_m[, 12] <- w_m * (mu(C12) + alpha) / k_m
  cand_h_m[, 13] <- T;  cand_h_f[, 13] <- (w_m / w_f) * T  # kink, he at T
  cand_h_f[, 14] <- T;  cand_h_m[, 14] <- (w_f / w_m) * T  # kink, she at T
  cand_h_m[, 15] <- T;  cand_h_f[, 15] <- T                # both at T
  cand_h_m[, 16] <- T;  cand_h_f[, 16] <- 0                # he at T, she out
  cand_h_m[, 17] <- 0;  cand_h_f[, 17] <- T                # she at T, he out

  # Time-endowment feasibility: h_i > T is outside the choice set; the maximum
  # on each face is supplied by IX-XVII.
  over <- cand_h_m > T | cand_h_f > T
  over[is.na(over)] <- FALSE
  cand_h_m[over] <- NA_real_
  cand_h_f[over] <- NA_real_

  # Minimum-hours feasibility: choice set {0} U [h_min, T].
  if (h_min > 0) {
    bad <- (cand_h_f > 0 & cand_h_f < h_min) | (cand_h_m > 0 & cand_h_m < h_min)
    bad[is.na(bad)] <- FALSE
    cand_h_f[bad] <- NA_real_
    cand_h_m[bad] <- NA_real_
  }

  # Non-negative hours.
  cand_h_m[cand_h_m < 0 | !is.finite(cand_h_m)] <- NA_real_
  cand_h_f[cand_h_f < 0 | !is.finite(cand_h_f)] <- NA_real_

  # Evaluate the TRUE objective at every candidate and take the maximum.
  U <- matrix(-Inf, n, ncand)
  for (j in seq_len(ncand)) {
    ok <- !is.na(cand_h_m[, j]) & !is.na(cand_h_f[, j])
    if (any(ok)) {
      U[ok, j] <- utility(cand_h_m[ok, j], cand_h_f[ok, j],
                          w_m[ok], w_f[ok], y0[ok], F[ok], alpha[ok],
                          k_m[ok], k_f[ok], alpha2[ok], gamma)
    }
  }
  best <- max.col(U, ties.method = "first")
  idx  <- cbind(seq_len(n), best)
  h_m  <- cand_h_m[idx]; h_f <- cand_h_f[idx]
  h_m[is.na(h_m)] <- 0;  h_f[is.na(h_f)] <- 0
  list(h_m = h_m, h_f = h_f,
       C = w_m * h_m + w_f * h_f + y0 - F * ((h_m > 0) + (h_f > 0)),
       regime = best, U = U[idx])
}
