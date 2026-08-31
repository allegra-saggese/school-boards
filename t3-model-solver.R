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

# Positive root of C^2 - b*C - c = 0.
pos_root <- function(b, c) 0.5 * (b + sqrt(b * b + 4 * c))

# alpha1 = RELATIVE-earnings prescription ("he should out-earn her"). Generates
#          the cliff. Provably cannot generate participation effects: the excess
#          it penalises can always be zeroed by cutting hours to the kink, which
#          is strictly cheaper than withdrawing (she keeps w_f*h_f = w_m*h_m).
# alpha2 = PARTICIPATION prescription ("wives don't work"). A flat utility cost
#          of her working at all, so bunching does NOT avoid it. This is the only
#          way the norm reaches the extensive margin.
# alpha2 is separable from F: F's burden is F/C and FALLS with consumption,
# while alpha2 is flat in utils, so they differ in how the corner varies with
# household resources.
utility <- function(h_m, h_f, w_m, w_f, y0, F, alpha, k_m, k_f, alpha2 = 0) {
  C <- w_m * h_m + w_f * h_f + y0 - F * ((h_m > 0) + (h_f > 0))
  V <- pmax(w_f * h_f - w_m * h_m, 0)
  out <- log(C) - k_m / 2 * h_m^2 - k_f / 2 * h_f^2 - alpha * V - alpha2 * (h_f > 0)
  out[!is.finite(C) | C <= 0] <- -Inf
  out[!is.finite(out)]        <- -Inf
  out
}

# Vectorised over households. Returns h_m, h_f, C and the winning regime.
solve_household <- function(w_m, w_f, y0, F, alpha, k_m, k_f, h_min = 0, alpha2 = 0) {
  n  <- length(w_m)
  # alpha and alpha2 may be per-household (e.g. a group-specific social
  # multiplier, or alpha(P_j) heterogeneity). Recycle to length n so the
  # subsetting below is valid for both the scalar and the vector case.
  alpha  <- rep_len(alpha,  n)
  alpha2 <- rep_len(alpha2, n)
  A  <- w_m^2 / k_m
  B  <- w_f^2 / k_f
  Y  <- y0 - 2 * F
  Yc <- y0 - F

  ncand <- 8L
  cand_h_m <- matrix(NA_real_, n, ncand)
  cand_h_f <- matrix(NA_real_, n, ncand)

  # I -- interior, norm slack
  C1 <- pos_root(Y, A + B)
  cand_h_m[, 1] <- w_m / (k_m * C1)
  cand_h_f[, 1] <- w_f / (k_f * C1)

  # II -- interior, norm binding. tau = alpha*C is a proportional subsidy on his
  # hours and an equal proportional tax on hers. tau >= 1 would mean a tax of
  # 100%+ on her earnings, at which point the corner dominates.
  C2  <- pos_root(Y + alpha * (A - B), A + B)
  tau <- alpha * C2
  cand_h_m[, 2] <- w_m * (1 + tau) / (k_m * C2)
  cand_h_f[, 2] <- w_f * (1 - tau) / (k_f * C2)

  # III -- exactly at the kink, w_f*h_f = w_m*h_m. This is the bunching regime.
  K  <- k_m + k_f * (w_m / w_f)^2
  C3 <- pos_root(Y, 4 * w_m^2 / K)
  cand_h_m[, 3] <- 2 * w_m / (K * C3)
  cand_h_f[, 3] <- (w_m / w_f) * cand_h_m[, 3]

  # IV -- he works only
  C4 <- pos_root(Yc, A)
  cand_h_m[, 4] <- w_m / (k_m * C4)
  cand_h_f[, 4] <- 0

  # V -- she works only (norm fully binding, V = w_f*h_f)
  C5 <- pos_root(Yc - alpha * B, B)
  cand_h_m[, 5] <- 0
  cand_h_f[, 5] <- w_f * (1 - alpha * C5) / (k_f * C5)

  # VI -- neither works
  cand_h_m[, 6] <- 0
  cand_h_f[, 6] <- 0

  # VII / VIII -- she works exactly the minimum, h_m optimal. Only relevant
  # when h_min > 0; these are the candidates that let a norm-constrained wife
  # with an unreachable kink stay in the market at reduced hours.
  Z  <- w_f * h_min + y0 - 2 * F
  C7 <- pos_root(Z, A)                       # (a) norm slack
  cand_h_m[, 7] <- w_m / (k_m * C7)
  cand_h_f[, 7] <- h_min
  C8 <- pos_root(Z + alpha * A, A)           # (b) norm binding on him
  cand_h_m[, 8] <- w_m * (1 + alpha * C8) / (k_m * C8)
  cand_h_f[, 8] <- h_min

  # MINIMUM-HOURS FEASIBILITY. The choice set is {0} U [h_min, T] for each
  # spouse, so any candidate landing strictly inside (0, h_min) is infeasible
  # and is dropped; the argmax then falls to h_min or to 0 on its own.
  if (h_min > 0) {
    bad_f <- cand_h_f > 0 & cand_h_f < h_min
    bad_m <- cand_h_m > 0 & cand_h_m < h_min
    cand_h_f[bad_f | bad_m] <- NA_real_
    cand_h_m[bad_f | bad_m] <- NA_real_
  }

  # Feasibility: no negative hours. Infeasible candidates get -Inf utility.
  cand_h_m[cand_h_m < 0 | !is.finite(cand_h_m)] <- NA_real_
  cand_h_f[cand_h_f < 0 | !is.finite(cand_h_f)] <- NA_real_

  U <- matrix(-Inf, n, ncand)
  for (j in seq_len(ncand)) {
    ok <- !is.na(cand_h_m[, j]) & !is.na(cand_h_f[, j])
    if (any(ok)) {
      U[ok, j] <- utility(cand_h_m[ok, j], cand_h_f[ok, j],
                          w_m[ok], w_f[ok], y0[ok], F, alpha[ok],
                          k_m[ok], k_f[ok], alpha2[ok])
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
