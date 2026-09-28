# T3 model — handoff (decisions, code state, next steps)

Paper: "Gender identity, household income composition, and the decision to work"
(draft `submission-tradwives.pdf`). Model code: `t3/`. Data: Dropbox
`mf-as-shared-ideas/tradwives/data` (see `config.yml`).

## The model (as implemented, `t3/t3-model-solver.R`)

Static, unitary household, partial equilibrium. For each couple j with inputs
(w_m, w_f, y0), choose hours (h_m, h_f):

    max  log C - k_m/2 h_m^2 - k_f/2 h_f^2 - alpha * max(w_f h_f - w_m h_m, 0)
    s.t. C = w_m h_m + w_f h_f + y0 - F (1[h_m>0] + 1[h_f>0]),  0 <= h_i <= T

- F = f * median(y_t): fixed cost per earner. Only non-convexity; the only source of
  non-participation (with F = 0 every wife works).
- Within a participation pattern the objective is strictly concave (log C concave,
  quadratic disutility strictly concave, -alpha*V concave) => unique optimum, KKT
  necessary and sufficient. Patterns compared discretely.
- Both-work KKT: k_f h_f C = w_f (1 - lam*tau), k_m h_m C = w_m (1 + lam*tau),
  tau = alpha/u'(C) = alpha*C. lam = 0 slack, 1 binding, [0,1] at the kink.
- With A = w_m^2/k_m, B = w_f^2/k_f, Y = y0 - 2F:
  - I  (slack):   C^2 - Y C - (A+B) = 0,             valid iff B < A
  - II (binding): C^2 - [Y + alpha(A-B)] C - (A+B) = 0, valid iff tau < s
  - III (kink):   C^2 - Y C - 4AB/(A+B) = 0, lam = s/tau_III, valid iff 0 <= s <= tau_III
  - s = (B-A)/(A+B) = her excess earnings share absent the norm.
  - IV he only: C^2 - (y0-F) C - A = 0; V she only: C^2 - (y0-F-alpha B) C - B = 0; VI none: C = y0.
  - Faces h_i = T: candidates IX–XVII.
- Solver evaluates the TRUE objective at every candidate and takes the argmax
  (equivalent to checking validity conditions; verified against brute-force grids).
- Interpretation: tau = share of each dollar she earns ABOVE him that the norm takes
  away. Report tau, not alpha (alpha is utils/dollar and falls with nominal growth).
  kappa is calibrated (husband's Regime-I FOC at dual-earner means:
  kappa = mean(w_m) / (Cbar * mean(h_m))), no standalone meaning.

## Decisions made (Allegra)

1. **Keep log utility (gamma = 1).** Standard; do not bend consumption utility to fit.
   The solver has an optional `gamma` argument (CRRA), default 1, unused by any
   estimation script. May be removed.
2. **T = 24 x 365 = 8,760 annual hours** (Ramey & Francis 2009), enforced as a
   constraint `T_ENDOW`, with KKT face candidates. Update Table 7 (was 4,000).
3. **y0 = capital income only (INCINVST, both spouses).** Dropped INCWELFR, INCSS,
   INCOTHER: transfers are conditional on not working (endogenous; inflated y0 for
   poor non-working households). Note: this will likely LOWER the model's Q1 corner
   share further (already too low: 0.14 vs 0.28 data).
4. **Core claim is the intensive margin:** in Q5 fewer couples are norm-bound
   (a high-earning husband is rarely out-earned), but where it binds the wife gives
   up ~2x the hours (Table A3: Q1 42% bound / 238 hrs; Q5 7% / 484 hrs), because
   tau = alpha*C rises with resources. Does not depend on fitting participation.
5. **New untargeted tests by husband's-wage quintile** (added to `moments()` in
   `t3-estimate-v2.R`): `cliff_Q{1..5}`, `overhrs_Q{1..5}` (share of dual earners
   where she works more hours than he), `hshareDE_Q{1..5}` (her share of couple
   hours, dual earners). Plus a **no-norm baseline** (alpha = 0) saved as
   `nonorm_*`. All three gradients exist mechanically at alpha = 0 (wage
   composition), so the test is DATA vs MODEL vs NO NORM, not the raw gradient.

## Code state

All of the above is committed in `d4e204b` ("solver changes"):
`t3/t3-model-solver.R`, `t3/t3-estimate-v2.R`, `ipums-model-data.R`.

## Next steps (not yet run)

1. Rebuild `data/processed/panel/model_input_households.csv` with `ipums-model-data.R`
   (needs `data/interim/ipums_bkp.sqlite` — not found in Dropbox; locate it).
2. Re-run T3: `t3-estimate-v2.R` then downstream scripts (`run-pipeline.sh`,
   T3_YEARS default = full series).
3. Checks to add/print: share of households at h_i = T (expect 0); share whose
   participation changes between alpha = 0 and alpha-hat (expect ~0 at f ≈ 0.08–0.11).
4. Report moments in three groups: (A) targeted — fit expected, not evidence;
   (B) untargeted aggregate — hours share, out-earn share; (C) untargeted by
   quintile — data / model / no-norm. One figure for (C): 3 panels, Q1–Q5.

## Open decisions

- **Targets: 5 moments (current) vs exactly identified (cliff + corner).**
  Recommended: `TARGETS <- c("cliff", "corner")` — estimates then don't depend on
  the arbitrary identity weighting; Q1/Q3/Q5 corner become untargeted tests.
  Currently the Q1/Q5 misfit is a failed over-identification test (J >> chi2(3)
  critical value 7.8), reported as if it were fit.
- Extensions discussed, not adopted: children in F (F = f0 + f1*1[child<5]);
  housing wealth (imputed rent for outright owners) in y0 as an untargeted test vs
  Table A8; alpha by county culture (2012–2020) or by cohort; hours regression
  ln h_f = ln w_f - gamma ln C (identifies the income effect; tests Frisch = 1).

## Paper corrections

1. "Regime III dominates Regime IV at any alpha" is not a theorem (the kink pays a
   second F). Correct statement: alpha can move her to the corner only if
   W_III < W_IV < W_I; zero such households at the estimated f (verify on data).
2. Write the Regime III closed form and lam = s/tau_III (replaces "analogous
   expressions").
3. "Two moments targeted" -> five moments / two parameters (or switch to two).
4. Table 7: T = 8,760 (Ramey–Francis); describe kappa calibration exactly.
5. Proposition 1: unit elasticity of tau in C is the log assumption, not a finding.
6. y0 definition: capital income only; state why transfers are excluded.
