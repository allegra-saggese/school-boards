# T3 figures and tables — what each one shows

Companion to the T3 figures and LaTeX tables. The figures carry only a title,
axis labels and a legend; everything needed to read them is here, keyed to the
output file. Update this file whenever a figure or table changes.

**Run documented:** 2026-09-28 (files prefixed `2026-09-28_`).
**Data root:** `~/Dropbox/mf-as-shared-ideas/tradwives/data/` (paths below are
relative to it). Figures are in `graphs/`, tables in `processed/results/`.
**Pipeline:** `ipums-bkp-build-database.R` (branch `rebuild-wks-supp`) →
`ipums-model-data.R` → `t3/t3-estimate-v2.R` → `t3-compute-tau.R`,
`t3-aggregate-distortion.R`, `t3-comparative-statics.R` → `t3-figures.R`.

## Conventions used throughout

- **Sample.** Married opposite-sex couples, both spouses aged 25–64, IPUMS USA:
  the 1980, 1990 and 2000 decennial 5% samples plus ACS 1-year 2001–2024;
  16,894,715 couples in total. Household weights (HHWT).
- **Decennial vs ACS.** 1980, 1990, 2000 are a different sample design from the
  2001–2024 ACS. Where a figure distinguishes them, decennial points are hollow.
  They are never pooled with ACS years.
- **Estimation.** Two parameters per year, alpha (norm) and f (fixed cost per
  earner, F = f × median household income that year), exactly identified by two
  moments: the cliff ratio and the wife's corner share. Everything else is
  **untargeted**. kappa is calibrated from the husband's first-order condition at
  dual-earner means. T = 8,760 annual hours. y0 = capital income (INCINVST) only.
- **Cliff ratio.** Weighted mass of couples with wife's earnings share in
  [0.40, 0.48) divided by mass in (0.52, 0.60]. A ratio above 1 means missing mass
  just above equal earnings. The ±0.02 donut excludes the kink itself.
- **tau.** tau = alpha × C, the implicit proportional tax the norm places on
  each dollar she earns above him (and the equal subsidy on his). Reported
  instead of alpha, which is in utils per dollar and falls with nominal growth.
- **No-norm baseline.** The same model, same F and kappa, with alpha = 0. Used to
  separate what the norm does from what wage composition does mechanically.
- **Husband's wage quintile.** Fixed on the data within each year, so model and
  data are compared within identical cells.

---

## Figures

### 1. `graphs/2026-09-28_t3_tau_over_time.png` (+ `.pdf`)

**Shows.** The norm wedge tau by year, 1980–2024, two series:
*all households* (alpha × mean model consumption) and *households the norm binds
on* (alpha × mean consumption among couples in regime II, both working with the
norm binding, or regime III, at the kink).

**How it is computed.** `t3-compute-tau.R`: one model solve per year at the
fitted (alpha, f), full sample. tau is linear in C, so the mean wedge is exactly
alpha × mean(C).

**How to read.** A tau of 0.20 means the norm acts like a 20% tax on the wife's
marginal earnings above her husband's. The binding series is the one to quote,
because averaging over households the norm never touches dilutes the wedge.

**Key numbers.** tau (binding) 0.262 (1980) → 0.166 (2024), **−37%**.
tau (all) 0.314 → 0.179, −43%. alpha itself falls 90%, most of which is nominal
income growth. The decline is not steady: plateaus around 0.21–0.24 (2001–2008)
and 0.18–0.21 (2009–2021), with the step between them in 2009–2010. 2023 (0.208) is out of line with 2022 (0.172) and 2024
(0.166); treat single recent years with caution.

### 2. `graphs/2026-09-28_t3_model_vs_data_over_time.png`

**Shows.** Four moments by year, data (black) vs model (red dashed). Top row
*targeted*: cliff ratio, corner share. Bottom row *untargeted*: wife's share of
the couple's hours, share of couples in which she out-earns him.

**How to read.** The top row matches by construction (exactly identified; the
loss is below 2.2e-6 in every year), so it is not evidence. The bottom row is.

**Key numbers (mean absolute error over 27 years).**

| Moment | Model | No-norm baseline |
|---|---|---|
| Wife's share of couple hours | 0.017 | 0.013 |
| Wife out-earns husband | 0.010 | 0.070 |

The out-earn share is real evidence for the norm: without it the model
overpredicts wives out-earning husbands by 7 points, and with it the error is 1
point. The hours share is **not** evidence: the no-norm baseline fits it
slightly better. The model underpredicts the hours share in every year.

### 3. `graphs/2026-09-28_t3_intensity_vs_exposure.png`

**Shows.** Three indices, 1980 = 100: *intensity* (hours of the wife's work lost
per norm-bound household), *exposure* (share of couples the norm binds on), and
*net* (share of all female market hours lost to the norm).

**How it is computed.** `t3-aggregate-distortion.R`: each year solved at
(alpha-hat, f-hat) and at alpha = 0 with F and kappa held fixed. The difference
in her hours is the norm's effect.

**How to read.** Net ≈ intensity × exposure. The two move in opposite
directions, so the net is roughly flat.

**Key numbers.** Exposure 16.2% → 29.6% of couples (**+83%**), mostly by 2000,
then rising slowly. Intensity 364 → 291 hours per bound couple (**−20%**), flat
until ~2008 and falling after. Net share of female hours lost 5.96% → 5.85%
(**−2%** endpoint to endpoint), peaking at index 129 in 2004. Rising exposure is
partly mechanical: as wives' wages approach husbands', more couples land where
"he should out-earn her" is a live constraint.

### 4. `graphs/2026-09-28_t3_aggregate_distortion.png`

**Shows.** Two panels by year: share of female market hours lost to the norm
(%), and full-time-equivalent jobs lost (millions, at 2,000 hours per FTE).

**How it is computed.** As figure 3; national totals use HHWT.

**Key numbers.** 5.96% of women's market hours in 1980, peak 7.66% in 2004,
5.85% in 2024. FTE jobs lost 1.17M (1980) → 2.40M (2024); the absolute number
roughly doubles because the population and exposure both grew. The norm also
**adds** hours for husbands, and more than she loses: in 1980 she loses 2.3bn
hours and he gains 3.0bn; in 2024, 4.8bn against 5.5bn. So total household
market hours **rise** under the norm, by +0.7bn in both years. The norm
reallocates market work from her to him rather than reducing it. Part of his
gain is husbands entering work (see the caveat below).

**Caveat (changed from the previous run).** The distortion is not purely
intensive. For wives it almost is: 0.03–0.06% of couples see the wife leave
work at alpha-hat. But the norm moves 1.8–3.5% of couples' **husbands into
work** (see the model checks table): she-only couples pay the norm on her whole
income, so a husband who would not work at alpha = 0 enters.

### 5. `graphs/2026-09-28_t3_corner_gradient_limitation.png`

**Shows.** Share of wives not working by year, in husband's-wage quintiles Q1,
Q3 and Q5, data vs model. **Untargeted**: only the aggregate corner share is
fitted.

**How to read.** Where the model puts non-participation. In the model it is set
by the income effect, rising with the husband's wage. In the data it is roughly
flat and U-shaped.

**Key numbers (mean over 27 years).** Q1: data 0.277 vs model 0.140. Q3: 0.226 vs
0.246. Q5: 0.301 vs 0.408. Q1 is unchanged from the previous run (0.140), even
though transfers left y0; Q5 rose from 0.396 to 0.408. The no-norm baseline is identical in
Q1/Q3/Q5, so the norm plays no part in this failure. Explanations already tested
and rejected: wage selection, and preference heterogeneity
(`t3-estimate-v3.R`).

### 6. `graphs/2026-09-28_t3_hours_earnings_wife_vs_husband.png`

**Shows.** Wife vs husband by year, three panels: mean annual market hours; mean
annual labour earnings (2024 dollars); median hourly wage (2024 dollars).

**How it is computed.** Directly from `model_input_households.csv`, no model.
Hours and earnings are means over all couples, zeros included. Wages are medians
among couples where **both** wages are observed, not imputed. Deflated to 2024
dollars with `deflate_to()`.

**How to read.** The norm's threshold is his earnings. His stagnation is part of
why exposure (figure 3) rises.

**Key numbers, 1980 → 2024.** Hours: wife +64%, husband 0%. Real earnings: wife
+190%, husband +30%. Real median wage: wife +49%, husband +8%.
*(Note: the previous version of this figure said "both spouses 18–65"; the
sample is 25–64. It also quoted an 18.5% "had his wages kept pace"
counterfactual that no current script computes. That claim is dropped until
it is reproduced.)*

### 7. `graphs/2026-09-28_t3_untargeted_by_quintile.png`

**Shows.** Group (C) of the moment report: three intensive-margin tests across
the husband's-wage quintile, each as data (black), model (red dashed) and
no-norm baseline (blue). Lines are the **mean over the 24 ACS years
2001–2024**; bands are the min–max range across those years. Decennial years are
excluded.
- Left: cliff ratio within the quintile. **Log scale.**
- Middle: among dual earners, share of couples where she works more hours than he.
- Right: among dual earners, her share of the couple's hours.

**How to read.** Every gradient exists at alpha = 0, because high-wage husbands
out-earn their wives mechanically. So the test is not whether the data slope,
but whether the data sit where the model puts them rather than where the
no-norm baseline does. The model's claim: tau = alpha × C rises with resources,
so the norm should bite harder at the top.

**Key numbers (ACS mean, Q1 → Q5).**

| | Data | Model | No norm |
|---|---|---|---|
| Cliff ratio | 0.99 → 2.67 | 0.81 → 18.1 | 0.85 → 1.80 |
| She works more hours | 0.26 → 0.19 | 0.53 → 0.00 | 0.64 → 0.11 |
| Her hours share | 0.457 → 0.438 | 0.520 → 0.359 | 0.531 → 0.372 |

- **Cliff.** The data rise more steeply than no-norm, so the norm is stronger at
  the top, qualitatively as the model says. But the model overshoots badly at
  Q4–Q5 (3.2 and 18.1 against 2.1 and 2.7). Measured by MAE across quintiles, the
  model is further from the data than the no-norm baseline in **all 24** ACS
  years. The unit income-elasticity of tau (the log-utility assumption,
  Proposition 1) produces too much bunching at the top.
- **She works more hours.** The model beats no-norm in 24 of 24 years (MAE 0.160
  vs 0.206), but the data are nearly flat and the model is far too steep.
- **Her hours share.** Near-tie (MAE 0.043 vs 0.047; model better in 21 of 24
  years). The data are flat; both model and baseline fall steeply.

Overall the data's quintile gradients are much flatter than the model's, which
is the same pattern as the corner gradient in figure 5.

---

## LaTeX tables

All are booktabs **tabulars only**, with no float, caption or label, so the
same file works in a Beamer frame and inside a `table` environment. Needs
`\usepackage{booktabs}`. Usage:

```latex
\begin{table}\centering\caption{...}\label{...}
  \input{2026-09-28_t3_moments_table.tex}
\end{table}
% in Beamer, for the 27-row tables:
{\scriptsize \resizebox{\textwidth}{!}{\input{2026-09-28_t3_estimates_table.tex}}}
```

Numbers are rounded in the script that writes the table (`write_tex_table()` in
`functions.R`), so rounding is a visible decision in code.

| File (`processed/results/`) | Written by | Contents |
|---|---|---|
| `2026-09-28_t3_estimates_table.tex` | `t3-estimate-v2.R` | By year: alpha × 10⁶, f, F ($), kappa × 10⁷, SMM loss |
| `2026-09-28_t3_model_checks_table.tex` | `t3-estimate-v2.R` | By year, % of couples: at h = T; any participation switch alpha = 0 → alpha-hat; wife exits; husband enters; wife exits at **any** alpha (W_III < W_IV < W_I) |
| `2026-09-28_t3_moments_table.tex` | `t3-estimate-v2.R` | (A) targeted and (B) untargeted moments, data / model / no norm, mean over ACS years |
| `2026-09-28_t3_quintile_tests_table.tex` | `t3-estimate-v2.R` | (C) quintile tests Q1–Q5, data / model / no norm, mean over ACS years (numbers behind figure 7) |
| `2026-09-28_t3_tau_table.tex` | `t3-compute-tau.R` | By year: mean C, tau (all), tau (binding), % bound (numbers behind figure 1) |
| `2026-09-28_t3_aggregate_distortion_table.tex` | `t3-aggregate-distortion.R` | By year: % bound, % female hours lost, hours lost per bound couple, FTE jobs lost, his hours gained (figures 3–4) |
| `2026-09-28_t3_norm_incidence_by_quintile_table.tex` | `t3-comparative-statics.R` | **Paper Table A3.** 2019, by husband's-wage quintile: % bound, hours lost, hours lost if bound |
| `2026-09-28_t3_norm_incidence_by_wage_ratio_table.tex` | `t3-comparative-statics.R` | 2019, the same by the couple's wage ratio w_f / w_m |
| `2026-09-28_t3_comparative_statics_table.tex` | `t3-comparative-statics.R` | 2019: % change in outcomes for +10% in each primitive |
| `2026-09-28_t3_alpha_heterogeneity_table.tex` | `t3-comparative-statics.R` | 2019: cliff, % at kink, corner as dispersion in alpha rises |

**Key numbers from the tables.**
- *Model checks.* h = T: **0.00%** in every year. Participation switch alpha = 0
  → alpha-hat: **1.9–3.5%** of couples, almost entirely husbands entering. Wives
  exiting at alpha-hat: 0.03–0.06%. Wives the norm could push out at **any** alpha
  (W_III < W_IV < W_I): **0.10–0.15%**, small but not zero.
- *Table A3 (2019).* Q1: 41.3% bound, 233 hours lost if bound. Q5: 7.2% bound,
  480 hours lost if bound. Q5 couples are less often bound, but a bound Q5 wife
  gives up about 2× the hours.
- *Comparative statics (2019).* +10% alpha moves the corner share by +0.01% and
  the cliff by +2.85%, so alpha is an hours parameter.
