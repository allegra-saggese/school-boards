# BKP (2015) replication — v2 decisions log (internal)

Internal documentation track: data provenance, cleaning/sample decisions, and
methodological choices behind the BKP replication update. Companion to the
external, reviewer-ready code in `ipums-bkp-pure-replication.R` and
`ipums-bkp-augmented-tests.R`. See `claude/future-extensions.md` for deferred
items and the new-extract checklist.

Source of the decisions below: a prior Cowork session
(`~/Downloads/bkp-replication-chat-history.md`, 2026-08-27) plus a direct
re-verification of BKP's sample construction against the NBER working paper
(w19023, May 2013) text, done in this session.

## Why the original `ipums-bkp-replication.R` wasn't actually a BKP replication

Six gaps, identified in the prior Cowork session and confirmed here:

1. **Age range.** BKP's Figure 1 (the headline 0.5-cliff figure) restricts to
   *young couples*: wife 22-31, husband 24-33 (NBER w19023, Section 3.1, p.6).
   The old script used the shared pipeline's 25-64 for both spouses, pooling
   cohorts and dropping BKP's core 22-24 age band for wives.
2. **Household-composition restriction not in BKP.** The shared pipeline
   (`ipums-county-household-analysis.R`) requires `n_work_age = 2 AND
   n_ge_25 = 2` — exactly two adults 25+ in the household. BKP has no such
   restriction; this drops multigenerational households, which correlates
   with culture/ethnicity/income (selection risk).
3. **Year/dataset footprint mismatch.** BKP Figure 1 = ACS 2008-2010 3-year
   aggregate; Figure 2 = decennial Census 1970-2000 (+ ACS). The old script's
   "1980-2011 era" was really 1980 + 1990 + ACS 2005-2011 — not BKP's years.
4. **The exact-0.5 mass point.** BKP recode it with a triangular kernel
   (footnote 12, exact formula below) rather than plotting the raw spike. The
   old script (and the existing donut-RDD design) instead excludes a ±2pp
   donut around 0.5 — a different, defensible choice for a different purpose
   (RDD identification vs. matching BKP's Figure 1 exactly).
5. **Income concept.** BKP's "labor income" = wage/salary + self-employment
   (INCBUS00 + INCFARM pre-2000). The old script used INCWAGE only.
6. **Earner requirement.** BKP restricts to the *interior* (both spouses'
   labor income > 0). The old script's `z_wage` used an at-least-one-earner
   sample (couple_wage > 0), which includes the 0/1 mass points BKP excludes
   from the distributional analysis.

Marriage definition (mutual SPLOC link, opposite-sex, married) and HHWT
weighting were already consistent with BKP.

## BKP's actual specification, verified against NBER w19023

**Figure 1** (p. 6-7): ACS 2008-2010 3-year aggregate, young couples (wife
22-31, husband 24-33) — "these ages correspond to the youngest age group in
our construction of the marriage markets" (footnote 8). Interior sample (both
labor incomes > 0). `relativeIncome_i = wifeIncome_i / (wifeIncome_i +
husbIncome_i)`. Exact-0.5 observations recoded via a **triangular kernel**
(footnote 12): with an n-bin histogram, bin *k* ∈ {1,...,n} receives a share

```
(n/2 − |n/2 − (k−1)|) / ((n/2)(n/2−1))
```

of the mass at exactly 0.5. 20 bins used throughout Figures 1-3. Panel (a) is
the histogram with a local polynomial fit estimated separately on each side
of 0.5.

**Figure 2** (p. 9): same construction, one panel per decade 1970/1980/1990/
2000, still Census/ACS. Introduced immediately after Figure 1 in the same
subsection with no separate restriction stated — we carry the young-couple
restriction forward for Figure 2 (a documented assumption, not stated
verbatim in the text).

**Section 3.2 sanity fact** (p. 9): "the wife earns more than the husband in
26 percent of the couples where both individuals are between 18 and 65 years
old" — ACS 2008-2010, the 18-65 sample (distinct from the young-couples
Figure-1 sample). Used as an in-script sanity check.

**Section 5, Table 2** (p. 18-20): **not** a simple `D(share > 0.5)`
regression. Sample: 1970-2000 Census + ACS 2008-2010, both spouses 18-65,
husband working. For each wife, a demographic cell is defined by 5-year age ×
5 education levels (< HS, HS, some college, college, > college) × race
(white/Black/Hispanic, others dropped) × state × year, among *working* women.
Her potential-earnings distribution is the empirical wage distribution in that
cell at the 5th-95th percentile (19 points, "vigintiles" in the text).
`PrWifeEarnsMore_i` = share of those 19 potential-income points exceeding the
husband's actual income. Baseline LPM: `wifeLFP ~ PrWifeEarnsMore + w_i^p
(vigintile controls) + ln(husbIncome) + year FE + state FE + race + 5yr
age-group dummies (both spouses) + education-group dummies (both spouses)`,
weighted, SEs clustered on the wife's demographic cell. Baseline β₁ = −0.254
(p<0.01); a 10pp increase in PrWifeEarnsMore lowers wife's LFP by ~1.8pp.

**Section 5.2, Table 3** (p. 21-22): same RHS, outcome = `incomeGap_i =
(wifeIncome_i − wifePotential_i) / wifePotential_i`, working wives only.
Baseline β₁ = −0.094 (p<0.01).

**Out of scope for this replication** (data/instruments we don't have):
Table 1 (marriage-formation LPM with a Bartik-style industry-wage
instrument — needs 12-industry wage growth by state/year); Tables 4-5
(marital satisfaction/divorce, NSFH 1987-88/1992-94 waves); Table 6 (division
of chores, ATUS × CPS). See `claude/future-extensions.md`.

## Decisions locked for the pure replication (`ipums-bkp-pure-replication.R`)

- **No county filter**, either track — BKP doesn't use one; the shared
  pipeline's `COUNTYICP IS NOT NULL` filter silently drops most PUMAs.
- **Loosen household composition** — only require a mutually-linked
  opposite-sex spouse pair (SPLOC), not exactly-two-adults. Built via a direct
  wife-candidate × husband-candidate SQL join on `ipums_data.sqlite`, bypassing
  the shared pipeline's household-composition CTE entirely (see architecture
  note below).
- **Income:** INCWAGE only for now (labor income concept #5 above — see
  `INCOME_MEASURE_SWAP` note in the script header and the new-extract gate in
  `claude/future-extensions.md`).
- **Race:** White/Black/Other (RACE variable only — no HISPAN in our extract,
  so BKP's white/Black/Hispanic split isn't exactly reproduced).
- **2000 decennial:** not in our extract; ACS 2001 used as the nearest
  available proxy, labeled as such everywhere it appears.
- **ACS 2008-2010 "3-year aggregate":** approximated by stacking the three
  1-year ACS files (2008, 2009, 2010) we have, not the Census Bureau's pooled
  3-year microdata product (different weighting scheme).
- **Regression sample cap:** `lm()` + `vcovCL` on the full multi-million-row
  pooled Table 2/3 sample is impractically slow. Fitting uses a capped random
  subsample (250k rows, `set.seed(42)`) — sanity-check summary statistics
  (LFP rates by year) still use the full uncapped sample. Compute-time guard,
  not a scope cut.

## Architecture: why a separate pipeline, not shared-script edits

`ipums-county-household-analysis.R`'s pair-builder (age 25-64,
`n_work_age=2`, county filter) feeds `ipums-married-household-suite.R`,
`ipums-rdd-breadwinner-norm.R`, the OLS script, and the frontier merge.
Changing its restrictions to match BKP would silently change every other
analysis's sample. `ipums-bkp-pure-replication.R` instead queries
`ipums_data.sqlite` directly with its own restrictions, entirely independent
of that shared pipeline.

`ipums-bkp-augmented-tests.R` (T1-T3) takes the opposite approach
deliberately: it *does* reuse the shared `ipums_married_oppositesex_
spouse_pairs_with_kids.csv` / `_with_groups.csv` panels and the donut-RDD
design from `ipums-rdd-breadwinner-norm.R`, because T1-T3 are meant to extend
"our design" (income-elastic, culturally-patterned norm), not replicate BKP's
sample — the two tracks intentionally use different sample constructions for
different purposes.

## T1-T3 (augmented track) — retired variable

`INCTOT - INCSS - INCWELFR` (the running variable used in
`ipums-rdd-breadwinner-norm.R` and the old approximate BKP script) is retired
in the augmented track's T1: it's incoherent as a "labor income" measure since
it leaves capital income and non-SS/welfare transfers in. T1 replaces it with
an explicit three-way decomposition: (a) labor earnings (INCWAGE), (b) total
income (INCTOT), (c) capital/non-labor income (INCINVST + INCOTHER).
