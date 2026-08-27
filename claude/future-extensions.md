# BKP replication — future extensions (internal)

Deferred items and the data-access gate for the BKP replication update. See
`claude/bkp-replication-v2-changes.md` for the decisions already implemented
in `ipums-bkp-pure-replication.R` and `ipums-bkp-augmented-tests.R`.

## Gate: new IPUMS extract needed

Blocking dependency the user must action (no IPUMS API key is configured in
this repo, and extract submission needs the account/website) — see plan file
for the full checklist. Submit a new IPUMS USA extract adding:

- **Variables:** `INCBUS00`, `INCFARM`, `INCRETIR`, `MARST` (only the quality
  flags for the first two — `QINCBUS`, `QINCFARM` — exist in the current
  extract; the value columns don't).
- **Samples:** 2000 decennial (1%); the official ACS 2008-2010 3-year
  aggregate product, if still available as a distinct IPUMS sample (else the
  1-year-stack proxy stays in place).

Once it lands (`data/interim/ipums_data.sqlite` rebuilt from the new extract
via `ipmus-data-cleaning.R`):
- Flip `labor_income_expr` / the `INCOME_MEASURE_SWAP` point in
  `ipums-bkp-pure-replication.R` from `INCWAGE` to
  `INCWAGE + INCBUS00 (+ INCFARM pre-2000)` — one-line change, structure
  already in place.
- Replace the ACS-2001-as-2000-proxy years in `bkp_era_decade_years` /
  `bkp_era_table_years` with the real 2000 decennial.
- Replace the 1-year-stack Figure 1 sample with the true 3-year aggregate
  sample if IPUMS still offers it.
- Re-derive `race3` bucketing to add a Hispanic category once `HISPAN` is
  pulled (currently White/Black/Other only).

## Deferred BKP tables (out of scope for the current replication)

- **Table 1 — marriage formation (Bartik IV).** Needs: 12-industry
  classification, state × industry × gender wage levels by year, a
  marriage-market panel (state × race × age-group × education-group ×
  census-year), and construction of the modified Bartik instrument
  (percentile-level predicted wages, p ∈ {5th,...,95th}, per BKP Section 4).
  Substantially larger build than Table 2/3 — revisit only if the pure
  replication's Table 2/3 results look worth the marriage-formation extension.
- **Tables 4-5 — marital satisfaction & divorce (NSFH).** Needs the National
  Survey of Families and Households, waves 1 (1987-88) and 2 (1992-94) —
  publicly available (ICPSR) but not currently in this repo's data folders.
- **Table 6 — division of chores (ATUS × CPS).** Needs the American Time Use
  Survey linked to CPS weekly-earnings data (2003-2011) — also public, also
  not currently pulled.

## T2 augmented-test follow-up

`ipums-bkp-augmented-tests.R`'s T3 stratifies T1 (the income-share
decomposition) by political lean × frontier status, but does **not**
re-stratify T2 (the hourly-wage horse race) — the
`ipums_married_oppositesex_spouse_pairs_with_groups.csv` panel used for the
political/frontier merge doesn't carry `male_annual_hours` (only the base
`_with_kids.csv` panel does). Follow-up: add `male_annual_hours` (and
`female_annual_hours`, already implicitly needed) to the `_with_groups.csv`
panel build in `ipums-county-household-analysis.R` Section 7, then T2 can be
re-stratified the same way T1 is.

## Admin/tax data (gold standard, not accessible to us)

BKP's own robustness checks use data we can't get:
- **SIPP × SSA/IRS DER** (BKP Figure 3): public SIPP exists but is
  self-reported, same misreporting concern as the ACS. BKP's version is
  linked to Social Security Administration / IRS Detailed Earnings Record
  data via the Census Synthetic Beta program — accessible only through a
  Census FSRDC (Federal Statistical Research Data Center) with a research
  proposal and months of clearance lead time. This is a genuine access
  barrier, not a cost one.
- **Canada LAD** (BKP Figure 4): Statistics Canada's Longitudinal
  Administrative Data Dictionary, a 20% taxfiler panel 1983-2006 — not
  publicly accessible; BKP's own RAs ran the code on Statistics Canada's
  behalf.
- **IRS SOI Public Use File:** publicly available but strips spouse-linkage
  and geography, so it can't support a household-pair analysis.

If a future version of this project pursues the admin-data route, an FSRDC
proposal is the realistic path — flagged here as a multi-month lead-time item,
not something to plan into a near-term deliverable.

## Literature backing the augmented track (T1-T3)

- Akerlof & Kranton (2000) — identity-economics foundation for the norm.
- Bertrand, Kamenica & Pan (2015, QJE) — the paper being replicated/extended.
- Binder & Lam (2022, JHR) — assortative-matching critique T1 is designed to
  address (a share-specific cliff pattern isn't produced by matching alone).
- Murray-Close & Heggeness (2018, Census WP) — misreporting critique; BKP's
  own SIPP/DER check addresses it but that data isn't available to us, hence
  T2's hourly-wage horse race as a partial, lower-bar alternative.
- Bertrand et al., "Coworking spouses" (AEJ:Applied, 2018) — related
  mechanism, not yet incorporated.
- International replications for context: Wieber/Holst and Sprengholz et al.
  (Germany); Lippmann, Georgieff & Senik (France).
