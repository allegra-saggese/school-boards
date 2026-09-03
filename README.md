# School boards, culture, and women who work

**WIP — last updated September 2026**

Economic analysis of culture and its effect on female labour force participation
in the US, with a focus on intra-household bargaining under social norms.

**Research question.** Do high-income women in conservative areas work less than
comparably rich women in liberal areas, holding household income constant? And
does the husband–wife hours gap widen with income in conservative but not
liberal counties — evidence of bargaining rather than constraint?

The end goal is a household bargaining model in which the norm penalty on the
wife's work is income-elastic: richer conservative households can afford to
enforce it, producing a cultural scissors in LFPR trends.

---

## The three parts

The paper has three components, and the repository is organised to match.

| | Folder | What it does | Identifies a causal effect? |
|---|---|---|---|
| **T1** | `t1/` | Replicates Bertrand, Kamenica & Pan (QJE 2015) on their own sample and extends it to 2024 | No — replication |
| **T2** | `t2/` | Empirical culture × wealth quadrant: is the norm elastic to wealth or to culture? | No — exploratory by design |
| **T3** | `t3/` | Structural household utility model with an identity norm | No — calibrated model |

Shared infrastructure and the data-build scripts every part depends on stay at
the repository root. Superseded work lives in `archive/`.

---

## Layout

```
school-boards/
├── _setup.R                  # shared preamble — source this first in every script
├── functions.R               # project-wide helpers
├── load-reqs.R               # package list
├── R/paths.R                 # path resolution (config.yml + env vars)
├── config.yml                # external data locations
│
├── <build scripts>           # shared data construction, see "Build" below
│
├── t1/                       # BKP replication
├── t2/                       # culture × wealth quadrant
├── t3/                       # structural model
└── archive/                  # superseded — kept for reference, not run
```

### Running a script

Every script begins with:

```r
source(here::here("_setup.R"))
```

`here::here()` anchors to the repository root via `school-boards.Rproj`, so
scripts run correctly **from any working directory**, including from inside
`t1/`, `t2/` and `t3/`. Nothing depends on `setwd()`.

```bash
Rscript t3/t3-figures.R
```

### Running everything

`run-pipeline.sh` runs every analysis script in dependency order, logging each
to `logs/` and writing a pass/fail table to `logs/summary.txt`:

```bash
./run-pipeline.sh            # the analysis layer (21 scripts)
./run-pipeline.sh --rebuild  # also rebuild the panels first (slow)
./run-pipeline.sh t3         # only scripts matching "t3"
./run-pipeline.sh --list     # print the run order
```

`--rebuild` adds the two panel-construction scripts. They are off by default
because they **overwrite large derived inputs** that everything else reads, and
a failure part-way leaves a truncated file:

| Opt-in via `--rebuild` | Rebuilds |
|---|---|
| `ipums-county-household-analysis.R` | The spouse-pair panel |
| `ipums-model-data.R` | `model_input_households.csv` (~3GB) |

Four scripts are **never** run by it — external side effects, or a key the
analysis layer does not need. Run these by hand:

| Excluded | Why |
|---|---|
| `ipums-bkp-build-database.R` | Re-downloads the extract and rebuilds the ~19GB SQLite |
| `ipums-submit-housing-extract.R` | Submits a *new* IPUMS extract request; their API has no delete endpoint |
| `lfpr-panel-analysis.R` | Needs `CENSUS_API_KEY` and re-downloads the ACS |
| `fred-county-panel.R` | Needs `FRED_API_KEY` and re-downloads |

`T3_YEARS` defaults to the full 1980–2024 series. Narrowing it is unsafe:
`t3-estimate-v2.R` writes a dated estimates file that every downstream T3
script reads via `read_newest()`, so a partial run silently shadows the full
series — and `t3-comparative-statics.R` hardcodes `YR <- 2019` and fails
outright without it.

---

## Build scripts (repository root)

Run in this order. These feed more than one of T1/T2/T3, which is why they sit
outside the three folders.

| # | Script | Produces | Needs |
|---|---|---|---|
| 1 | `ipums-bkp-build-database.R` | `data/interim/ipums_bkp.sqlite` | `IPUMS_API_KEY` |
| 2 | `lfpr-panel-analysis.R` | `data/processed/panel/YYYY-MM-DD_lfpr_panel.csv` | `CENSUS_API_KEY` |
| 3 | `lfpr-groupings.R` | `…_lfpr_panel_with_groups.csv` | step 2 |
| 4 | `ipums-county-household-analysis.R` | spouse-pair panel + county LFPR panel | steps 1, 3 |
| 5 | `ipums-model-data.R` | `data/processed/panel/model_input_households.csv` | step 1 |
| 6 | `ipums-submit-housing-extract.R` → `ipums-build-housing-merge.R` | housing-wealth lookup for T2 | `IPUMS_API_KEY` |

`ipums-married-household-suite.R` is sourced automatically at the end of step 4,
and can also be run on its own. `fred-county-panel.R` (needs `FRED_API_KEY`) is
an optional county series used by the earlier ACS work.

**Step 1 must run first.** It downloads IPUMS USA extract #4 — 29 samples, 39
variables, no overlapping samples — covering the 1970/1980/1990/2000 decennials
and ACS 2001–2024, including self-employment income (`INCBUS`/`INCFARM`/
`INCBUS00`), `HISPAN` and `MARST`. Everything downstream reads it.

Exploratory descriptive graphs also live at the root:
`ipums-county-female-lfpr-scatter.R`, `ipums-wage-quintile-time-graphs.R`,
`ipums-spouse-income-scatter-plots.R`.

---

## T1 — BKP replication

Bertrand, Kamenica & Pan (2015), *Gender Identity and Relative Income within
Households*. Replicated on BKP's own sample, then re-run on all data now
available (through 2024).

| Script | Role |
|---|---|
| `t1/t1-replication.R` | The replication proper: Figures 1–2, Tables 2–3, on three samples |
| `t1/t1-summary-outputs.R` | Summary tables — sample construction, Table 1, cliff ratio and β₁ by year |
| `t1/t1-augmented-tests.R` | Supporting decompositions A and B (mechanism tests) |
| `t1/t1-figures.R` | **All T1 figures** |

Run order: `t1-replication.R` → `t1-summary-outputs.R` → `t1-figures.R`.
`t1-augmented-tests.R` is independent of the figures.

**Benchmarks** are the published QJE tables, not the 2013 NBER working paper:
Table II (wife's LFP, p.593) col 1 −0.178, col 2 (cubic) −0.142, col 4 −0.143;
Table III (income gap, p.597) col 1 −0.031, col 2 −0.095, col 4 −0.109.

**Key outputs**

- `data/graphs/*_bkp_pure_descriptive_trends.png` — the four Data-section series
- `data/graphs/*_bkp_pure_cliff_ratio_by_year.png` — has threshold avoidance weakened?
- `data/graphs/*_bkp_pure_coefficient_plot.png` — β₁ by sample and specification, against BKP's published values
- `data/graphs/*_bkp_pure_beta1_by_year.png` — β₁ year by year
- `data/graphs/*_bkp_pure_figure{1,2}_*.png` — BKP's own two figures
- `data/processed/results/*_bkp_pure_table23_era_comparison.csv` — the headline coefficients
- `data/processed/results/*_bkp_pure_{sample_construction,summary_statistics}.csv`

Deliberate deviations from BKP and out-of-scope tables are documented in the
header of `t1/t1-replication.R` and in `claude/bkp-replication-v2-changes.md`.

---

## T2 — the culture × wealth quadrant

Exploratory. Sorts married couples into a 2×2 on culture (county presidential
vote margin) and wealth (housing), and compares the wife's labour supply on both
margins — participation, and hours given participation.

|  | not wealthy | wealthy |
|---|---|---|
| **conservative** | cons / poor | cons / wealthy |
| **progressive** | prog / poor | prog / wealthy |

| Script | Role |
|---|---|
| `t2/t2-empirical-quadrant.R` | Builds the quadrant, runs the regressions, writes the RDS cache |
| `t2/t2-figures.R` | **All T2 tables and figures** |
| `t2/t2-rdd-breadwinner-norm.R` | Donut RDD at the equal-earnings threshold |
| `t2/t2-ols-regressions.R` | Descriptive OLS, county and household level |

`t2-empirical-quadrant.R` writes an RDS cache to
`data/interim/t2_quadrant.rds`, and `t2-figures.R` reads it from there. No
environment setup is needed:

```bash
Rscript t2/t2-empirical-quadrant.R
Rscript t2/t2-figures.R
```

Override the location with `T2_CACHE_WRITE` (writer) and `T2_CACHE` (reader) if
you want to keep several quadrant samples side by side.

**Why housing is the wealth axis.** Home equity is the median household's
dominant asset and is reported for ~70% of households, so it gives a wealth
*level* rather than a binary "reports asset income". It also separates two
forces with opposite signs — leverage (a mortgage requires a second income,
pushing her *into* work) and wealth (owning outright lets her *out*) — that a
single asset-income measure averages into an uninformative null.

**Two guards** keep the quadrant from silently re-measuring T1's income
gradient: house value is ranked *within state × year*, and every regression
conditions on the **husband's** labour-income decile, never the couple's.

**Key outputs**

- `data/graphs/*_t2_quadrant_lfp_and_hours.png` — the headline comparison
- `data/graphs/*_t2_income_elasticity_of_the_norm.png`
- `data/graphs/*_t2_coefplot_wife_{lfp,weekly_hours}.png`
- `data/graphs/*_t2_interaction_{across_specs,by_year}.png` — robustness
- `data/graphs/*_rdd_donut_*.png` — RDD density and kink plots
- `data/processed/results/*_t2_main_table.tex` — the formatted main table
- `data/processed/results/*_t2_{balance_table,quadrant_means,decile_coefficients}.csv`
- `data/processed/results/rdd_donut_breadwinner_norm_results.csv`

**Limitations.** Vote margin is contextual (the place, not the couple), so
culture estimates are attenuated. County is identified for only ~61% of ACS
households. Repeated cross-sections; wealth and labour supply are jointly
chosen. Housing data covers 2012–2020.

---

## T3 — the structural model

A static household model in which the identity penalty enters as a wedge
τ = α/u′(C), which under log utility is α·C — a proportional subsidy on his
hours and an equal proportional tax on hers.

| Script | Role |
|---|---|
| `t3/t3-model-solver.R` | Closed-form solver. Sourced by everything below; writes nothing |
| `t3/t3-estimate-v2.R` | **Main estimation** of (α, f), year by year |
| `t3/t3-estimate-v3.R` | Extension adding preference heterogeneity (α, f, σ_κ) |
| `t3/t3-compute-tau.R` | Converts α to the wedge τ, comparable across years |
| `t3/t3-aggregate-distortion.R` | Counterfactual: hours lost with the norm switched off |
| `t3/t3-comparative-statics.R` | Elasticities, and who bears the norm |
| `t3/t3-social-multiplier.R` | Social enforcement term, explored on random draws |
| `t3/t3-figures.R` | **All T3 figures** |

Run order: `t3-estimate-v2.R` → `t3-compute-tau.R` → `t3-aggregate-distortion.R`
→ `t3-figures.R`. The estimation takes a year list via `T3_YEARS`:

```bash
T3_YEARS=1980,1990,2000,2010,2020,2024 Rscript t3/t3-estimate-v2.R
```

**Identification.** Two free parameters against five moments, so the fit is
testable. α is pinned by the cliff ratio, f by the corner share, and the corner
share by the husband's wage quintile tests F's functional form. Two moments —
the wife's share of couple hours, and the share of couples where she out-earns —
are **held back entirely** and are the model's out-of-sample test.

**Key outputs**

- `data/graphs/*_t3_tau_over_time.png` (+ `.pdf`) — the headline: the norm wedge, 1980–2024
- `data/graphs/*_t3_model_vs_data_over_time.png` — fit on targeted *and* untargeted moments
- `data/graphs/*_t3_aggregate_distortion.png` — female hours lost to the norm
- `data/graphs/*_t3_intensity_vs_exposure.png` — the two offsetting forces
- `data/graphs/*_t3_hours_earnings_wife_vs_husband.png` — why his stagnation matters
- `data/graphs/*_t3_corner_gradient_limitation.png` — the model's known failure, disclosed
- `data/processed/results/*_t3_estimates_v2_by_year.csv` — the fitted parameters
- `data/processed/results/*_t3_{tau_series,aggregate_distortion}.csv`

τ rather than α is reported over time: α falls 89% across the sample, but most
of that is nominal income growth rather than the norm weakening.

---

## Outputs, paths and conventions

All writes go through two helpers in `functions.R`. Nothing writes a path
directly.

| Helper | Destination | Used for |
|---|---|---|
| `save_plot(name, expr, …)` | `data/graphs/` | every figure; `also_pdf = TRUE` adds a vector copy for LaTeX |
| `dated_path(dir, name)` | anywhere | every CSV, table and cache |
| `read_newest(dir, pattern)` | — | reading the current version of a result back |

Both prefix filenames with `YYYY-MM-DD`, so results accumulate rather than
overwrite. **Read them back with `read_newest()`**, never `list.files()[1]` —
it prefers dated files over undated ones and returns the most recent, which
plain sorting does not.

### Data layout

| Directory | Contents | In git? |
|---|---|---|
| `data/raw/` | raw inputs | no |
| `data/interim/` | SQLite databases, RDS caches | no |
| `data/processed/panel/` | analysis panels | no |
| `data/processed/results/` | tables, coefficients, model estimates | no |
| `data/graphs/` | figures | no |
| `data/graphs/archive/` | superseded figures | no |

Graphs that have been regenerated are moved to `data/graphs/archive/`; only the
most recent version of each figure stays in `data/graphs/`. Result files with no
live producer are moved to `data/processed/results/archive/`.

### Configuration

- Paths resolve through `R/paths.R`, driven by `config.yml`.
- Set `external_data_root` in `config.yml` to your Dropbox project root.
- Override at runtime with `SCHOOL_BOARDS_DATA_ROOT` or
  `SCHOOL_BOARDS_EXTERNAL_ROOT`.
- API keys live in `.Renviron` (gitignored): `IPUMS_API_KEY`, `CENSUS_API_KEY`,
  `FRED_API_KEY`.

### Expected external files

Place in `<external_data_root>/data/`:
`Census-ACSST5Y2023.S2301-Data.csv`, `BEA-county-per-capita-income.xlsx` (and
`_edited`), `BEA-county-percapita-GDP.xlsx` (and `-edits`), `data-zipped/`,
`grf-unzipped/`, `ipums/`.

---

## The ACS county panel (Track A)

Built by `lfpr-panel-analysis.R`; merge keys `fips` + `year`.

- **Outcomes**: `lfpr_total`, `lfpr_female`, `lfpr_gap` (male − female), built
  from ACS detailed table `B23001` as count-based 20–64 rates, consistent
  across years.
- **Income**: ACS `B19013_001`, CPI-adjusted with annual `R-CPI-U-RS` and
  rebased to 2023 dollars. Regressions use `log(median_hh_income_real)`.
- **Elections**: `vote_margin`, `vote_spread`, `vote_spread_norm` (within-year
  signed normalisation). Non-election years filled by LOCF.
- **Income groups**: `income_quintile_national` (within year) and
  `income_quintile_state` (within state-year).

**Overlap caveat.** The panel uses ACS 5-year endpoint files for full county
coverage. Endpoint years are rolling windows (2010 = 2006–2010, 2011 =
2007–2011), so adjacent years overlap by four and changes are *not* strictly
non-overlapping year-over-year changes.

---

## `archive/`

Kept for reference; not part of any pipeline and not run by anything.

| Script | Why |
|---|---|
| `ipmus-data-cleaning.R` | Built the old 39GB `ipums_data.sqlite`, replaced by `ipums_bkp.sqlite` |
| `ipums-bkp-replication-approximate.R` | Predates the corrected BKP sample construction |
| `t3-estimate.R` | Two-norm-parameter (α₁, α₂) specification, superseded by `t3-estimate-v2.R` |
| `model-simulation.R`, `model-simulation-v2.R` | Prototype models, superseded by `t3/t3-model-solver.R` |
| `bazzi-frontier-merge.R`, `frontier-secondary-analysis.R` | Frontier-culture extension, dropped 2026-08-30 |
| `10-2025-prelim-analysis.R` | Early Census/BEA exploration |
| `lfpr-bls-figures.R` | Standalone BLS plots; reads hard-coded `~/Downloads` paths |
| `ballotopedia-data.R` | Planning notes for the school-board event study; no code yet |

The frontier line was dropped because total frontier experience duplicates what
the county vote margin already measures in T2, adding a second attenuating proxy
on top of an attenuating one — and the T2 political interaction was null under
state fixed effects and county clustering.

---

## Internal documentation

Decisions logs, not reviewer-facing:
`claude/bkp-replication-v2-changes.md`, `claude/future-extensions.md`.

## Data and reference sources

- [MIT Election Data and Science Lab](https://electionlab.mit.edu/data#data)
- [Princeton Elections LibGuide](https://libguides.princeton.edu/elections#s-lg-box-10082744)
- [tidycensus](https://walker-data.com/tidycensus/) ·
  [ACS 5-year API](https://www.census.gov/data/developers/data-sets/acs-5year.html) ·
  [ACS program overview](https://www.census.gov/programs-surveys/acs)
- [FRED API](https://fred.stlouisfed.org/docs/api/fred/)
- [urbnmapr](https://urbaninstitute.github.io/urbnmapr/) — future county/state mapping
