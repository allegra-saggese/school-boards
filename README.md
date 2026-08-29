### School boards, culture, and women who work
#### WIP, last updated: MAY 2026 (frontier extension added)
***

##### Project overview
Economic analysis of culture and its impact on female labor force participation (LFPR)
in the US, with a focus on intra-household bargaining under social norms.
The project generates stylized facts about female LFPR across income levels, political
culture, and time — targeting findings that depart from the standard "economics dominate
at high income" story (Goldin U-curve). The end goal is a household bargaining model
where a norm penalty on wife's work is income-elastic: richer conservative households
can afford to enforce it, producing a cultural scissors in LFPR trends.

**Research question**: Do high-income women in conservative areas work less than
comparably-rich women in liberal areas, even holding household income constant?
And does the husband–wife hours gap widen with income in conservative (but not liberal)
counties — evidence of bargaining, not just constraint?

#### File structure

**Analysis tracks (two parallel pipelines):**

**Track A — ACS county panel (2010–2020):**
- `lfpr-panel-analysis.R` → county LFPR + income panel from ACS 5-year, CPI-adjusted
- `lfpr-groupings.R` → political-income group indicators (trad/asp_trad/dem_solid_poor/dem_solid_rich); *Section 6* adds departure descriptives: female LFPR × income quintile × political direction

**Track B — IPUMS household microdata (1970–2024):**
- Database is built by `ipums-bkp-build-database.R` (see BKP section below). `ipmus-data-cleaning.R` is **superseded**.
- `ipums-county-household-analysis.R` → county LFPR/hours panel + opposite-sex married spouse-pair micro file; *Section 6* adds HH composition descriptives (1-person, 2-person, spousal breakdown); *Section 7* adds STATEICP/COUNTYICP → FIPS crosswalk and merges spouse-pair data to political groups
- `ipums-married-household-suite.R` → multi-measure income analysis on spouse pairs; *Section 12* adds household-level hours × political group descriptives (requires Section 7 output)
- `ipums-county-female-lfpr-scatter.R`, `ipums-wage-quintile-time-graphs.R`, `ipums-spouse-income-scatter-plots.R` → additional IPUMS graphs

**BKP (2015) replication (Bertrand, Kamenica & Pan, QJE — "Gender Identity and Relative Income within Households"):**
- `ipums-bkp-build-database.R` → downloads IPUMS USA extract #4 via the API and builds `data/interim/ipums_bkp.sqlite`. Requires `IPUMS_API_KEY` in `.Renviron` (gitignored). Decennial 1970 (both forms)/1980/1990/2000 5% + ACS 1-year 2001–2024, no overlapping samples; ~39 variables including self-employment income (`INCBUS`/`INCFARM`/`INCBUS00`) and `HISPAN`. **Run this first.** Supersedes `ipmus-data-cleaning.R`, which is deprecated.
- `ipums-bkp-pure-replication.R` → recreates BKP's actual sample/spec as closely as our data allows: Figure 1/2 (young-couple distribution, triangular-kernel recode of the exact-0.5 mass) and Table 2/3 (wife's LFP & income gap ~ `PrWifeEarnsMore`, a demographic-cell potential-income measure), extended through 2024. Queries `ipums_bkp.sqlite` directly — does **not** use the shared pair-builder above (different age/household-composition/county restrictions; see architecture note in `claude/bkp-replication-v2-changes.md`).
- `ipums-bkp-augmented-tests.R` → second track: T1 income-share decomposition (labor/total/capital), T2 hourly-wage horse race, T3 political × frontier heterogeneity. Reuses the shared pair panel and the donut-RDD design from `ipums-rdd-breadwinner-norm.R`.
- `ipums-bkp-replication-approximate.R` → earlier, approximate comparison (predates the corrected sample construction above); kept as-is, not on BKP's actual sample.
- Internal decisions log and data-access gate: `claude/bkp-replication-v2-changes.md`, `claude/future-extensions.md`.

**Regressions:**
- `ipums-ols-regressions.R` → descriptive OLS: county-level (LFPR ~ income × vote_margin + FEs) and household-level (wife hours ~ income quintile × conservative + year FE + children)

**Frontier culture extension:**
- `bazzi-frontier-merge.R` → extracts county-level frontier indicator from Bazzi, Fiszbein & Gebresilasse replication GDB; merges to ACS panel and IPUMS pairs; produces frontier maps and frontier-stratified versions of all core Dem/Rep graphs (see *Section 6e* of `lfpr-groupings.R`, *Section 12e* of `ipums-married-household-suite.R`, and *Section 9* of `ipums-rdd-breadwinner-norm.R`)
- External data required: `FrontierCultureReplication/` folder (Bazzi et al. replication zip, placed in repo root)

**Identification (in progress):**
- `ballotopedia-data.R` → school board election event study for causal identification

**Infrastructure:**
- `functions.R`, `load-reqs.R`, `R/paths.R` → utilities; load into preamble of all files
- `config.yml` → path configuration (external data in Dropbox)

#### Data layout
- `data/raw`: raw inputs (kept out of git)
- `data/interim`: intermediate files (kept out of git)
- `data/processed`: processed outputs (kept out of git)
- `data/graphs`: plots and visuals

#### Paths and config
- Central path helpers live in `R/paths.R`.
- Update `config.yml` for external data locations (e.g., Dropbox).
- You can override paths via environment variables:
  - `SCHOOL_BOARDS_DATA_ROOT`
  - `SCHOOL_BOARDS_EXTERNAL_ROOT`

#### Workflow

**Track A — ACS county panel:**
1. Configure paths: set `external_data_root` in `config.yml` to your Dropbox project root.
2. Run `fred-county-panel.R` (requires `FRED_API_KEY`) → `data/processed/fred/`.
3. Run `10-2025-prelim-analysis.R` → Census/BEA regressions and plots.
4. Run `lfpr-panel-analysis.R` → ACS county panel; outputs date-prefixed `YYYY-MM-DD_lfpr_panel.csv`.
5. Run `lfpr-groupings.R` → political group indicators and departure descriptives (Section 6); outputs `YYYY-MM-DD_lfpr_panel_with_groups.csv`.

**Track B — IPUMS household microdata:**
6. Run `ipums-bkp-build-database.R` → `data/interim/ipums_bkp.sqlite` (needs `IPUMS_API_KEY` in `.Renviron`). *`ipmus-data-cleaning.R` is superseded and will refuse to run.*
7. Run `ipums-county-household-analysis.R` → county LFPR panel, spouse-pair micro file, HH composition summary (Section 6), and political merge (Section 7).
   - Section 7 requires `lfpr_panel_with_groups.csv` from step 5.
   - Output: `ipums_married_oppositesex_spouse_pairs_with_groups.csv`
8. `ipums-married-household-suite.R` is sourced automatically at the end of step 7 (or run independently).
   - Section 12 generates household-level political × income descriptives.

**Frontier culture extension:**
8b. Run `bazzi-frontier-merge.R` → requires `FrontierCultureReplication/data/GIS/maps.gdb` and `bazzi_frontier_indicators.csv` output from step 8. Outputs frontier maps and stratified graphs. Run after step 8.
   - Adds `data/processed/panel/bazzi_frontier_indicators.csv` (county-level frontier lookup, merge key: `fips`)
   - Frontier-stratified graphs are produced automatically when re-running `lfpr-groupings.R` (Section 6e), `ipums-married-household-suite.R` (Section 12e), and `ipums-rdd-breadwinner-norm.R` (Section 9) after the lookup file exists

**Regressions:**
9. Run `ipums-ols-regressions.R` → requires outputs from steps 5 and 7.

#### LFPR panel notes
- Script: `lfpr-panel-analysis.R`
- Merge keys: `fips`, `year`
- Core outcomes:
  - `lfpr_total`
  - `lfpr_female`
  - `lfpr_gap` (`lfpr_male - lfpr_female`)
- LFPR construction:
  - built from ACS detailed table `B23001` (sex-by-age-by-employment counts)
  - computes 20-64 LFPR as count-based rates, consistent across years
- Income source: ACS `B19013_001` (median household income)
- Real-value conversion:
  - nominal ACS income is CPI-adjusted using annual `R-CPI-U-RS`
  - values are rebased to `income_base_year` (current script default: 2023 dollars)
  - regression/plot income axis uses `log(median_hh_income_real)`
- Election overlays:
  - `vote_margin`
  - `vote_spread`
  - `vote_spread_norm` (within-year signed normalization for comparable color scales)
  - non-election years are filled using last observation carried forward (LOCF)
- Income groups:
  - `income_quintile_national` (within year, national distribution)
  - `income_quintile_state` (within state-year)

#### ACS overlap caveat
- The panel uses ACS 5-year endpoint files for full county coverage.
- Endpoint years are rolling windows (example: 2010 is 2006-2010, 2011 is 2007-2011).
- Adjacent endpoint years overlap by 4 years, so changes are not strictly non-overlapping year-over-year changes.

#### Model and plot outputs
- Models are run separately by year for each outcome with:
  - linear: `y ~ log_income`
  - quadratic: `y ~ log_income + I(log_income^2)`
- Graphics are organized broadly into:
  - baseline model visuals:
    - county scatter + fitted curves by outcome/year
    - state-by-year facet panels
  - election-overlay visuals:
    - county scatter/facet panels colored by vote metrics (`vote_margin`, `vote_spread_norm`, party shares)
    - national election-year comparison (`2012`, `2016`, `2020`) with national quintile cutoff lines
  - change-over-time visuals:
    - 2010 to 2020 county movement vector plots
  - grouping diagnostics and comparisons (`lfpr-groupings.R`):
    - boxplots by group
    - group time-trend plots with uncertainty bands
    - grouped income-vs-LFPR scatters (pooled and faceted by year)
    - male-vs-female LFPR overlays by group

#### Expected external files
Place these in `<external_data_root>/data/` (Dropbox):
- `Census-ACSST5Y2023.S2301-Data.csv`
- `BEA-county-per-capita-income.xlsx`
- `BEA-county-per-capita-income_edited.xlsx`
- `BEA-county-percapita-GDP.xlsx`
- `BEA-county-percapita-GDP-edits.xlsx`
- `data-zipped/` (GRF ZIP files)
- `grf-unzipped/` (GRF unzipped files)
- `ipums/` (IPUMS raw data files, including `usa_00001.xml`)

#### Panel outputs
- `data/processed/panel/YYYY-MM-DD_lfpr_panel.csv`
- `data/processed/panel/YYYY-MM-DD_lfpr_panel_with_groups.csv`
- `data/processed/panel/bazzi_frontier_indicators.csv` — county-level frontier lookup (3,109 counties; merge key: `fips`; variables: `tfe`, `is_frontier`, `yr_entered`)
- `data/processed/results/YYYY-MM-DD_lfpr_model_summary.csv`
- `data/processed/results/YYYY-MM-DD_lfpr_model_coefficients.csv`
- `data/processed/results/YYYY-MM-DD_lfpr_model_tables.tex`
- `data/processed/results/YYYY-MM-DD_lfpr_group_stats.csv`
- `data/processed/results/YYYY-MM-DD_lfpr_group_year_stats.csv`
- `data/processed/results/rdd_donut_breadwinner_norm_results.csv`
- `data/graphs/YYYY-MM-DD_*.png`

#### Frontier culture extension — methodology notes

The frontier classification follows Bazzi, Fiszbein & Gebresilasse (2020, QJE; 2025 working paper). A county is classified as **frontier** (`is_frontier = 1`) if it was ever within 100km of the US Census Bureau's historical frontier line and had population density < 6 persons/sq mi in any decennial Census from 1790–1890. The continuous measure `tfe` (total frontier experience) counts the number of such Census decades (range 0–63). Source data: `FrontierCultureReplication/data/GIS/maps.gdb`, layer `US_county_2010_TFE_Popdens`.

80.4% of contiguous US counties are classified as frontier (2,501 of 3,109). Non-frontier counties (TFE = 0) are primarily the original eastern seaboard settlements. The frontier indicator is pre-determined relative to the 2010–2020 analysis panel and is used as a fixed county characteristic.

**Prediction tested**: the income-elastic norm gap (near-zero at Q1–Q3, positive at Q4–Q5 in Republican vs Democratic counties) should be amplified in frontier counties, where Bazzi et al. document persistently stronger gender norms. This connects the income-elasticity finding to a deep historical root rather than recent political sorting.

**Key frontier output graphs** (all in `data/graphs/`):
- `YYYY-MM-DD_frontier_map_binary.png` — US choropleth: frontier vs non-frontier counties
- `YYYY-MM-DD_frontier_map_tfe.png` — continuous TFE depth map
- `YYYY-MM-DD_departure_female_lfpr_by_quintile_dem_vs_rep_frontier.png`
- `YYYY-MM-DD_departure_q5_female_lfpr_trend_dem_vs_rep_frontier.png`
- `YYYY-MM-DD_ipums_hh_gradient_dem_vs_rep_frontier.png`
- `YYYY-MM-DD_hours_by_income_decile_husband_wife_dem_vs_rep_frontier.png`
- `YYYY-MM-DD_share_vs_hh_income_decile_dem_vs_rep_frontier.png`

#### Data and reference sources
- Princeton Elections LibGuide (data references and archives): [https://libguides.princeton.edu/elections#s-lg-box-10082744](https://libguides.princeton.edu/elections#s-lg-box-10082744)
- MIT Election Data and Science Lab (MEDSL): [https://electionlab.mit.edu/data#data](https://electionlab.mit.edu/data#data)
- Urban Institute `urbnmapr` (future source for county/state mapping workflows): [https://urbaninstitute.github.io/urbnmapr/](https://urbaninstitute.github.io/urbnmapr/)
- `tidycensus` documentation: [https://walker-data.com/tidycensus/](https://walker-data.com/tidycensus/)
- Census API / ACS 5-year docs: [https://www.census.gov/data/developers/data-sets/acs-5year.html](https://www.census.gov/data/developers/data-sets/acs-5year.html)
- ACS program overview: [https://www.census.gov/programs-surveys/acs](https://www.census.gov/programs-surveys/acs)
- FRED API docs: [https://fred.stlouisfed.org/docs/api/fred/](https://fred.stlouisfed.org/docs/api/fred/)
