### School boards, culture, and women who work
#### WIP, last updated: MAY 2026
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

**Track B — IPUMS household microdata (1980–2023):**
- `ipums-data-cleaning.R` → builds SQLite database from IPUMS extract
- `ipums-county-household-analysis.R` → county LFPR/hours panel + opposite-sex married spouse-pair micro file; *Section 6* adds HH composition descriptives (1-person, 2-person, spousal breakdown); *Section 7* adds STATEICP/COUNTYICP → FIPS crosswalk and merges spouse-pair data to political groups
- `ipums-married-household-suite.R` → multi-measure income analysis on spouse pairs; *Section 12* adds household-level hours × political group descriptives (requires Section 7 output)
- `ipums-county-female-lfpr-scatter.R`, `ipums-wage-quintile-time-graphs.R`, `ipums-spouse-income-scatter-plots.R` → additional IPUMS graphs

**Regressions:**
- `ipums-ols-regressions.R` → descriptive OLS: county-level (LFPR ~ income × vote_margin + FEs) and household-level (wife hours ~ income quintile × conservative + year FE + children)

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
6. Run `ipmus-data-cleaning.R` → `data/interim/ipums_data.sqlite`.
7. Run `ipums-county-household-analysis.R` → county LFPR panel, spouse-pair micro file, HH composition summary (Section 6), and political merge (Section 7).
   - Section 7 requires `lfpr_panel_with_groups.csv` from step 5.
   - Output: `ipums_married_oppositesex_spouse_pairs_with_groups.csv`
8. `ipums-married-household-suite.R` is sourced automatically at the end of step 7 (or run independently).
   - Section 12 generates household-level political × income descriptives.

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
- `data/processed/results/YYYY-MM-DD_lfpr_model_summary.csv`
- `data/processed/results/YYYY-MM-DD_lfpr_model_coefficients.csv`
- `data/processed/results/YYYY-MM-DD_lfpr_model_tables.tex`
- `data/processed/results/YYYY-MM-DD_lfpr_group_stats.csv`
- `data/processed/results/YYYY-MM-DD_lfpr_group_year_stats.csv`
- `data/graphs/YYYY-MM-DD_*.png`

#### Data and reference sources
- Princeton Elections LibGuide (data references and archives): [https://libguides.princeton.edu/elections#s-lg-box-10082744](https://libguides.princeton.edu/elections#s-lg-box-10082744)
- MIT Election Data and Science Lab (MEDSL): [https://electionlab.mit.edu/data#data](https://electionlab.mit.edu/data#data)
- Urban Institute `urbnmapr` (future source for county/state mapping workflows): [https://urbaninstitute.github.io/urbnmapr/](https://urbaninstitute.github.io/urbnmapr/)
- `tidycensus` documentation: [https://walker-data.com/tidycensus/](https://walker-data.com/tidycensus/)
- Census API / ACS 5-year docs: [https://www.census.gov/data/developers/data-sets/acs-5year.html](https://www.census.gov/data/developers/data-sets/acs-5year.html)
- ACS program overview: [https://www.census.gov/programs-surveys/acs](https://www.census.gov/programs-surveys/acs)
- FRED API docs: [https://fred.stlouisfed.org/docs/api/fred/](https://fred.stlouisfed.org/docs/api/fred/)
