### School boards, culture, and women who work
#### WIP, last updated: OCT 2025 
***

##### Project overview
Economic analysis of culture and its impact on female labor force 
participation rates in the US for high potential earners. This data is a first attempt
at generating stylized facts for individuals across incomes and understanding female 
labor force participation over time in the country. 

#### File structure 
*Note: all files and coding is in progress - early stages*
- IPUMS data cleaning file: containing US survey and census data, at following levels:
    - household level
    - female parent in household level
    - school district level
    - county level
- Ballotopedia data cleaning file: for scraping, compiling school board level elections, ideology, conflicts data
- To do file: for note taking by authors
- Functions file: for storage of loops and other common functions to be repeated in cleaning process, load into preamble of all files
- Prelim analysis file: using US census and BEA data to fit intra-country (county-level) U-curves for labor force participation (female) in the US

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
1. Configure paths:
   - Set `external_data_root` in `config.yml` to your Dropbox project root.
   - Place external files under `<external_data_root>/data/`.
2. Prepare IPUMS data:
   - Run `ipmus-data-cleaning.R` to unzip GRF files and build the SQLite database.
   - Output goes to `data/interim/ipums_data.sqlite`.
3. Build county panel inputs:
   - Run `fred-county-panel.R` (requires `FRED_API_KEY`) to download county series.
   - Output goes to `data/processed/fred/`.
4. Run preliminary analysis:
   - Run `10-2025-prelim-analysis.R` to load Census/BEA data from Dropbox, clean, merge, and run regressions.
   - Plots are automatically saved to `data/graphs/`.
5. Run panel pipeline:
   - Run `lfpr-panel-analysis.R` to pull ACS county LFPR + household income panel, merge presidential vote data with LOCF, run yearly linear/quadratic models, and generate plots.
   - Outputs are date-prefixed as `YYYY-MM-DD_*`.
   - Current default panel window is endpoint years `2010:2020`.

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
- Income regressor: `log(median_hh_income)` (ACS `B19013_001`)
- Election overlays:
  - `vote_margin`
  - `vote_spread`
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
- Plot families generated:
  - state-level yearly facets (`5x2` for `2010:2020`) per outcome
  - color variants by `vote_margin`, `vote_spread`, national quintile, state quintile
  - vector movement plots from 2010 to 2020
  - national election-year comparison facets (`2012`, `2016`, `2020`) with vertical national income quintile cutoff lines and color by:
    - `vote_spread`
    - `vote_margin`
    - `rep_percent`
    - `dem_percent`

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
- `data/processed/results/YYYY-MM-DD_lfpr_model_summary.csv`
- `data/processed/results/YYYY-MM-DD_lfpr_model_coefficients.csv`
- `data/processed/results/YYYY-MM-DD_lfpr_model_tables.tex`
- `data/graphs/YYYY-MM-DD_*.png`

#### Data and reference sources
- Princeton Elections LibGuide (data references and archives): [https://libguides.princeton.edu/elections#s-lg-box-10082744](https://libguides.princeton.edu/elections#s-lg-box-10082744)
- MIT Election Data and Science Lab (MEDSL): [https://electionlab.mit.edu/data#data](https://electionlab.mit.edu/data#data)
- Urban Institute `urbnmapr` (future source for county/state mapping workflows): [https://urbaninstitute.github.io/urbnmapr/](https://urbaninstitute.github.io/urbnmapr/)
- `tidycensus` documentation: [https://walker-data.com/tidycensus/](https://walker-data.com/tidycensus/)
- Census API / ACS 5-year docs: [https://www.census.gov/data/developers/data-sets/acs-5year.html](https://www.census.gov/data/developers/data-sets/acs-5year.html)
- ACS program overview: [https://www.census.gov/programs-surveys/acs](https://www.census.gov/programs-surveys/acs)
- FRED API docs: [https://fred.stlouisfed.org/docs/api/fred/](https://fred.stlouisfed.org/docs/api/fred/)
