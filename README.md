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
