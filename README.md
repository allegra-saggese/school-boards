### School boards, culture, and women who work
#### WIP, last updated: OCT 2025 
***

##### Project overview
Economic analysis of culture and its impact on female labor force 
participation rates in the US for high potential earners. We will
use an IV approach with novel school board data to show current 
culture effects economic decision making - beyond its functionality. 

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
