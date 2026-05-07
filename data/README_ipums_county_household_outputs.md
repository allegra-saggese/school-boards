# IPUMS County + Household Output Guide

Run date for these outputs: **March 3, 2026**  
Script: `/Users/allegrasaggese/Documents/GitHub/school-boards/ipums-county-household-analysis.R`

## Scope and sample rules

- Years included: `1980`, `1990`, `2005-2023`
- Geography required: non-missing `STATEICP` and `COUNTYICP`
- County LFPR/hours sample: people age `20-64`
- Spouse-household sample:
  - exactly two adults age `25-64`
  - one female + one male adult
  - reciprocal spouse links (`SPLOC`)
  - additional household members allowed only if under age 25 (kids/young dependents)

## Weights used

- Person-level county rates/hours use `PERWT`.
- Household/spouse conditional summaries use `HHWT`.

## Files and column dictionary

### 1) County sex panel
File: `/Users/allegrasaggese/Documents/GitHub/school-boards/data/processed/panel/ipums_county_sex_lfpr_hours.csv`

Unit of observation: **county-year**.

- `YEAR`, `STATEICP`, `COUNTYICP`: year and county identifiers.
- `female_pop_wt`, `male_pop_wt`: weighted population totals by sex (age 20-64).
- `female_lf_wt`, `male_lf_wt`: weighted labor-force totals (`EMPSTAT in (1,2)`).
- `female_emp_hours_wt`, `male_emp_hours_wt`: weighted counts among employed with valid weekly hours.
- `female_weekly_hours_num`, `male_weekly_hours_num`: weighted sum of weekly hours (`PERWT * UHRSWORK`) for employed.
- `female_emp_annual_wt`, `male_emp_annual_wt`: weighted counts among employed with valid weekly and weeks-worked values.
- `female_annual_hours_num`, `male_annual_hours_num`: weighted sum of annual hours (`PERWT * UHRSWORK * WKSWORK1`) for employed.
- `female_lfpr`, `male_lfpr`: labor force participation rate in percent.
- `lfpr_gap_male_minus_female`: male LFPR minus female LFPR (percentage points).
- `female_mean_weekly_hours_employed`, `male_mean_weekly_hours_employed`: average weekly hours among employed.
- `hours_gap_weekly_male_minus_female`: male minus female weekly-hours gap.
- `female_mean_annual_hours_employed`, `male_mean_annual_hours_employed`: average annual hours among employed.
- `hours_gap_annual_male_minus_female`: male minus female annual-hours gap.

### 2) County weighted national summary by year
File: `/Users/allegrasaggese/Documents/GitHub/school-boards/data/processed/results/ipums_county_population_weighted_summaries.csv`

Unit of observation: **year**.

- `YEAR`: year.
- `female_lfpr_pw_mean`, `male_lfpr_pw_mean`, `lfpr_gap_pw_mean`: population-weighted county means.
- `female_lfpr_pw_median`, `male_lfpr_pw_median`, `lfpr_gap_pw_median`: weighted county medians.
- `female_lfpr_county_min`, `female_lfpr_county_max`: min/max county female LFPR.
- `male_lfpr_county_min`, `male_lfpr_county_max`: min/max county male LFPR.
- `lfpr_gap_county_min`, `lfpr_gap_county_max`: min/max county gap (male - female).

### 3) Spouse-pair micro panel (with kids allowed)
File: `/Users/allegrasaggese/Documents/GitHub/school-boards/data/processed/panel/ipums_married_oppositesex_spouse_pairs_with_kids.csv`

Unit of observation: **eligible household-year spouse pair**.

- IDs and weights: `YEAR`, `SAMPLE`, `SERIAL`, `STATEICP`, `COUNTYICP`, `HHWT`.
- Raw household income: `HHINCOME`; cleaned positive value field: `hhincome_nominal`.
- Spouse IDs: `female_pernum`, `male_pernum`.
- Employment/raw work fields:
  - `female_empstat`, `male_empstat`
  - `female_uhrswork_raw`, `male_uhrswork_raw`
  - `female_wkswork1_raw`, `male_wkswork1_raw`
- Raw income fields:
  - `female_incwage`, `male_incwage`
  - `female_inctot`, `male_inctot`
- Constructed hours:
  - `female_weekly_hours`, `male_weekly_hours` (0 when not employed/invalid)
  - `female_wkswork1`, `male_wkswork1` (0 when not employed/invalid)
  - `female_annual_hours`, `male_annual_hours`
- Nonnegative income transforms:
  - `female_income_total_nonneg`, `male_income_total_nonneg`
  - `female_income_wage_nonneg`, `male_income_wage_nonneg`
- Income shares relative to `hhincome_nominal`:
  - `female_share_hh_income_total`, `male_share_hh_income_total`
  - `female_share_hh_income_wage`, `male_share_hh_income_wage`
- Earner-type indicators:
  - `male_only_earner`, `female_only_earner`, `dual_earner`
- Conditional-bin variable:
  - `income_quintile` (weighted quintile within year)
- Gap variables:
  - `hours_gap_weekly_female_minus_male`
  - `hours_gap_annual_female_minus_male`
  - `income_share_gap_total_female_minus_male`
  - `income_share_gap_wage_female_minus_male`

### 4) Conditional spouse summary by income quintile
File: `/Users/allegrasaggese/Documents/GitHub/school-boards/data/processed/results/ipums_conditional_spouse_hours_income_quintile.csv`

Unit of observation: **income-quintile-year** (`scope = year`) or **pooled income quintile** (`scope = pooled`).

- `scope`: `year` or `pooled`.
- `year`: year when `scope = year`; missing for pooled rows.
- `income_quintile`: 1 (lowest) to 5 (highest).
- `households_wt`: weighted household count.
- Mean hours:
  - `female_weekly_hours_mean`, `male_weekly_hours_mean`
  - `weekly_hours_gap_female_minus_male`
  - `female_annual_hours_mean`, `male_annual_hours_mean`
  - `annual_hours_gap_female_minus_male`
- Mean income shares:
  - `female_share_hh_income_total_mean`, `male_share_hh_income_total_mean`
  - `female_share_hh_income_wage_mean`, `male_share_hh_income_wage_mean`
- Earner-structure shares:
  - `male_only_earner_share`, `female_only_earner_share`, `dual_earner_share`

## Graph outputs

Directory: `/Users/allegrasaggese/Documents/GitHub/school-boards/data/graphs`

Files generated in this run:

- `2026-03-03_ipums_county_population_weighted_lfpr_trends.png`
- `2026-03-03_ipums_spouse_weekly_hours_over_time_by_income_quintile.png`
- `2026-03-03_ipums_spouse_weekly_hours_gap_over_time_by_income_quintile.png`
- `2026-03-03_ipums_spouse_income_share_total_over_time_by_income_quintile.png`
- `2026-03-03_ipums_spouse_income_share_wage_over_time_by_income_quintile.png`
- `2026-03-03_ipums_spouse_earner_structure_over_time_by_income_quintile.png`

## March 3 extension: multi-measure suite

Suite script: `/Users/allegrasaggese/Documents/GitHub/school-boards/ipums-married-household-suite.R`

### Income interpretation note

- `HHINCOME`, `INCTOT`, and `INCWAGE` are treated as annual-flow income variables in each sample year.
- Real-dollar outputs are rebased to **2020 dollars**.
- CPI setup:
  - 2010-2024 uses the in-repo R-CPI-U-RS annual values already used in ACS work.
  - 1980/1990/2005-2009 are backcast proxy values aligned to the 2010 R-CPI-U-RS level.

### Additional output files

1) Income measure grid + national-year bin thresholds  
File: `/Users/allegrasaggese/Documents/GitHub/school-boards/data/processed/results/ipums_income_measure_grid.csv`

- `conditioning_measure`: household income definition (`hh_income`, `hh_income_no_transfers`, `hh_wage_income`, `hh_hourly_wage`)
- `bin_count`: `5`, `10`, `20`
- `year`, `bin`: year and bin index
- `lower_nominal`, `upper_nominal`: nominal bin thresholds
- `lower_real`, `upper_real`: 2020-dollar bin thresholds
- `formula`: measure construction formula

2) Binned income curves (ventiles)  
File: `/Users/allegrasaggese/Documents/GitHub/school-boards/data/processed/results/ipums_binned_income_curves.csv`

- Unit: year × income ventile × conditioning measure × dollar basis
- Includes:
  - female/male spouse income means (total, wage, no-transfer)
  - female/male weekly and annual hours means
  - female/male working shares for `EMPSTAT==1`, `EMPSTAT in (1,2)`, and annual-hours-positive definitions
  - gap and ratio fields

3) Heatmap metrics (deciles)  
File: `/Users/allegrasaggese/Documents/GitHub/school-boards/data/processed/results/ipums_heatmap_metrics.csv`

- Unit: year × income decile × conditioning measure × dollar basis
- Includes:
  - female/male no-transfer household income shares
  - female-minus-male hours gap
  - composition shares (`zero`, `male_only`, `female_only`, `both`) for each working definition
  - female/male working-share ratio (`EMPSTAT==1`)

4) County married-pair conditional metrics  
File: `/Users/allegrasaggese/Documents/GitHub/school-boards/data/processed/results/ipums_county_married_pair_metrics.csv`

- Unit: county-year overall and county-year-income-quintile
- Includes:
  - spouse hours means and gaps
  - spouse no-transfer income means and gaps
  - female/male working shares and ratios
  - joined county LFPR ratio (`male_lfpr / female_lfpr`)

5) County LFPR ratio series  
File: `/Users/allegrasaggese/Documents/GitHub/school-boards/data/processed/results/ipums_county_lfpr_ratio_timeseries.csv`

- `scope = county`: county-year male/female LFPR ratio
- `scope = national`: population-weighted national trend over county ratios

6) Married-household composition over time  
File: `/Users/allegrasaggese/Documents/GitHub/school-boards/data/processed/results/ipums_married_household_composition_timeseries.csv`

- Unit: year overall or year × income quintile
- Dimensions:
  - conditioning measure
  - working definition (`emp`, `lf`, `hours`)
- Shares:
  - `zero_share`, `male_only_share`, `female_only_share`, `both_share`
  - female/male working shares and female/male ratio

7) Validation summary  
File: `/Users/allegrasaggese/Documents/GitHub/school-boards/data/processed/results/ipums_suite_validation_summary.csv`

- Identity/range checks
- Composition add-up checks
- County LFPR join coverage by year

### Additional graph families

Directory: `/Users/allegrasaggese/Documents/GitHub/school-boards/data/graphs`

- `2026-03-03_ipums_binned_curve_*`
- `2026-03-03_ipums_heatmap_*`
- `2026-03-03_ipums_county_cross_section_*`
- `2026-03-03_ipums_composition_*`
- `2026-03-03_ipums_county_lfpr_ratio_*`
