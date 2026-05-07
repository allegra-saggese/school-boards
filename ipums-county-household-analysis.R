library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

source("R/paths.R")
source("functions.R")

# =========================================================
# 0) Configuration
# =========================================================
years_keep <- c(1980, 1990, 2005:2023)
county_age_min <- 20
county_age_max <- 64
spouse_age_min <- 25
spouse_age_max <- 64
income_bins <- 5L # household income quintiles

panel_dir <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
ensure_dir(panel_dir)
ensure_dir(results_dir)
ensure_dir(graphs_dir())

sqlite_path <- data_path("interim", "ipums_data.sqlite")
if (!file.exists(sqlite_path)) {
  stop("Missing SQLite file: ", sqlite_path)
}

# =========================================================
# 1) Helpers
# =========================================================
weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

weighted_quantile <- function(x, w, probs) {
  ok <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]
  w <- w[ok]
  if (length(x) == 0) return(rep(NA_real_, length(probs)))
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cum_w <- cumsum(w) / sum(w)
  sapply(probs, function(p) {
    idx <- which(cum_w >= p)[1]
    if (is.na(idx)) NA_real_ else x[idx]
  })
}

assign_weighted_bins <- function(x, w, n_bins = 5L) {
  probs <- seq(1 / n_bins, (n_bins - 1) / n_bins, by = 1 / n_bins)
  qs <- weighted_quantile(x, w, probs)
  if (any(is.na(qs))) {
    return(rep(NA_integer_, length(x)))
  }
  # Ensure strictly increasing cut points.
  for (i in 2:length(qs)) {
    if (qs[i] <= qs[i - 1]) {
      qs[i] <- qs[i - 1] + 1e-8
    }
  }
  breaks <- c(-Inf, qs, Inf)
  as.integer(cut(x, breaks = breaks, labels = FALSE, include.lowest = TRUE))
}

weighted_median_safe <- function(x, w) {
  weighted_quantile(x, w, probs = 0.5)[1]
}

write_csv_append <- function(df, path, first_write) {
  if (first_write) {
    write_csv(df, path)
  } else {
    write.table(
      df,
      file = path,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      append = TRUE,
      quote = TRUE,
      na = ""
    )
  }
}

# =========================================================
# 2) Connect DB
# =========================================================
con <- dbConnect(SQLite(), sqlite_path)
on.exit(dbDisconnect(con), add = TRUE)
dbExecute(con, "PRAGMA busy_timeout = 5000")

# Indexes to make spouse-link and county-year aggregation queries feasible on large tables.
build_indexes <- tolower(Sys.getenv("IPUMS_BUILD_INDEXES", "false")) == "true"
if (build_indexes) {
  message("Ensuring SQLite indexes...")
  index_statements <- c(
    "CREATE INDEX IF NOT EXISTS idx_ipums_year_geo ON ipums_table (YEAR, STATEICP, COUNTYICP)",
    "CREATE INDEX IF NOT EXISTS idx_ipums_hh ON ipums_table (YEAR, SAMPLE, SERIAL)",
    "CREATE INDEX IF NOT EXISTS idx_ipums_hh_person ON ipums_table (YEAR, SAMPLE, SERIAL, PERNUM)",
    "CREATE INDEX IF NOT EXISTS idx_ipums_hh_spouse ON ipums_table (YEAR, SAMPLE, SERIAL, SPLOC)",
    "CREATE INDEX IF NOT EXISTS idx_ipums_age_sex ON ipums_table (YEAR, AGE, SEX)"
  )

  for (stmt in index_statements) {
    tryCatch(
      dbExecute(con, stmt),
      error = function(e) {
        message("Skipping index build due lock/contention: ", conditionMessage(e))
      }
    )
  }
} else {
  message("Skipping SQLite index creation (set IPUMS_BUILD_INDEXES=true to enable).")
}

year_list_sql <- paste(years_keep, collapse = ",")

# =========================================================
# 3) County-year female/male LFPR and hours (PERWT)
# =========================================================
county_sql <- paste0(
  "WITH base AS (",
  "  SELECT YEAR, STATEICP, COUNTYICP, SEX, AGE, PERWT, EMPSTAT, UHRSWORK, WKSWORK1 ",
  "  FROM ipums_table ",
  "  WHERE YEAR IN (", year_list_sql, ") ",
  "    AND STATEICP IS NOT NULL ",
  "    AND COUNTYICP IS NOT NULL ",
  "    AND AGE BETWEEN ", county_age_min, " AND ", county_age_max,
  "), agg AS (",
  "  SELECT ",
  "    YEAR, STATEICP, COUNTYICP,",
  "    SUM(CASE WHEN SEX=2 THEN PERWT ELSE 0 END) AS female_pop_wt,",
  "    SUM(CASE WHEN SEX=1 THEN PERWT ELSE 0 END) AS male_pop_wt,",
  "    SUM(CASE WHEN SEX=2 AND EMPSTAT IN (1,2) THEN PERWT ELSE 0 END) AS female_lf_wt,",
  "    SUM(CASE WHEN SEX=1 AND EMPSTAT IN (1,2) THEN PERWT ELSE 0 END) AS male_lf_wt,",
  "    SUM(CASE WHEN SEX=2 AND EMPSTAT=1 AND UHRSWORK IS NOT NULL AND UHRSWORK>=0 THEN PERWT ELSE 0 END) AS female_emp_hours_wt,",
  "    SUM(CASE WHEN SEX=1 AND EMPSTAT=1 AND UHRSWORK IS NOT NULL AND UHRSWORK>=0 THEN PERWT ELSE 0 END) AS male_emp_hours_wt,",
  "    SUM(CASE WHEN SEX=2 AND EMPSTAT=1 AND UHRSWORK IS NOT NULL AND UHRSWORK>=0 THEN PERWT * UHRSWORK ELSE 0 END) AS female_weekly_hours_num,",
  "    SUM(CASE WHEN SEX=1 AND EMPSTAT=1 AND UHRSWORK IS NOT NULL AND UHRSWORK>=0 THEN PERWT * UHRSWORK ELSE 0 END) AS male_weekly_hours_num,",
  "    SUM(CASE WHEN SEX=2 AND EMPSTAT=1 AND UHRSWORK IS NOT NULL AND UHRSWORK>=0 AND WKSWORK1 IS NOT NULL AND WKSWORK1>=0 THEN PERWT ELSE 0 END) AS female_emp_annual_wt,",
  "    SUM(CASE WHEN SEX=1 AND EMPSTAT=1 AND UHRSWORK IS NOT NULL AND UHRSWORK>=0 AND WKSWORK1 IS NOT NULL AND WKSWORK1>=0 THEN PERWT ELSE 0 END) AS male_emp_annual_wt,",
  "    SUM(CASE WHEN SEX=2 AND EMPSTAT=1 AND UHRSWORK IS NOT NULL AND UHRSWORK>=0 AND WKSWORK1 IS NOT NULL AND WKSWORK1>=0 THEN PERWT * UHRSWORK * WKSWORK1 ELSE 0 END) AS female_annual_hours_num,",
  "    SUM(CASE WHEN SEX=1 AND EMPSTAT=1 AND UHRSWORK IS NOT NULL AND UHRSWORK>=0 AND WKSWORK1 IS NOT NULL AND WKSWORK1>=0 THEN PERWT * UHRSWORK * WKSWORK1 ELSE 0 END) AS male_annual_hours_num ",
  "  FROM base ",
  "  GROUP BY YEAR, STATEICP, COUNTYICP",
  ") ",
  "SELECT * FROM agg"
)

county_df <- dbGetQuery(con, county_sql) %>%
  mutate(
    female_lfpr = ifelse(female_pop_wt > 0, 100 * female_lf_wt / female_pop_wt, NA_real_),
    male_lfpr = ifelse(male_pop_wt > 0, 100 * male_lf_wt / male_pop_wt, NA_real_),
    lfpr_gap_male_minus_female = male_lfpr - female_lfpr,
    female_mean_weekly_hours_employed = ifelse(female_emp_hours_wt > 0, female_weekly_hours_num / female_emp_hours_wt, NA_real_),
    male_mean_weekly_hours_employed = ifelse(male_emp_hours_wt > 0, male_weekly_hours_num / male_emp_hours_wt, NA_real_),
    hours_gap_weekly_male_minus_female = male_mean_weekly_hours_employed - female_mean_weekly_hours_employed,
    female_mean_annual_hours_employed = ifelse(female_emp_annual_wt > 0, female_annual_hours_num / female_emp_annual_wt, NA_real_),
    male_mean_annual_hours_employed = ifelse(male_emp_annual_wt > 0, male_annual_hours_num / male_emp_annual_wt, NA_real_),
    hours_gap_annual_male_minus_female = male_mean_annual_hours_employed - female_mean_annual_hours_employed
  )

county_file <- file.path(panel_dir, "ipums_county_sex_lfpr_hours.csv")
write_csv(county_df, county_file)
message("County-level LFPR/hours panel written: ", county_file)

# Population-weighted county summary trends + county spread stats.
county_summary <- county_df %>%
  group_by(YEAR) %>%
  summarise(
    female_lfpr_pw_mean = weighted_mean_safe(female_lfpr, female_pop_wt),
    male_lfpr_pw_mean = weighted_mean_safe(male_lfpr, male_pop_wt),
    lfpr_gap_pw_mean = weighted_mean_safe(lfpr_gap_male_minus_female, female_pop_wt + male_pop_wt),
    female_lfpr_pw_median = weighted_median_safe(female_lfpr, female_pop_wt),
    male_lfpr_pw_median = weighted_median_safe(male_lfpr, male_pop_wt),
    lfpr_gap_pw_median = weighted_median_safe(lfpr_gap_male_minus_female, female_pop_wt + male_pop_wt),
    female_lfpr_county_min = suppressWarnings(min(female_lfpr, na.rm = TRUE)),
    female_lfpr_county_max = suppressWarnings(max(female_lfpr, na.rm = TRUE)),
    male_lfpr_county_min = suppressWarnings(min(male_lfpr, na.rm = TRUE)),
    male_lfpr_county_max = suppressWarnings(max(male_lfpr, na.rm = TRUE)),
    lfpr_gap_county_min = suppressWarnings(min(lfpr_gap_male_minus_female, na.rm = TRUE)),
    lfpr_gap_county_max = suppressWarnings(max(lfpr_gap_male_minus_female, na.rm = TRUE)),
    .groups = "drop"
  )

county_summary_file <- file.path(results_dir, "ipums_county_population_weighted_summaries.csv")
write_csv(county_summary, county_summary_file)
message("County population-weighted summary written: ", county_summary_file)

# =========================================================
# 4) Spouse-pair households (kids allowed; exactly two adults 25-64)
# =========================================================
pair_file <- file.path(panel_dir, "ipums_married_oppositesex_spouse_pairs_with_kids.csv")
if (file.exists(pair_file)) {
  file.remove(pair_file)
}

conditional_yearly <- list()
scatter_samples <- list()
first_write <- TRUE
set.seed(42)

for (yr in years_keep) {
  message("Building spouse-pair households for year ", yr)

  pair_sql <- paste0(
    "WITH base AS (",
    "  SELECT YEAR, SAMPLE, SERIAL, PERNUM, STATEICP, COUNTYICP, AGE, SEX, SPLOC, HHWT, HHINCOME,",
    "         EMPSTAT, UHRSWORK, WKSWORK1, INCWAGE, INCTOT, INCSS, INCWELFR, NCHILD ",
    "  FROM ipums_table ",
    "  WHERE YEAR = ", yr, " ",
    "    AND STATEICP IS NOT NULL ",
    "    AND COUNTYICP IS NOT NULL",
    "), hh_screen AS (",
    "  SELECT YEAR, SAMPLE, SERIAL,",
    "         SUM(CASE WHEN AGE BETWEEN ", spouse_age_min, " AND ", spouse_age_max, " THEN 1 ELSE 0 END) AS n_work_age,",
    "         SUM(CASE WHEN AGE >= ", spouse_age_min, " THEN 1 ELSE 0 END) AS n_ge_", spouse_age_min, " ",
    "  FROM base ",
    "  GROUP BY YEAR, SAMPLE, SERIAL ",
    "  HAVING n_work_age = 2 AND n_ge_", spouse_age_min, " = 2",
    "), adults AS (",
    "  SELECT b.* ",
    "  FROM base b ",
    "  JOIN hh_screen h ",
    "    ON b.YEAR=h.YEAR AND b.SAMPLE=h.SAMPLE AND b.SERIAL=h.SERIAL ",
    "  WHERE b.AGE BETWEEN ", spouse_age_min, " AND ", spouse_age_max,
    "), hh_transfers AS (",
    "  SELECT ",
    "    YEAR, SAMPLE, SERIAL,",
    "    SUM(CASE WHEN INCSS IS NOT NULL AND INCSS > 0 THEN INCSS ELSE 0 END) +",
    "    SUM(CASE WHEN INCWELFR IS NOT NULL AND INCWELFR > 0 THEN INCWELFR ELSE 0 END) AS hh_transfer_income ",
    "  FROM base ",
    "  GROUP BY YEAR, SAMPLE, SERIAL",
    "), hh_pairs AS (",
    "  SELECT ",
    "    YEAR, SAMPLE, SERIAL,",
    "    MAX(STATEICP) AS STATEICP,",
    "    MAX(COUNTYICP) AS COUNTYICP,",
    "    MAX(HHWT) AS HHWT,",
    "    MAX(HHINCOME) AS HHINCOME,",
    "    SUM(CASE WHEN SEX = 2 THEN 1 ELSE 0 END) AS n_female,",
    "    SUM(CASE WHEN SEX = 1 THEN 1 ELSE 0 END) AS n_male,",
    "    MAX(CASE WHEN SEX = 2 THEN PERNUM END) AS female_pernum,",
    "    MAX(CASE WHEN SEX = 1 THEN PERNUM END) AS male_pernum,",
    "    MAX(CASE WHEN SEX = 2 THEN SPLOC END) AS female_sploc,",
    "    MAX(CASE WHEN SEX = 1 THEN SPLOC END) AS male_sploc,",
    "    MAX(CASE WHEN SEX = 2 THEN EMPSTAT END) AS female_empstat,",
    "    MAX(CASE WHEN SEX = 1 THEN EMPSTAT END) AS male_empstat,",
    "    MAX(CASE WHEN SEX = 2 THEN UHRSWORK END) AS female_uhrswork_raw,",
    "    MAX(CASE WHEN SEX = 1 THEN UHRSWORK END) AS male_uhrswork_raw,",
    "    MAX(CASE WHEN SEX = 2 THEN WKSWORK1 END) AS female_wkswork1_raw,",
    "    MAX(CASE WHEN SEX = 1 THEN WKSWORK1 END) AS male_wkswork1_raw,",
    "    MAX(CASE WHEN SEX = 2 THEN INCWAGE END) AS female_incwage,",
    "    MAX(CASE WHEN SEX = 1 THEN INCWAGE END) AS male_incwage,",
    "    MAX(CASE WHEN SEX = 2 THEN INCTOT END) AS female_inctot,",
    "    MAX(CASE WHEN SEX = 1 THEN INCTOT END) AS male_inctot,",
    "    MAX(CASE WHEN SEX = 2 THEN INCSS END) AS female_incss,",
    "    MAX(CASE WHEN SEX = 1 THEN INCSS END) AS male_incss,",
    "    MAX(CASE WHEN SEX = 2 THEN INCWELFR END) AS female_incwelfr,",
    "    MAX(CASE WHEN SEX = 1 THEN INCWELFR END) AS male_incwelfr,",
    "    MAX(NCHILD) AS nchild ",
    "  FROM adults ",
    "  GROUP BY YEAR, SAMPLE, SERIAL ",
    "  HAVING SUM(CASE WHEN SEX = 2 THEN 1 ELSE 0 END) = 1 ",
    "     AND SUM(CASE WHEN SEX = 1 THEN 1 ELSE 0 END) = 1",
    "), pairs AS (",
    "  SELECT p.*, t.hh_transfer_income ",
    "  FROM hh_pairs p ",
    "  JOIN hh_transfers t ",
    "    ON p.YEAR=t.YEAR AND p.SAMPLE=t.SAMPLE AND p.SERIAL=t.SERIAL ",
    "  WHERE female_sploc = male_pernum ",
    "    AND male_sploc = female_pernum",
    ") ",
    "SELECT ",
    "  YEAR, SAMPLE, SERIAL, STATEICP, COUNTYICP, HHWT, HHINCOME,",
    "  female_pernum, male_pernum, female_empstat, male_empstat,",
    "  female_uhrswork_raw, male_uhrswork_raw, female_wkswork1_raw, male_wkswork1_raw,",
    "  female_incwage, male_incwage, female_inctot, male_inctot,",
    "  female_incss, male_incss, female_incwelfr, male_incwelfr, hh_transfer_income, nchild ",
    "FROM pairs"
  )

  pairs_year <- dbGetQuery(con, pair_sql) %>%
    mutate(
      hhincome_nominal = ifelse(!is.na(HHINCOME) & HHINCOME > 0, HHINCOME, NA_real_),
      female_weekly_hours = ifelse(female_empstat == 1 & !is.na(female_uhrswork_raw) & female_uhrswork_raw >= 0, female_uhrswork_raw, 0),
      male_weekly_hours = ifelse(male_empstat == 1 & !is.na(male_uhrswork_raw) & male_uhrswork_raw >= 0, male_uhrswork_raw, 0),
      female_wkswork1 = ifelse(female_empstat == 1 & !is.na(female_wkswork1_raw) & female_wkswork1_raw >= 0, female_wkswork1_raw, 0),
      male_wkswork1 = ifelse(male_empstat == 1 & !is.na(male_wkswork1_raw) & male_wkswork1_raw >= 0, male_wkswork1_raw, 0),
      female_annual_hours = female_weekly_hours * female_wkswork1,
      male_annual_hours = male_weekly_hours * male_wkswork1,
      female_income_total_nonneg = pmax(female_inctot, 0, na.rm = FALSE),
      male_income_total_nonneg = pmax(male_inctot, 0, na.rm = FALSE),
      female_income_wage_nonneg = pmax(female_incwage, 0, na.rm = FALSE),
      male_income_wage_nonneg = pmax(male_incwage, 0, na.rm = FALSE),
      female_transfer_income_nonneg = pmax(female_incss, 0, na.rm = FALSE) + pmax(female_incwelfr, 0, na.rm = FALSE),
      male_transfer_income_nonneg = pmax(male_incss, 0, na.rm = FALSE) + pmax(male_incwelfr, 0, na.rm = FALSE),
      household_transfer_income_nonneg = pmax(hh_transfer_income, 0, na.rm = FALSE),
      female_income_no_transfers = pmax(female_income_total_nonneg - female_transfer_income_nonneg, 0, na.rm = FALSE),
      male_income_no_transfers = pmax(male_income_total_nonneg - male_transfer_income_nonneg, 0, na.rm = FALSE),
      household_income_no_transfers = ifelse(hhincome_nominal > 0, pmax(hhincome_nominal - household_transfer_income_nonneg, 0), NA_real_),
      female_share_hh_income_total = ifelse(hhincome_nominal > 0, female_income_total_nonneg / hhincome_nominal, NA_real_),
      male_share_hh_income_total = ifelse(hhincome_nominal > 0, male_income_total_nonneg / hhincome_nominal, NA_real_),
      female_share_hh_income_wage = ifelse(hhincome_nominal > 0, female_income_wage_nonneg / hhincome_nominal, NA_real_),
      male_share_hh_income_wage = ifelse(hhincome_nominal > 0, male_income_wage_nonneg / hhincome_nominal, NA_real_),
      female_share_hh_income_no_transfers = ifelse(household_income_no_transfers > 0, female_income_no_transfers / household_income_no_transfers, NA_real_),
      male_share_hh_income_no_transfers = ifelse(household_income_no_transfers > 0, male_income_no_transfers / household_income_no_transfers, NA_real_),
      male_only_earner = as.integer(coalesce(female_income_wage_nonneg <= 0 & male_income_wage_nonneg > 0, FALSE)),
      female_only_earner = as.integer(coalesce(male_income_wage_nonneg <= 0 & female_income_wage_nonneg > 0, FALSE)),
      dual_earner = as.integer(coalesce(male_income_wage_nonneg > 0 & female_income_wage_nonneg > 0, FALSE)),
      nchild = as.integer(nchild),
      has_children = as.integer(coalesce(nchild > 0, FALSE))
    )

  message("Year ", yr, ": extracted ", nrow(pairs_year), " spouse pairs")

  if (nrow(pairs_year) == 0) {
    next
  }

  pairs_year <- pairs_year %>%
    mutate(
      income_quintile = assign_weighted_bins(hhincome_nominal, HHWT, n_bins = income_bins),
      hours_gap_weekly_female_minus_male = female_weekly_hours - male_weekly_hours,
      hours_gap_annual_female_minus_male = female_annual_hours - male_annual_hours,
      income_share_gap_total_female_minus_male = female_share_hh_income_total - male_share_hh_income_total,
      income_share_gap_wage_female_minus_male = female_share_hh_income_wage - male_share_hh_income_wage,
      income_gap_no_transfers_female_minus_male = female_income_no_transfers - male_income_no_transfers,
      income_share_gap_no_transfers_female_minus_male = female_share_hh_income_no_transfers - male_share_hh_income_no_transfers
    )

  scatter_n <- min(6000L, nrow(pairs_year))
  if (scatter_n > 0) {
    scatter_samples[[as.character(yr)]] <- pairs_year %>%
      filter(
        !is.na(hhincome_nominal),
        !is.na(male_income_no_transfers),
        hhincome_nominal > 0
      ) %>%
      sample_n(size = min(scatter_n, n()), replace = FALSE) %>%
      select(YEAR, income_quintile, hhincome_nominal, household_income_no_transfers, male_income_no_transfers)
  }

  write_csv_append(pairs_year, pair_file, first_write = first_write)
  first_write <- FALSE

  yearly_summary <- pairs_year %>%
    filter(!is.na(income_quintile)) %>%
    group_by(YEAR, income_quintile) %>%
    summarise(
      households_wt = sum(HHWT, na.rm = TRUE),
      female_weekly_hours_mean = weighted_mean_safe(female_weekly_hours, HHWT),
      male_weekly_hours_mean = weighted_mean_safe(male_weekly_hours, HHWT),
      weekly_hours_gap_female_minus_male = weighted_mean_safe(hours_gap_weekly_female_minus_male, HHWT),
      female_annual_hours_mean = weighted_mean_safe(female_annual_hours, HHWT),
      male_annual_hours_mean = weighted_mean_safe(male_annual_hours, HHWT),
      annual_hours_gap_female_minus_male = weighted_mean_safe(hours_gap_annual_female_minus_male, HHWT),
      female_share_hh_income_total_mean = weighted_mean_safe(female_share_hh_income_total, HHWT),
      male_share_hh_income_total_mean = weighted_mean_safe(male_share_hh_income_total, HHWT),
      female_share_hh_income_wage_mean = weighted_mean_safe(female_share_hh_income_wage, HHWT),
      male_share_hh_income_wage_mean = weighted_mean_safe(male_share_hh_income_wage, HHWT),
      female_income_no_transfers_mean = weighted_mean_safe(female_income_no_transfers, HHWT),
      male_income_no_transfers_mean = weighted_mean_safe(male_income_no_transfers, HHWT),
      household_income_nominal_mean = weighted_mean_safe(hhincome_nominal, HHWT),
      household_income_no_transfers_mean = weighted_mean_safe(household_income_no_transfers, HHWT),
      female_share_hh_income_no_transfers_mean = weighted_mean_safe(female_share_hh_income_no_transfers, HHWT),
      male_share_hh_income_no_transfers_mean = weighted_mean_safe(male_share_hh_income_no_transfers, HHWT),
      male_only_earner_share = weighted_mean_safe(male_only_earner, HHWT),
      female_only_earner_share = weighted_mean_safe(female_only_earner, HHWT),
      dual_earner_share = weighted_mean_safe(dual_earner, HHWT),
      has_children_share = weighted_mean_safe(has_children, HHWT),
      nchild_mean = weighted_mean_safe(nchild, HHWT),
      .groups = "drop"
    ) %>%
    rename(year = YEAR)

  conditional_yearly[[as.character(yr)]] <- yearly_summary
}

if (length(conditional_yearly) == 0) {
  stop("No spouse-pair households found after filters.")
}

conditional_by_year <- bind_rows(conditional_yearly) %>%
  arrange(year, income_quintile) %>%
  mutate(scope = "year")

conditional_pooled <- conditional_by_year %>%
  group_by(income_quintile) %>%
  summarise(
    households_wt = sum(households_wt, na.rm = TRUE),
    female_weekly_hours_mean = weighted_mean_safe(female_weekly_hours_mean, households_wt),
    male_weekly_hours_mean = weighted_mean_safe(male_weekly_hours_mean, households_wt),
    weekly_hours_gap_female_minus_male = weighted_mean_safe(weekly_hours_gap_female_minus_male, households_wt),
    female_annual_hours_mean = weighted_mean_safe(female_annual_hours_mean, households_wt),
    male_annual_hours_mean = weighted_mean_safe(male_annual_hours_mean, households_wt),
    annual_hours_gap_female_minus_male = weighted_mean_safe(annual_hours_gap_female_minus_male, households_wt),
    female_share_hh_income_total_mean = weighted_mean_safe(female_share_hh_income_total_mean, households_wt),
    male_share_hh_income_total_mean = weighted_mean_safe(male_share_hh_income_total_mean, households_wt),
    female_share_hh_income_wage_mean = weighted_mean_safe(female_share_hh_income_wage_mean, households_wt),
    male_share_hh_income_wage_mean = weighted_mean_safe(male_share_hh_income_wage_mean, households_wt),
    female_income_no_transfers_mean = weighted_mean_safe(female_income_no_transfers_mean, households_wt),
    male_income_no_transfers_mean = weighted_mean_safe(male_income_no_transfers_mean, households_wt),
    household_income_nominal_mean = weighted_mean_safe(household_income_nominal_mean, households_wt),
    household_income_no_transfers_mean = weighted_mean_safe(household_income_no_transfers_mean, households_wt),
    female_share_hh_income_no_transfers_mean = weighted_mean_safe(female_share_hh_income_no_transfers_mean, households_wt),
    male_share_hh_income_no_transfers_mean = weighted_mean_safe(male_share_hh_income_no_transfers_mean, households_wt),
    male_only_earner_share = weighted_mean_safe(male_only_earner_share, households_wt),
    female_only_earner_share = weighted_mean_safe(female_only_earner_share, households_wt),
    dual_earner_share = weighted_mean_safe(dual_earner_share, households_wt),
    .groups = "drop"
  ) %>%
  mutate(
    scope = "pooled",
    year = NA_integer_
  ) %>%
  select(scope, year, everything())

conditional_out <- bind_rows(
  conditional_by_year %>% select(scope, year, everything()),
  conditional_pooled
)

conditional_file <- file.path(results_dir, "ipums_conditional_spouse_hours_income_quintile.csv")
write_csv(conditional_out, conditional_file)

if (length(scatter_samples) > 0) {
  scatter_sample_out <- bind_rows(scatter_samples)
} else {
  scatter_sample_out <- tibble(
    YEAR = integer(),
    income_quintile = integer(),
    hhincome_nominal = numeric(),
    household_income_no_transfers = numeric(),
    male_income_no_transfers = numeric()
  )
}
scatter_sample_file <- file.path(results_dir, "ipums_household_income_vs_male_income_no_transfers_scatter_sample.csv")
write_csv(scatter_sample_out, scatter_sample_file)

# =========================================================
# 5) Graphs
# =========================================================
cond_year <- conditional_by_year

hours_long <- cond_year %>%
  select(year, income_quintile, female_weekly_hours_mean, male_weekly_hours_mean) %>%
  pivot_longer(
    cols = c(female_weekly_hours_mean, male_weekly_hours_mean),
    names_to = "sex",
    values_to = "weekly_hours"
  ) %>%
  mutate(
    sex = recode(
      sex,
      female_weekly_hours_mean = "Female",
      male_weekly_hours_mean = "Male"
    )
  )

p_hours <- ggplot(hours_long, aes(x = year, y = weekly_hours, color = sex)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~income_quintile, ncol = 3) +
  labs(
    title = "Spouse weekly hours over time by household income quintile",
    x = "Year",
    y = "Weekly hours",
    color = "Sex"
  ) +
  theme_minimal(base_size = 11)
save_plot("ipums_spouse_weekly_hours_over_time_by_income_quintile.png", { print(p_hours) }, width = 2200, height = 1400)

share_long_total <- cond_year %>%
  select(year, income_quintile, female_share_hh_income_total_mean, male_share_hh_income_total_mean) %>%
  pivot_longer(
    cols = c(female_share_hh_income_total_mean, male_share_hh_income_total_mean),
    names_to = "sex",
    values_to = "share"
  ) %>%
  mutate(
    sex = recode(
      sex,
      female_share_hh_income_total_mean = "Female",
      male_share_hh_income_total_mean = "Male"
    )
  )

p_share_total <- ggplot(share_long_total, aes(x = year, y = share, color = sex)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~income_quintile, ncol = 3) +
  labs(
    title = "Spouse share of household total income over time by income quintile",
    x = "Year",
    y = "Share of household income",
    color = "Sex"
  ) +
  theme_minimal(base_size = 11)
save_plot("ipums_spouse_income_share_total_over_time_by_income_quintile.png", { print(p_share_total) }, width = 2200, height = 1400)

share_long_wage <- cond_year %>%
  select(year, income_quintile, female_share_hh_income_wage_mean, male_share_hh_income_wage_mean) %>%
  pivot_longer(
    cols = c(female_share_hh_income_wage_mean, male_share_hh_income_wage_mean),
    names_to = "sex",
    values_to = "share"
  ) %>%
  mutate(
    sex = recode(
      sex,
      female_share_hh_income_wage_mean = "Female",
      male_share_hh_income_wage_mean = "Male"
    )
  )

p_share_wage <- ggplot(share_long_wage, aes(x = year, y = share, color = sex)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~income_quintile, ncol = 3) +
  labs(
    title = "Spouse share of household income from wage earnings over time by income quintile",
    x = "Year",
    y = "Share of household income",
    color = "Sex"
  ) +
  theme_minimal(base_size = 11)
save_plot("ipums_spouse_income_share_wage_over_time_by_income_quintile.png", { print(p_share_wage) }, width = 2200, height = 1400)

p_income_no_transfer <- cond_year %>%
  select(year, income_quintile, female_income_no_transfers_mean, male_income_no_transfers_mean) %>%
  pivot_longer(
    cols = c(female_income_no_transfers_mean, male_income_no_transfers_mean),
    names_to = "sex",
    values_to = "income_no_transfers"
  ) %>%
  mutate(
    sex = recode(
      sex,
      female_income_no_transfers_mean = "Female",
      male_income_no_transfers_mean = "Male"
    )
  ) %>%
  ggplot(aes(x = year, y = income_no_transfers, color = sex)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~income_quintile, ncol = 3) +
  labs(
    title = "Spouse personal income (no transfers) over time by income quintile",
    x = "Year",
    y = "Income (nominal dollars)",
    color = "Sex"
  ) +
  theme_minimal(base_size = 11)
save_plot("ipums_spouse_personal_income_no_transfers_over_time_by_income_quintile.png", { print(p_income_no_transfer) }, width = 2200, height = 1400)

share_long_no_transfer <- cond_year %>%
  select(year, income_quintile, female_share_hh_income_no_transfers_mean, male_share_hh_income_no_transfers_mean) %>%
  pivot_longer(
    cols = c(female_share_hh_income_no_transfers_mean, male_share_hh_income_no_transfers_mean),
    names_to = "sex",
    values_to = "share"
  ) %>%
  mutate(
    sex = recode(
      sex,
      female_share_hh_income_no_transfers_mean = "Female",
      male_share_hh_income_no_transfers_mean = "Male"
    )
  )

p_share_no_transfer <- ggplot(share_long_no_transfer, aes(x = year, y = share, color = sex)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~income_quintile, ncol = 3) +
  labs(
    title = "Spouse share of household income (no transfers) over time by income quintile",
    x = "Year",
    y = "Share of household income (no transfers)",
    color = "Sex"
  ) +
  theme_minimal(base_size = 11)
save_plot("ipums_spouse_income_share_no_transfers_over_time_by_income_quintile.png", { print(p_share_no_transfer) }, width = 2200, height = 1400)

p_gap <- ggplot(cond_year, aes(x = year, y = weekly_hours_gap_female_minus_male, color = factor(income_quintile))) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  labs(
    title = "Female-minus-male spouse weekly hours gap over time",
    x = "Year",
    y = "Weekly hours gap (female - male)",
    color = "Income quintile"
  ) +
  theme_minimal(base_size = 11)
save_plot("ipums_spouse_weekly_hours_gap_over_time_by_income_quintile.png", { print(p_gap) }, width = 1800, height = 1100)

earner_long <- cond_year %>%
  select(year, income_quintile, male_only_earner_share, female_only_earner_share, dual_earner_share) %>%
  pivot_longer(
    cols = c(male_only_earner_share, female_only_earner_share, dual_earner_share),
    names_to = "earner_type",
    values_to = "share"
  ) %>%
  mutate(
    earner_type = recode(
      earner_type,
      male_only_earner_share = "Male-only",
      female_only_earner_share = "Female-only",
      dual_earner_share = "Dual-earner"
    )
  )

p_earner <- ggplot(earner_long, aes(x = year, y = share, color = earner_type)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~income_quintile, ncol = 3) +
  labs(
    title = "Earner structure over time by household income quintile",
    x = "Year",
    y = "Household share",
    color = "Type"
  ) +
  theme_minimal(base_size = 11)
save_plot("ipums_spouse_earner_structure_over_time_by_income_quintile.png", { print(p_earner) }, width = 2200, height = 1400)

county_long <- county_summary %>%
  select(
    year = YEAR,
    female_lfpr_pw_mean,
    male_lfpr_pw_mean,
    lfpr_gap_pw_mean
  ) %>%
  pivot_longer(
    cols = c(female_lfpr_pw_mean, male_lfpr_pw_mean, lfpr_gap_pw_mean),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      female_lfpr_pw_mean = "Female LFPR",
      male_lfpr_pw_mean = "Male LFPR",
      lfpr_gap_pw_mean = "LFPR gap (male-female)"
    )
  )

p_county <- ggplot(county_long, aes(x = year, y = value, color = metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  labs(
    title = "Population-weighted county LFPR trends",
    x = "Year",
    y = "Rate / gap (percentage points)",
    color = "Metric"
  ) +
  theme_minimal(base_size = 11)
save_plot("ipums_county_population_weighted_lfpr_trends.png", { print(p_county) }, width = 1800, height = 1100)

if (nrow(scatter_sample_out) > 0) {
  x_cap <- suppressWarnings(as.numeric(quantile(scatter_sample_out$hhincome_nominal, probs = 0.99, na.rm = TRUE)))
  y_cap <- suppressWarnings(as.numeric(quantile(scatter_sample_out$male_income_no_transfers, probs = 0.99, na.rm = TRUE)))

  p_scatter <- ggplot(
    scatter_sample_out,
    aes(x = hhincome_nominal, y = male_income_no_transfers)
  ) +
    geom_point(alpha = 0.08, size = 0.6, color = "#1f77b4") +
    coord_cartesian(
      xlim = c(0, x_cap),
      ylim = c(0, y_cap)
    ) +
    labs(
      title = "Male spouse personal income (no transfers) vs household income",
      subtitle = "Scatter uses a random sample of spouse-pair households; axes clipped at 99th percentile",
      x = "Household income (nominal dollars)",
      y = "Male spouse personal income, no transfers (nominal dollars)"
    ) +
    theme_minimal(base_size = 11)

  save_plot("ipums_scatter_household_income_vs_male_income_no_transfers.png", { print(p_scatter) }, width = 1800, height = 1200)
}

# =========================================================
# 6) Household composition descriptives
# =========================================================
# Population summary: how large is the universe of households relative
# to the spouse-pair sample we study?
# Counts: all HH, 1-person HH, 2-person HH, spousal HH by work status.
# Spousal HH = HH with at least one opposite-sex married couple
# (detected via SPLOC link for both sexes).

comp_sql <- paste0(
  "WITH hh_base AS (",
  "  SELECT YEAR, SAMPLE, SERIAL,",
  "         MAX(NUMPREC) AS numprec,",
  "         MAX(HHWT) AS hhwt,",
  "         SUM(CASE WHEN SPLOC > 0 AND SEX = 2 AND AGE BETWEEN 18 AND 70 THEN 1 ELSE 0 END) AS n_wife_linked,",
  "         SUM(CASE WHEN SPLOC > 0 AND SEX = 1 AND AGE BETWEEN 18 AND 70 THEN 1 ELSE 0 END) AS n_husb_linked,",
  "         MAX(CASE WHEN SPLOC > 0 AND SEX = 2 AND AGE BETWEEN 18 AND 70 THEN EMPSTAT END) AS wife_empstat,",
  "         MAX(CASE WHEN SPLOC > 0 AND SEX = 1 AND AGE BETWEEN 18 AND 70 THEN EMPSTAT END) AS husb_empstat ",
  "  FROM ipums_table ",
  "  WHERE YEAR IN (", paste(years_keep, collapse = ","), ") AND GQ IN (1, 2) ",
  "  GROUP BY YEAR, SAMPLE, SERIAL",
  ") ",
  "SELECT ",
  "  YEAR,",
  "  SUM(hhwt) AS n_hh_total,",
  "  SUM(CASE WHEN numprec = 1 THEN hhwt ELSE 0 END) AS n_hh_single_person,",
  "  SUM(CASE WHEN numprec = 2 THEN hhwt ELSE 0 END) AS n_hh_two_person,",
  "  SUM(CASE WHEN n_wife_linked >= 1 AND n_husb_linked >= 1 THEN hhwt ELSE 0 END) AS n_hh_spousal,",
  "  SUM(CASE WHEN n_wife_linked >= 1 AND n_husb_linked >= 1",
  "           AND wife_empstat = 1 AND husb_empstat = 1 THEN hhwt ELSE 0 END) AS n_spousal_both_working,",
  "  SUM(CASE WHEN n_wife_linked >= 1 AND n_husb_linked >= 1",
  "           AND wife_empstat = 1 AND (husb_empstat IS NULL OR husb_empstat != 1) THEN hhwt ELSE 0 END) AS n_spousal_female_only,",
  "  SUM(CASE WHEN n_wife_linked >= 1 AND n_husb_linked >= 1",
  "           AND husb_empstat = 1 AND (wife_empstat IS NULL OR wife_empstat != 1) THEN hhwt ELSE 0 END) AS n_spousal_male_only,",
  "  SUM(CASE WHEN n_wife_linked >= 1 AND n_husb_linked >= 1",
  "           AND (wife_empstat IS NULL OR wife_empstat != 1)",
  "           AND (husb_empstat IS NULL OR husb_empstat != 1) THEN hhwt ELSE 0 END) AS n_spousal_neither ",
  "FROM hh_base ",
  "GROUP BY YEAR ",
  "ORDER BY YEAR"
)

comp_df <- dbGetQuery(con, comp_sql) %>%
  mutate(
    pct_single_person  = 100 * n_hh_single_person / n_hh_total,
    pct_two_person     = 100 * n_hh_two_person    / n_hh_total,
    pct_spousal        = 100 * n_hh_spousal        / n_hh_total,
    # Within spousal HH: shares of each work status (must sum to ~1)
    pct_both_working   = 100 * n_spousal_both_working  / n_hh_spousal,
    pct_female_only    = 100 * n_spousal_female_only   / n_hh_spousal,
    pct_male_only      = 100 * n_spousal_male_only     / n_hh_spousal,
    pct_neither        = 100 * n_spousal_neither        / n_hh_spousal,
    pct_work_status_sum = pct_both_working + pct_female_only + pct_male_only + pct_neither
  )

# Validation: within-spousal work status shares should sum to ~100%
max_share_err <- max(abs(comp_df$pct_work_status_sum - 100), na.rm = TRUE)
message("HH composition — work status share sum max error: ", round(max_share_err, 2), "pp (should be ~0)")

comp_file <- file.path(results_dir, "ipums_hh_composition_summary.csv")
write_csv(comp_df, comp_file)
message("Household composition summary written: ", comp_file)

# Plot: spousal HH work status shares over time (stacked area)
comp_long <- comp_df %>%
  select(YEAR, pct_both_working, pct_female_only, pct_male_only, pct_neither) %>%
  pivot_longer(
    cols = c(pct_both_working, pct_female_only, pct_male_only, pct_neither),
    names_to = "status",
    values_to = "pct"
  ) %>%
  mutate(
    status = recode(status,
      pct_both_working = "Both working",
      pct_female_only  = "Female-only earner",
      pct_male_only    = "Male-only earner",
      pct_neither      = "Neither working"
    ),
    status = factor(status, levels = c("Both working", "Female-only earner",
                                       "Male-only earner", "Neither working"))
  )

p_comp <- ggplot(comp_long, aes(x = YEAR, y = pct, fill = status)) +
  geom_area() +
  labs(
    title    = "Work status composition of spousal households over time",
    subtitle = "All opposite-sex spousal households (broad), ages 18-70; IPUMS micro-data",
    x = "Year", y = "Share of spousal HH (%)", fill = "Work status"
  ) +
  theme_minimal(base_size = 11)
save_plot("ipums_hh_composition_spousal_work_status_over_time.png",
          { print(p_comp) }, width = 1800, height = 1100)

# =========================================================
# 7) Merge spouse-pair data to county political groups
# =========================================================
# Builds: ipums_married_oppositesex_spouse_pairs_with_groups.csv
# Join key: STATEICP + COUNTYICP → FIPS state + county code → panel FIPS string.
#
# STATEICP → FIPS state: standard IPUMS ICP code crosswalk (hardcoded below,
# validated for CA(71), TX(49), WI(25), MD(52), VA(40), WV(56), AK(81), DC(98)).
# COUNTYICP → FIPS county: COUNTYICP / 10 maps to 3-digit FIPS county code
# for post-1980 samples; minor historical exceptions may produce NA matches.

stateicp_fips_xwalk <- data.frame(
  STATEICP  = c(
     1,  2,  3,  4,  5,  6,          # CT  ME  MA  NH  RI  VT
    11, 12, 13, 14,                   # DE  NJ  NY  PA
    21, 22, 23, 24, 25,               # IL  IN  MI  OH  WI
    31, 32, 33, 34, 35, 36, 37,       # IA  KS  MN  MO  NE  ND  SD
    40, 41, 42, 43, 44, 45, 46, 47, 48, 49,  # VA  AL  AR  FL  GA  LA  MS  NC  SC  TX
    51, 52, 53, 54, 56,               # KY  MD  OK  TN  WV
    61, 62, 63, 64, 65, 66, 67, 68,  # AZ  CO  ID  MT  NV  NM  UT  WY
    71, 72, 73,                       # CA  OR  WA
    81, 82,                           # AK  HI
    98                                # DC
  ),
  state_fips = c(
     9, 23, 25, 33, 44, 50,
    10, 34, 36, 42,
    17, 18, 26, 39, 55,
    19, 20, 27, 29, 31, 38, 46,
    51,  1,  5, 12, 13, 22, 28, 37, 45, 48,
    21, 24, 40, 47, 54,
     4,  8, 16, 30, 32, 35, 49, 56,
     6, 41, 53,
     2, 15,
    11
  ),
  stringsAsFactors = FALSE
)

# Build FIPS string from STATEICP and COUNTYICP
pairs_raw <- read_csv(pair_file, show_col_types = FALSE)

pairs_with_fips <- pairs_raw %>%
  left_join(stateicp_fips_xwalk, by = "STATEICP") %>%
  mutate(
    county_fips_int = as.integer(floor(COUNTYICP / 10)),
    fips = ifelse(
      !is.na(state_fips) & !is.na(county_fips_int) & county_fips_int > 0,
      sprintf("%02d%03d", state_fips, county_fips_int),
      NA_character_
    )
  )

match_rate <- mean(!is.na(pairs_with_fips$fips)) * 100
message("Spouse-pair FIPS match rate: ", round(match_rate, 1), "% of household-year observations")

# Load the political-income group panel (latest dated file)
groups_files <- list.files(
  data_path("processed", "panel"),
  pattern = "_lfpr_panel_with_groups\\.csv$",
  full.names = TRUE
)
if (length(groups_files) == 0) stop("No lfpr_panel_with_groups file found; run lfpr-groupings.R first.")
groups_panel <- read_csv(sort(groups_files)[length(groups_files)], show_col_types = FALSE) %>%
  select(fips, year, vote_margin, vote_spread, dem_percent, rep_percent,
         income_quintile_national, income_quintile_state,
         trad, asp_trad, dem_solid_poor, dem_solid_rich) %>%
  rename(YEAR = year)

# Merge: left join on (fips, YEAR) — LOCF already applied in groups_panel
pairs_merged <- pairs_with_fips %>%
  left_join(groups_panel, by = c("fips", "YEAR"))

group_match_rate <- mean(!is.na(pairs_merged$vote_margin)) * 100
message("Political group merge rate: ", round(group_match_rate, 1),
        "% of households matched to vote data")

# Validation: group flags should be mutually exclusive
overlap_n <- sum(
  (coalesce(pairs_merged$trad, 0) + coalesce(pairs_merged$asp_trad, 0) +
   coalesce(pairs_merged$dem_solid_poor, 0) + coalesce(pairs_merged$dem_solid_rich, 0)) > 1,
  na.rm = TRUE
)
message("Political group overlap check: ", overlap_n,
        " households flagged in more than one group (should be 0)")

merged_file <- file.path(panel_dir, "ipums_married_oppositesex_spouse_pairs_with_groups.csv")
write_csv(pairs_merged, merged_file)
message("Merged spouse-pair file written: ", merged_file)

message("IPUMS county-household pipeline complete.")
message("County metrics file: ", county_file)
message("Spouse pair file: ", pair_file)
message("Conditional file: ", conditional_file)
message("Scatter sample file: ", scatter_sample_file)
message("HH composition file: ", comp_file)
message("Merged political spouse-pair file: ", merged_file)

run_extended_suite <- tolower(Sys.getenv("RUN_IPUMS_MARRIED_SUITE", "true")) == "true"
if (run_extended_suite) {
  message("Running extended married-household suite...")
  source("ipums-married-household-suite.R")
}
