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
first_write <- TRUE

for (yr in years_keep) {
  message("Building spouse-pair households for year ", yr)

  pair_sql <- paste0(
    "WITH base AS (",
    "  SELECT YEAR, SAMPLE, SERIAL, PERNUM, STATEICP, COUNTYICP, AGE, SEX, SPLOC, HHWT, HHINCOME,",
    "         EMPSTAT, UHRSWORK, WKSWORK1, INCWAGE, INCTOT ",
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
    "    MAX(CASE WHEN SEX = 1 THEN INCTOT END) AS male_inctot ",
    "  FROM adults ",
    "  GROUP BY YEAR, SAMPLE, SERIAL ",
    "  HAVING SUM(CASE WHEN SEX = 2 THEN 1 ELSE 0 END) = 1 ",
    "     AND SUM(CASE WHEN SEX = 1 THEN 1 ELSE 0 END) = 1",
    "), pairs AS (",
    "  SELECT * ",
    "  FROM hh_pairs ",
    "  WHERE female_sploc = male_pernum ",
    "    AND male_sploc = female_pernum",
    ") ",
    "SELECT ",
    "  YEAR, SAMPLE, SERIAL, STATEICP, COUNTYICP, HHWT, HHINCOME,",
    "  female_pernum, male_pernum, female_empstat, male_empstat,",
    "  female_uhrswork_raw, male_uhrswork_raw, female_wkswork1_raw, male_wkswork1_raw,",
    "  female_incwage, male_incwage, female_inctot, male_inctot ",
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
      female_share_hh_income_total = ifelse(hhincome_nominal > 0, female_income_total_nonneg / hhincome_nominal, NA_real_),
      male_share_hh_income_total = ifelse(hhincome_nominal > 0, male_income_total_nonneg / hhincome_nominal, NA_real_),
      female_share_hh_income_wage = ifelse(hhincome_nominal > 0, female_income_wage_nonneg / hhincome_nominal, NA_real_),
      male_share_hh_income_wage = ifelse(hhincome_nominal > 0, male_income_wage_nonneg / hhincome_nominal, NA_real_),
      male_only_earner = as.integer(coalesce(female_income_wage_nonneg <= 0 & male_income_wage_nonneg > 0, FALSE)),
      female_only_earner = as.integer(coalesce(male_income_wage_nonneg <= 0 & female_income_wage_nonneg > 0, FALSE)),
      dual_earner = as.integer(coalesce(male_income_wage_nonneg > 0 & female_income_wage_nonneg > 0, FALSE))
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
      income_share_gap_wage_female_minus_male = female_share_hh_income_wage - male_share_hh_income_wage
    )

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
      male_only_earner_share = weighted_mean_safe(male_only_earner, HHWT),
      female_only_earner_share = weighted_mean_safe(female_only_earner, HHWT),
      dual_earner_share = weighted_mean_safe(dual_earner, HHWT),
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

message("IPUMS county-household pipeline complete.")
message("County metrics file: ", county_file)
message("Spouse pair file: ", pair_file)
message("Conditional file: ", conditional_file)
