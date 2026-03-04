library(data.table)
library(ggplot2)

source("R/paths.R")
source("functions.R")

weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & !is.na(x) & is.finite(w) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

pair_file <- data_path("processed", "panel", "ipums_married_oppositesex_spouse_pairs_with_kids.csv")
results_dir <- data_path("processed", "results")
ensure_dir(results_dir)
ensure_dir(graphs_dir())

if (!file.exists(pair_file)) {
  stop("Missing pair file: ", pair_file)
}

message("Loading spouse-pair wage columns...")
dt <- fread(
  pair_file,
  select = c(
    "YEAR",
    "income_quintile",
    "HHWT",
    "female_income_wage_nonneg",
    "male_income_wage_nonneg",
    "household_income_no_transfers"
  )
)

dt <- dt[
  !is.na(income_quintile) &
    !is.na(HHWT) &
    HHWT > 0
]

agg <- dt[
  ,
  .(
    female_log_wage_mean = weighted_mean_safe(log1p(female_income_wage_nonneg), HHWT),
    male_log_wage_mean = weighted_mean_safe(log1p(male_income_wage_nonneg), HHWT),
    female_wage_share_no_transfers_mean = weighted_mean_safe(
      fifelse(household_income_no_transfers > 0, female_income_wage_nonneg / household_income_no_transfers, NA_real_),
      HHWT
    ),
    female_log_wage_share_no_transfers_mean = weighted_mean_safe(
      log1p(fifelse(household_income_no_transfers > 0, female_income_wage_nonneg / household_income_no_transfers, NA_real_)),
      HHWT
    )
  ),
  by = .(year = YEAR, income_quintile)
][order(year, income_quintile)]

agg_file <- file.path(results_dir, "ipums_wage_quintile_time_aggregates.csv")
fwrite(agg, agg_file)

long_wage <- melt(
  agg,
  id.vars = c("year", "income_quintile"),
  measure.vars = c("female_log_wage_mean", "male_log_wage_mean"),
  variable.name = "sex",
  value.name = "log_wage_mean"
)
long_wage[, sex := fifelse(sex == "female_log_wage_mean", "Female", "Male")]

p_wage <- ggplot(long_wage, aes(x = year, y = log_wage_mean, color = sex)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  facet_wrap(~income_quintile, ncol = 3) +
  labs(
    title = "Male vs female log wage earnings over time by income quintile",
    subtitle = "Metric is weighted mean log(1 + spouse wage earnings)",
    x = "Year",
    y = "Weighted mean log(1 + wage earnings)",
    color = "Spouse"
  ) +
  theme_minimal(base_size = 11)

save_plot(
  "ipums_spouse_log_wage_earnings_over_time_by_income_quintile.png",
  { print(p_wage) },
  width = 2200,
  height = 1400
)

p_share_log <- ggplot(agg, aes(x = year, y = female_log_wage_share_no_transfers_mean, color = factor(income_quintile))) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  labs(
    title = "Female log wage earnings share of household income (less transfers) over time",
    subtitle = "Metric is weighted mean log(1 + female_wage / household_income_no_transfers)",
    x = "Year",
    y = "Weighted mean log(1 + female wage share)",
    color = "Income quintile"
  ) +
  theme_minimal(base_size = 11)

save_plot(
  "ipums_female_log_wage_share_of_hh_income_no_transfers_over_time_by_income_quintile.png",
  { print(p_share_log) },
  width = 2200,
  height = 1400
)

message("Wrote aggregate data: ", agg_file)
