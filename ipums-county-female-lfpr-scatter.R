library(data.table)
library(ggplot2)

source("R/paths.R")
source("functions.R")

weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & !is.na(x) & is.finite(w) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

weighted_quantile <- function(x, w, probs) {
  ok <- is.finite(x) & !is.na(x) & is.finite(w) & !is.na(w) & w > 0
  x <- x[ok]
  w <- w[ok]
  if (!length(x)) return(rep(NA_real_, length(probs)))
  o <- order(x)
  x <- x[o]
  w <- w[o]
  cw <- cumsum(w) / sum(w)
  sapply(probs, function(p) x[which(cw >= p)[1]])
}

weighted_median_safe <- function(x, w) {
  weighted_quantile(x, w, 0.5)[1]
}

results_dir <- data_path("processed", "results")
panel_dir <- data_path("processed", "panel")

county_metrics_file <- file.path(results_dir, "ipums_county_married_pair_metrics.csv")
pair_file <- file.path(panel_dir, "ipums_married_oppositesex_spouse_pairs_with_kids.csv")

if (!file.exists(county_metrics_file)) stop("Missing: ", county_metrics_file)
if (!file.exists(pair_file)) stop("Missing: ", pair_file)

facet_years <- c(1980L, 1990L, 2005L, 2010L, 2015L, 2020L)
available_years <- facet_years

message("Reading county married-pair metrics...")
county_dt <- fread(
  county_metrics_file,
  select = c(
    "YEAR", "STATEICP", "COUNTYICP", "income_quintile",
    "conditioning_measure", "dollar_basis", "level", "households_wt",
    "female_lfpr", "female_work_share_empstat"
  )
)

county_dt <- county_dt[
  conditioning_measure == "hh_income" &
    dollar_basis == "nominal" &
    level == "quintile" &
    YEAR %in% available_years
]

message("Computing county-year-quintile HH income means/medians...")
pair_dt <- fread(
  pair_file,
  select = c("YEAR", "STATEICP", "COUNTYICP", "income_quintile", "HHWT", "hhincome_nominal")
)

pair_dt <- pair_dt[
  YEAR %in% available_years &
    !is.na(income_quintile) &
    !is.na(hhincome_nominal) &
    hhincome_nominal > 0
]

hh_quintile_stats <- pair_dt[
  ,
  .(
    hh_income_mean = weighted_mean_safe(hhincome_nominal, HHWT),
    hh_income_median = weighted_median_safe(hhincome_nominal, HHWT)
  ),
  by = .(YEAR, STATEICP, COUNTYICP, income_quintile)
]

plot_dt <- merge(
  county_dt,
  hh_quintile_stats,
  by = c("YEAR", "STATEICP", "COUNTYICP", "income_quintile"),
  all.x = TRUE
)

plot_dt[, `:=`(
  log_median_hh_income = fifelse(hh_income_median > 0, log(hh_income_median), NA_real_),
  year_facet = factor(YEAR, levels = facet_years)
)]

scatter_out_file <- file.path(results_dir, "ipums_county_female_lfpr_scatter_plot_data.csv")
fwrite(plot_dt, scatter_out_file)

lfpr_median_pts <- plot_dt[
  !is.na(log_median_hh_income) & !is.na(female_lfpr),
  .(
    x_median = weighted_median_safe(log_median_hh_income, households_wt),
    y_median = weighted_median_safe(female_lfpr, households_wt)
  ),
  by = .(year_facet, income_quintile)
]

p_lfpr <- ggplot(
  plot_dt[!is.na(log_median_hh_income) & !is.na(female_lfpr)],
  aes(x = log_median_hh_income, y = female_lfpr, color = factor(income_quintile))
) +
  geom_point(alpha = 0.28, size = 0.85) +
  geom_point(
    data = lfpr_median_pts,
    aes(x = x_median, y = y_median),
    inherit.aes = FALSE,
    color = "black",
    alpha = 0.95,
    size = 2.2
  ) +
  facet_wrap(~year_facet, scales = "free_x") +
  labs(
    title = "County female LFPR vs log median household income (quintile-colored)",
    subtitle = "Black points are weighted medians within each year×quintile facet",
    x = "log(median household income in county-year-quintile)",
    y = "Female LFPR (county, age 20-64)",
    color = "Income quintile"
  ) +
  theme_minimal(base_size = 11)

save_plot(
  "ipums_county_female_lfpr_scatter_log_median_hh_income_quintile_facets_1980_1990_2005_2010_2015_2020.png",
  { print(p_lfpr) },
  width = 2400,
  height = 1400
)

# Companion figure: within-married-pair female working share.
work_median_pts <- plot_dt[
  !is.na(log_median_hh_income) & !is.na(female_work_share_empstat),
  .(
    x_median = weighted_median_safe(log_median_hh_income, households_wt),
    y_median = weighted_median_safe(female_work_share_empstat, households_wt)
  ),
  by = .(year_facet, income_quintile)
]

p_work <- ggplot(
  plot_dt[!is.na(log_median_hh_income) & !is.na(female_work_share_empstat)],
  aes(x = log_median_hh_income, y = female_work_share_empstat, color = factor(income_quintile))
) +
  geom_point(alpha = 0.28, size = 0.85) +
  geom_point(
    data = work_median_pts,
    aes(x = x_median, y = y_median),
    inherit.aes = FALSE,
    color = "black",
    alpha = 0.95,
    size = 2.2
  ) +
  facet_wrap(~year_facet, scales = "free_x") +
  labs(
    title = "County female spouse working share vs log median household income (quintile-colored)",
    subtitle = "Female working share uses EMPSTAT==1 within married spouse pairs; black points are weighted medians",
    x = "log(median household income in county-year-quintile)",
    y = "Female spouse working share (EMPSTAT==1)",
    color = "Income quintile"
  ) +
  theme_minimal(base_size = 11)

save_plot(
  "ipums_county_female_work_share_scatter_log_median_hh_income_quintile_facets_1980_1990_2005_2010_2015_2020.png",
  { print(p_work) },
  width = 2400,
  height = 1400
)

message("Wrote scatter data: ", scatter_out_file)
