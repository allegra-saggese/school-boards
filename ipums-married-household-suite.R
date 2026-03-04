library(data.table)
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
anchor_years <- c(1980, 1990, 2005, 2010, 2015, 2020, 2023)
income_bin_counts <- c(5L, 10L, 20L) # quintile / decile / ventile
real_base_year <- 2020L

panel_dir <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
graphs_out_dir <- graphs_dir()
ensure_dir(results_dir)
ensure_dir(graphs_out_dir)

pair_file <- file.path(panel_dir, "ipums_married_oppositesex_spouse_pairs_with_kids.csv")
county_file <- file.path(panel_dir, "ipums_county_sex_lfpr_hours.csv")

if (!file.exists(pair_file)) stop("Missing pair file: ", pair_file)
if (!file.exists(county_file)) stop("Missing county panel: ", county_file)

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
  if (!length(x)) return(rep(NA_real_, length(probs)))
  o <- order(x)
  x <- x[o]
  w <- w[o]
  cw <- cumsum(w) / sum(w)
  sapply(probs, function(p) x[which(cw >= p)[1]])
}

make_year_bins <- function(dt, value_col, weight_col, year_col, n_bins) {
  probs <- seq(1 / n_bins, (n_bins - 1) / n_bins, by = 1 / n_bins)
  out_bin <- rep(NA_integer_, nrow(dt))
  defs <- vector("list", length(unique(dt[[year_col]])))
  i <- 0L

  for (yr in sort(unique(dt[[year_col]]))) {
    idx <- which(
      dt[[year_col]] == yr &
        is.finite(dt[[value_col]]) &
        !is.na(dt[[value_col]]) &
        is.finite(dt[[weight_col]]) &
        dt[[weight_col]] > 0
    )
    if (!length(idx)) next

    x <- dt[[value_col]][idx]
    w <- dt[[weight_col]][idx]
    qs <- weighted_quantile(x, w, probs)
    if (any(is.na(qs))) next
    for (k in 2:length(qs)) {
      if (qs[k] <= qs[k - 1]) qs[k] <- qs[k - 1] + 1e-8
    }
    br <- c(-Inf, qs, Inf)
    out_bin[idx] <- as.integer(cut(x, breaks = br, labels = FALSE, include.lowest = TRUE))

    i <- i + 1L
    defs[[i]] <- data.table(
      year = yr,
      bin = seq_len(n_bins),
      lower = br[seq_len(n_bins)],
      upper = br[seq_len(n_bins) + 1L]
    )
  }

  defs <- rbindlist(defs[seq_len(i)], use.names = TRUE, fill = TRUE)
  list(bin = out_bin, defs = defs)
}

safe_ratio <- function(num, den) {
  ifelse(is.finite(num) & is.finite(den) & !is.na(num) & !is.na(den) & den > 0, num / den, NA_real_)
}

save_heatmap <- function(df, metric_col, title, filename) {
  p <- ggplot(df, aes(x = income_bin, y = factor(year), fill = .data[[metric_col]])) +
    geom_tile() +
    labs(title = title, x = "Income decile", y = "Year", fill = metric_col) +
    theme_minimal(base_size = 11)
  save_plot(filename, { print(p) }, width = 1700, height = 1200)
}

# =========================================================
# 2) CPI setup (2020 dollars)
# =========================================================
# R-CPI-U-RS known values currently available in-repo (exact).
rcpi_known <- data.table(
  year = 2010:2024,
  cpi_year = c(
    236.876, 241.949, 245.245, 248.004, 252.156,
    252.553, 255.401, 260.609, 266.818, 271.917,
    275.665, 289.268, 313.761, 329.725, 344.667
  )
)

# Backcast proxy for pre-2010 years in this sample using CPI-U annual level
# scaled to the R-CPI-U-RS 2010 level.
cpi_u_proxy <- data.table(
  year = c(1980L, 1990L, 2005:2009),
  cpi_u = c(82.4, 130.7, 195.3, 201.6, 207.342, 215.303, 214.537)
)
scale_k <- rcpi_known[year == 2010, cpi_year][1] / 218.056
cpi_proxy <- cpi_u_proxy[, .(year, cpi_year = cpi_u * scale_k)]

cpi_lookup <- rbindlist(list(cpi_proxy, rcpi_known), use.names = TRUE, fill = TRUE)[
  year %in% years_keep
][order(year)]

if (!real_base_year %in% cpi_lookup$year) {
  stop("real_base_year not in CPI lookup: ", real_base_year)
}
base_cpi <- cpi_lookup[year == real_base_year, cpi_year][1]

# =========================================================
# 3) County LFPR ratio outputs
# =========================================================
county_dt <- fread(county_file)
county_dt <- county_dt[
  YEAR %in% years_keep
][
  ,
  `:=`(
    county_pop_wt = female_pop_wt + male_pop_wt,
    lfpr_ratio_male_to_female = safe_ratio(male_lfpr, female_lfpr)
  )
]

county_national_ratio <- county_dt[
  ,
  .(
    national_lfpr_ratio_pw_mean = weighted_mean_safe(lfpr_ratio_male_to_female, county_pop_wt),
    n_counties = .N
  ),
  by = .(YEAR)
][order(YEAR)]

county_lfpr_ratio_out <- rbindlist(
  list(
    county_dt[
      ,
      .(
        scope = "county",
        YEAR,
        STATEICP,
        COUNTYICP,
        female_lfpr,
        male_lfpr,
        lfpr_ratio_male_to_female,
        county_pop_wt
      )
    ],
    county_national_ratio[
      ,
      .(
        scope = "national",
        YEAR,
        STATEICP = NA_integer_,
        COUNTYICP = NA_integer_,
        female_lfpr = NA_real_,
        male_lfpr = NA_real_,
        lfpr_ratio_male_to_female = national_lfpr_ratio_pw_mean,
        county_pop_wt = NA_real_
      )
    ]
  ),
  use.names = TRUE,
  fill = TRUE
)

county_lfpr_ratio_file <- file.path(results_dir, "ipums_county_lfpr_ratio_timeseries.csv")
fwrite(county_lfpr_ratio_out, county_lfpr_ratio_file)

p_ratio_trend <- ggplot(county_national_ratio, aes(x = YEAR, y = national_lfpr_ratio_pw_mean)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  labs(
    title = "Population-weighted county male/female LFPR ratio over time",
    x = "Year",
    y = "Male LFPR / Female LFPR"
  ) +
  theme_minimal(base_size = 11)
save_plot("ipums_county_lfpr_ratio_national_trend.png", { print(p_ratio_trend) }, width = 1800, height = 1100)

p_ratio_dist <- ggplot(
  county_dt[YEAR %in% anchor_years & is.finite(lfpr_ratio_male_to_female)],
  aes(x = factor(YEAR), y = lfpr_ratio_male_to_female)
) +
  geom_boxplot(outlier.alpha = 0.08) +
  labs(
    title = "County male/female LFPR ratio distribution in anchor years",
    x = "Year",
    y = "Male LFPR / Female LFPR"
  ) +
  theme_minimal(base_size = 11)
save_plot("ipums_county_lfpr_ratio_anchor_year_distribution.png", { print(p_ratio_dist) }, width = 1900, height = 1100)

# =========================================================
# 4) Load spouse-pair panel and derive metric universe
# =========================================================
pair_cols <- c(
  "YEAR", "STATEICP", "COUNTYICP", "HHWT",
  "hhincome_nominal", "household_income_no_transfers",
  "female_income_wage_nonneg", "male_income_wage_nonneg",
  "female_income_total_nonneg", "male_income_total_nonneg",
  "female_income_no_transfers", "male_income_no_transfers",
  "female_annual_hours", "male_annual_hours",
  "female_weekly_hours", "male_weekly_hours",
  "female_empstat", "male_empstat"
)

message("Loading spouse-pair panel columns...")
dt <- fread(pair_file, select = pair_cols)
dt <- dt[YEAR %in% years_keep]

setkey(cpi_lookup, year)
dt <- cpi_lookup[dt, on = .(year = YEAR)]
setnames(dt, c("year", "cpi_year"), c("YEAR", "cpi_year_proxy"))
dt[, deflator_to_2020 := base_cpi / cpi_year_proxy]

# Household income variants
dt[, hh_wage_income_nominal := female_income_wage_nonneg + male_income_wage_nonneg]

# Requested hourly household measure: spouse-specific wage/hour then sum components.
dt[, female_hourly_component_nominal := fifelse(
  female_annual_hours > 0, female_income_wage_nonneg / female_annual_hours,
  fifelse(female_income_wage_nonneg == 0, 0, NA_real_)
)]
dt[, male_hourly_component_nominal := fifelse(
  male_annual_hours > 0, male_income_wage_nonneg / male_annual_hours,
  fifelse(male_income_wage_nonneg == 0, 0, NA_real_)
)]
dt[, female_hourly_component_issue := as.integer(female_income_wage_nonneg > 0 & !(female_annual_hours > 0))]
dt[, male_hourly_component_issue := as.integer(male_income_wage_nonneg > 0 & !(male_annual_hours > 0))]
dt[, hh_hourly_wage_nominal := fifelse(
  is.na(female_hourly_component_nominal) | is.na(male_hourly_component_nominal),
  NA_real_,
  female_hourly_component_nominal + male_hourly_component_nominal
)]

# Real-dollar variants (2020 dollars)
dt[, `:=`(
  hh_income_real = hhincome_nominal * deflator_to_2020,
  hh_income_no_transfers_real = household_income_no_transfers * deflator_to_2020,
  hh_wage_income_real = hh_wage_income_nominal * deflator_to_2020,
  hh_hourly_wage_real = hh_hourly_wage_nominal * deflator_to_2020,
  female_income_total_real = female_income_total_nonneg * deflator_to_2020,
  male_income_total_real = male_income_total_nonneg * deflator_to_2020,
  female_income_wage_real = female_income_wage_nonneg * deflator_to_2020,
  male_income_wage_real = male_income_wage_nonneg * deflator_to_2020,
  female_income_no_transfers_real = female_income_no_transfers * deflator_to_2020,
  male_income_no_transfers_real = male_income_no_transfers * deflator_to_2020
)]

dt[, `:=`(
  female_share_hh_income_total = safe_ratio(female_income_total_nonneg, hhincome_nominal),
  male_share_hh_income_total = safe_ratio(male_income_total_nonneg, hhincome_nominal),
  female_share_hh_income_no_transfers = safe_ratio(female_income_no_transfers, household_income_no_transfers),
  male_share_hh_income_no_transfers = safe_ratio(male_income_no_transfers, household_income_no_transfers)
)]

# Working definitions (all requested)
dt[, `:=`(
  f_work_emp = as.integer(female_empstat == 1),
  m_work_emp = as.integer(male_empstat == 1),
  f_work_lf = as.integer(female_empstat %in% c(1, 2)),
  m_work_lf = as.integer(male_empstat %in% c(1, 2)),
  f_work_hours = as.integer(female_annual_hours > 0),
  m_work_hours = as.integer(male_annual_hours > 0)
)]

for (pref in c("emp", "lf", "hours")) {
  fcol <- paste0("f_work_", pref)
  mcol <- paste0("m_work_", pref)
  dt[, (paste0("zero_", pref)) := as.integer(get(fcol) == 0 & get(mcol) == 0)]
  dt[, (paste0("male_only_", pref)) := as.integer(get(fcol) == 0 & get(mcol) == 1)]
  dt[, (paste0("female_only_", pref)) := as.integer(get(fcol) == 1 & get(mcol) == 0)]
  dt[, (paste0("both_", pref)) := as.integer(get(fcol) == 1 & get(mcol) == 1)]
}

# =========================================================
# 5) Income measure grid + national-year bins
# =========================================================
measure_map <- data.table(
  conditioning_measure = c("hh_income", "hh_income_no_transfers", "hh_wage_income", "hh_hourly_wage"),
  nominal_col = c("hhincome_nominal", "household_income_no_transfers", "hh_wage_income_nominal", "hh_hourly_wage_nominal"),
  real_col = c("hh_income_real", "hh_income_no_transfers_real", "hh_wage_income_real", "hh_hourly_wage_real"),
  formula = c(
    "HHINCOME (positive)",
    "max(HHINCOME - household_transfers, 0)",
    "female_wage_income + male_wage_income",
    "(female_incwage/female_annual_hours) + (male_incwage/male_annual_hours)"
  )
)

bin_defs_all <- list()

for (i in seq_len(nrow(measure_map))) {
  m <- measure_map$conditioning_measure[i]
  c_nom <- measure_map$nominal_col[i]
  for (nb in income_bin_counts) {
    b <- make_year_bins(dt, c_nom, "HHWT", "YEAR", nb)
    bcol <- paste0("bin_", nb, "_", m)
    dt[, (bcol) := b$bin]

    defs <- b$defs
    defs[, `:=`(
      conditioning_measure = m,
      bin_count = nb
    )]
    defs <- merge(defs, unique(dt[, .(YEAR, deflator_to_2020)]), by.x = "year", by.y = "YEAR", all.x = TRUE)
    defs[, `:=`(
      lower_real = lower * deflator_to_2020,
      upper_real = upper * deflator_to_2020
    )]
    bin_defs_all[[paste(m, nb, sep = "_")]] <- defs
  }
}

income_measure_grid <- rbindlist(bin_defs_all, use.names = TRUE, fill = TRUE)[
  order(conditioning_measure, bin_count, year, bin)
][
  ,
  .(
    conditioning_measure,
    bin_count,
    year,
    bin,
    lower_nominal = lower,
    upper_nominal = upper,
    lower_real,
    upper_real
  )
]

income_measure_grid <- merge(income_measure_grid, measure_map[, .(conditioning_measure, formula)], by = "conditioning_measure", all.x = TRUE)

income_measure_grid_file <- file.path(results_dir, "ipums_income_measure_grid.csv")
fwrite(income_measure_grid, income_measure_grid_file)

# =========================================================
# 6) Binned curves (ventiles)
# =========================================================
binned_curves <- list()

for (i in seq_len(nrow(measure_map))) {
  m <- measure_map$conditioning_measure[i]
  bcol <- paste0("bin_20_", m)
  for (basis in c("nominal", "real")) {
    xcol <- if (basis == "nominal") measure_map$nominal_col[i] else measure_map$real_col[i]
    f_total <- if (basis == "nominal") "female_income_total_nonneg" else "female_income_total_real"
    m_total <- if (basis == "nominal") "male_income_total_nonneg" else "male_income_total_real"
    f_wage <- if (basis == "nominal") "female_income_wage_nonneg" else "female_income_wage_real"
    m_wage <- if (basis == "nominal") "male_income_wage_nonneg" else "male_income_wage_real"
    f_not <- if (basis == "nominal") "female_income_no_transfers" else "female_income_no_transfers_real"
    m_not <- if (basis == "nominal") "male_income_no_transfers" else "male_income_no_transfers_real"

    tmp <- dt[!is.na(get(bcol)),
      .(
        households_wt = sum(HHWT, na.rm = TRUE),
        income_midpoint = weighted_mean_safe(get(xcol), HHWT),
        female_total_income_mean = weighted_mean_safe(get(f_total), HHWT),
        male_total_income_mean = weighted_mean_safe(get(m_total), HHWT),
        female_wage_income_mean = weighted_mean_safe(get(f_wage), HHWT),
        male_wage_income_mean = weighted_mean_safe(get(m_wage), HHWT),
        female_no_transfer_income_mean = weighted_mean_safe(get(f_not), HHWT),
        male_no_transfer_income_mean = weighted_mean_safe(get(m_not), HHWT),
        female_weekly_hours_mean = weighted_mean_safe(female_weekly_hours, HHWT),
        male_weekly_hours_mean = weighted_mean_safe(male_weekly_hours, HHWT),
        female_annual_hours_mean = weighted_mean_safe(female_annual_hours, HHWT),
        male_annual_hours_mean = weighted_mean_safe(male_annual_hours, HHWT),
        female_work_share_empstat = weighted_mean_safe(f_work_emp, HHWT),
        male_work_share_empstat = weighted_mean_safe(m_work_emp, HHWT),
        female_work_share_lf = weighted_mean_safe(f_work_lf, HHWT),
        male_work_share_lf = weighted_mean_safe(m_work_lf, HHWT),
        female_work_share_hours = weighted_mean_safe(f_work_hours, HHWT),
        male_work_share_hours = weighted_mean_safe(m_work_hours, HHWT)
      ),
      by = .(year = YEAR, income_bin = get(bcol))
    ]

    tmp[, `:=`(
      conditioning_measure = m,
      dollar_basis = basis,
      bin_count = 20L,
      weekly_hours_gap_female_minus_male = female_weekly_hours_mean - male_weekly_hours_mean,
      annual_hours_gap_female_minus_male = female_annual_hours_mean - male_annual_hours_mean,
      work_share_ratio_female_to_male_empstat = safe_ratio(female_work_share_empstat, male_work_share_empstat)
    )]
    binned_curves[[paste(m, basis, sep = "_")]] <- tmp
  }
}

binned_curves_dt <- rbindlist(binned_curves, use.names = TRUE, fill = TRUE)[
  order(conditioning_measure, dollar_basis, year, income_bin)
]

binned_curves_file <- file.path(results_dir, "ipums_binned_income_curves.csv")
fwrite(binned_curves_dt, binned_curves_file)

# =========================================================
# 7) Heatmap metrics (deciles)
# =========================================================
heatmap_out <- list()

for (i in seq_len(nrow(measure_map))) {
  m <- measure_map$conditioning_measure[i]
  bcol <- paste0("bin_10_", m)
  for (basis in c("nominal", "real")) {
    tmp <- dt[!is.na(get(bcol)),
      .(
        households_wt = sum(HHWT, na.rm = TRUE),
        female_share_hh_income_no_transfers_mean = weighted_mean_safe(female_share_hh_income_no_transfers, HHWT),
        male_share_hh_income_no_transfers_mean = weighted_mean_safe(male_share_hh_income_no_transfers, HHWT),
        weekly_hours_gap_female_minus_male = weighted_mean_safe(female_weekly_hours - male_weekly_hours, HHWT),
        male_only_share_empstat = weighted_mean_safe(male_only_emp, HHWT),
        female_only_share_empstat = weighted_mean_safe(female_only_emp, HHWT),
        both_share_empstat = weighted_mean_safe(both_emp, HHWT),
        zero_share_empstat = weighted_mean_safe(zero_emp, HHWT),
        male_only_share_lf = weighted_mean_safe(male_only_lf, HHWT),
        female_only_share_lf = weighted_mean_safe(female_only_lf, HHWT),
        both_share_lf = weighted_mean_safe(both_lf, HHWT),
        zero_share_lf = weighted_mean_safe(zero_lf, HHWT),
        male_only_share_hours = weighted_mean_safe(male_only_hours, HHWT),
        female_only_share_hours = weighted_mean_safe(female_only_hours, HHWT),
        both_share_hours = weighted_mean_safe(both_hours, HHWT),
        zero_share_hours = weighted_mean_safe(zero_hours, HHWT),
        female_work_share_empstat = weighted_mean_safe(f_work_emp, HHWT),
        male_work_share_empstat = weighted_mean_safe(m_work_emp, HHWT)
      ),
      by = .(year = YEAR, income_bin = get(bcol))
    ]
    tmp[, `:=`(
      conditioning_measure = m,
      dollar_basis = basis,
      bin_count = 10L,
      female_male_work_ratio_empstat = safe_ratio(female_work_share_empstat, male_work_share_empstat)
    )]
    heatmap_out[[paste(m, basis, sep = "_")]] <- tmp
  }
}

heatmap_dt <- rbindlist(heatmap_out, use.names = TRUE, fill = TRUE)[
  order(conditioning_measure, dollar_basis, year, income_bin)
]

heatmap_file <- file.path(results_dir, "ipums_heatmap_metrics.csv")
fwrite(heatmap_dt, heatmap_file)

# =========================================================
# 8) Married-household composition over time
# =========================================================
comp_out <- list()
def_map <- c(emp = "EMPSTAT==1", lf = "EMPSTAT in (1,2)", hours = "annual_hours>0")

for (i in seq_len(nrow(measure_map))) {
  m <- measure_map$conditioning_measure[i]
  bcol <- paste0("bin_5_", m)

  for (pref in names(def_map)) {
    zero_col <- paste0("zero_", pref)
    male_col <- paste0("male_only_", pref)
    female_col <- paste0("female_only_", pref)
    both_col <- paste0("both_", pref)
    f_work_col <- paste0("f_work_", pref)
    m_work_col <- paste0("m_work_", pref)

    overall <- dt[
      ,
      .(
        households_wt = sum(HHWT, na.rm = TRUE),
        zero_share = weighted_mean_safe(get(zero_col), HHWT),
        male_only_share = weighted_mean_safe(get(male_col), HHWT),
        female_only_share = weighted_mean_safe(get(female_col), HHWT),
        both_share = weighted_mean_safe(get(both_col), HHWT),
        female_work_share = weighted_mean_safe(get(f_work_col), HHWT),
        male_work_share = weighted_mean_safe(get(m_work_col), HHWT)
      ),
      by = .(year = YEAR)
    ][
      ,
      `:=`(
        conditioning_measure = m,
        work_definition = pref,
        income_quintile = NA_integer_,
        level = "overall",
        female_male_work_ratio = safe_ratio(female_work_share, male_work_share)
      )
    ]

    by_quintile <- dt[
      !is.na(get(bcol)),
      .(
        households_wt = sum(HHWT, na.rm = TRUE),
        zero_share = weighted_mean_safe(get(zero_col), HHWT),
        male_only_share = weighted_mean_safe(get(male_col), HHWT),
        female_only_share = weighted_mean_safe(get(female_col), HHWT),
        both_share = weighted_mean_safe(get(both_col), HHWT),
        female_work_share = weighted_mean_safe(get(f_work_col), HHWT),
        male_work_share = weighted_mean_safe(get(m_work_col), HHWT)
      ),
      by = .(year = YEAR, income_quintile = get(bcol))
    ][
      ,
      `:=`(
        conditioning_measure = m,
        work_definition = pref,
        level = "quintile",
        female_male_work_ratio = safe_ratio(female_work_share, male_work_share)
      )
    ]

    comp_out[[paste(m, pref, "overall", sep = "_")]] <- overall
    comp_out[[paste(m, pref, "q", sep = "_")]] <- by_quintile
  }
}

composition_dt <- rbindlist(comp_out, use.names = TRUE, fill = TRUE)[
  order(conditioning_measure, work_definition, level, year, income_quintile)
]

composition_file <- file.path(results_dir, "ipums_married_household_composition_timeseries.csv")
fwrite(composition_dt, composition_file)

# =========================================================
# 9) County married-pair metrics + county cross-sections
# =========================================================
county_pair_out <- list()

for (i in seq_len(nrow(measure_map))) {
  m <- measure_map$conditioning_measure[i]
  bcol <- paste0("bin_5_", m)
  for (basis in c("nominal", "real")) {
    f_not <- if (basis == "nominal") "female_income_no_transfers" else "female_income_no_transfers_real"
    m_not <- if (basis == "nominal") "male_income_no_transfers" else "male_income_no_transfers_real"

    qtab <- dt[
      !is.na(get(bcol)),
      .(
        households_wt = sum(HHWT, na.rm = TRUE),
        female_weekly_hours_mean = weighted_mean_safe(female_weekly_hours, HHWT),
        male_weekly_hours_mean = weighted_mean_safe(male_weekly_hours, HHWT),
        female_annual_hours_mean = weighted_mean_safe(female_annual_hours, HHWT),
        male_annual_hours_mean = weighted_mean_safe(male_annual_hours, HHWT),
        female_income_no_transfers_mean = weighted_mean_safe(get(f_not), HHWT),
        male_income_no_transfers_mean = weighted_mean_safe(get(m_not), HHWT),
        female_work_share_empstat = weighted_mean_safe(f_work_emp, HHWT),
        male_work_share_empstat = weighted_mean_safe(m_work_emp, HHWT),
        female_work_share_lf = weighted_mean_safe(f_work_lf, HHWT),
        male_work_share_lf = weighted_mean_safe(m_work_lf, HHWT),
        female_work_share_hours = weighted_mean_safe(f_work_hours, HHWT),
        male_work_share_hours = weighted_mean_safe(m_work_hours, HHWT)
      ),
      by = .(
        YEAR,
        STATEICP,
        COUNTYICP,
        income_quintile = get(bcol)
      )
    ][
      ,
      `:=`(
        conditioning_measure = m,
        dollar_basis = basis,
        level = "quintile",
        weekly_hours_gap_female_minus_male = female_weekly_hours_mean - male_weekly_hours_mean,
        annual_hours_gap_female_minus_male = female_annual_hours_mean - male_annual_hours_mean,
        income_gap_no_transfers_female_minus_male = female_income_no_transfers_mean - male_income_no_transfers_mean,
        work_share_ratio_female_to_male_empstat = safe_ratio(female_work_share_empstat, male_work_share_empstat)
      )
    ]

    overall <- dt[
      ,
      .(
        households_wt = sum(HHWT, na.rm = TRUE),
        female_weekly_hours_mean = weighted_mean_safe(female_weekly_hours, HHWT),
        male_weekly_hours_mean = weighted_mean_safe(male_weekly_hours, HHWT),
        female_annual_hours_mean = weighted_mean_safe(female_annual_hours, HHWT),
        male_annual_hours_mean = weighted_mean_safe(male_annual_hours, HHWT),
        female_income_no_transfers_mean = weighted_mean_safe(get(f_not), HHWT),
        male_income_no_transfers_mean = weighted_mean_safe(get(m_not), HHWT),
        female_work_share_empstat = weighted_mean_safe(f_work_emp, HHWT),
        male_work_share_empstat = weighted_mean_safe(m_work_emp, HHWT),
        female_work_share_lf = weighted_mean_safe(f_work_lf, HHWT),
        male_work_share_lf = weighted_mean_safe(m_work_lf, HHWT),
        female_work_share_hours = weighted_mean_safe(f_work_hours, HHWT),
        male_work_share_hours = weighted_mean_safe(m_work_hours, HHWT)
      ),
      by = .(YEAR, STATEICP, COUNTYICP)
    ][
      ,
      `:=`(
        conditioning_measure = m,
        dollar_basis = basis,
        level = "overall",
        income_quintile = NA_integer_,
        weekly_hours_gap_female_minus_male = female_weekly_hours_mean - male_weekly_hours_mean,
        annual_hours_gap_female_minus_male = female_annual_hours_mean - male_annual_hours_mean,
        income_gap_no_transfers_female_minus_male = female_income_no_transfers_mean - male_income_no_transfers_mean,
        work_share_ratio_female_to_male_empstat = safe_ratio(female_work_share_empstat, male_work_share_empstat)
      )
    ]

    county_pair_out[[paste(m, basis, "q", sep = "_")]] <- qtab
    county_pair_out[[paste(m, basis, "overall", sep = "_")]] <- overall
  }
}

county_pair_dt <- rbindlist(county_pair_out, use.names = TRUE, fill = TRUE)
setDT(county_dt)
county_pair_dt <- merge(
  county_pair_dt,
  county_dt[, .(YEAR, STATEICP, COUNTYICP, lfpr_ratio_male_to_female, female_lfpr, male_lfpr)],
  by = c("YEAR", "STATEICP", "COUNTYICP"),
  all.x = TRUE
)

county_pair_file <- file.path(results_dir, "ipums_county_married_pair_metrics.csv")
fwrite(county_pair_dt, county_pair_file)

# Cross-section plots: primary measure = hh_income_no_transfers, nominal, overall.
cross_dt <- county_pair_dt[
  conditioning_measure == "hh_income_no_transfers" &
    dollar_basis == "nominal" &
    level == "overall" &
    YEAR %in% anchor_years
]

p_cross_income <- ggplot(
  cross_dt[is.finite(lfpr_ratio_male_to_female) & is.finite(income_gap_no_transfers_female_minus_male)],
  aes(x = lfpr_ratio_male_to_female, y = income_gap_no_transfers_female_minus_male, size = households_wt)
) +
  geom_point(alpha = 0.25, color = "#1f77b4") +
  facet_wrap(~YEAR, scales = "free") +
  labs(
    title = "County cross-section: LFPR ratio vs spouse income gap (no transfers)",
    x = "County male/female LFPR ratio",
    y = "Female - male spouse income gap (nominal dollars)",
    size = "HH weight"
  ) +
  theme_minimal(base_size = 10)
save_plot("ipums_county_cross_section_income_gap_no_transfers.png", { print(p_cross_income) }, width = 2400, height = 1400)

p_cross_hours <- ggplot(
  cross_dt[is.finite(lfpr_ratio_male_to_female) & is.finite(weekly_hours_gap_female_minus_male)],
  aes(x = lfpr_ratio_male_to_female, y = weekly_hours_gap_female_minus_male, size = households_wt)
) +
  geom_point(alpha = 0.25, color = "#2ca02c") +
  facet_wrap(~YEAR, scales = "free") +
  labs(
    title = "County cross-section: LFPR ratio vs spouse weekly-hours gap",
    x = "County male/female LFPR ratio",
    y = "Female - male weekly-hours gap",
    size = "HH weight"
  ) +
  theme_minimal(base_size = 10)
save_plot("ipums_county_cross_section_hours_gap_weekly.png", { print(p_cross_hours) }, width = 2400, height = 1400)

p_cross_work <- ggplot(
  cross_dt[is.finite(lfpr_ratio_male_to_female) & is.finite(work_share_ratio_female_to_male_empstat)],
  aes(x = lfpr_ratio_male_to_female, y = work_share_ratio_female_to_male_empstat, size = households_wt)
) +
  geom_point(alpha = 0.25, color = "#d62728") +
  facet_wrap(~YEAR, scales = "free") +
  labs(
    title = "County cross-section: LFPR ratio vs within-pair female/male work-share ratio",
    x = "County male/female LFPR ratio",
    y = "Female/male work-share ratio (EMPSTAT==1)",
    size = "HH weight"
  ) +
  theme_minimal(base_size = 10)
save_plot("ipums_county_cross_section_work_share_ratio_empstat.png", { print(p_cross_work) }, width = 2400, height = 1400)

# =========================================================
# 10) Graph families: binned curves + heatmaps + composition
# =========================================================
for (i in seq_len(nrow(measure_map))) {
  m <- measure_map$conditioning_measure[i]
  for (basis in c("nominal", "real")) {
    sub <- binned_curves_dt[
      conditioning_measure == m &
        dollar_basis == basis &
        year %in% anchor_years
    ]
    if (!nrow(sub)) next

    inc_long <- melt(
      sub,
      id.vars = c("year", "income_bin", "income_midpoint"),
      measure.vars = c("female_no_transfer_income_mean", "male_no_transfer_income_mean"),
      variable.name = "sex",
      value.name = "income_value"
    )
    inc_long[, sex := fifelse(sex == "female_no_transfer_income_mean", "Female", "Male")]

    p_inc <- ggplot(inc_long, aes(x = income_midpoint, y = income_value, color = sex)) +
      geom_line(linewidth = 0.8) +
      facet_wrap(~year, scales = "free_x") +
      labs(
        title = paste0("Binned curve: spouse no-transfer income vs ", m, " (", basis, ")"),
        x = paste0(m, " (", basis, ")"),
        y = "Spouse no-transfer income",
        color = "Spouse"
      ) +
      theme_minimal(base_size = 10)
    save_plot(paste0("ipums_binned_curve_", m, "_", basis, "_spouse_income_no_transfers.png"), { print(p_inc) }, width = 2400, height = 1400)

    hrs_long <- melt(
      sub,
      id.vars = c("year", "income_bin", "income_midpoint"),
      measure.vars = c("female_weekly_hours_mean", "male_weekly_hours_mean"),
      variable.name = "sex",
      value.name = "hours"
    )
    hrs_long[, sex := fifelse(sex == "female_weekly_hours_mean", "Female", "Male")]

    p_hrs <- ggplot(hrs_long, aes(x = income_midpoint, y = hours, color = sex)) +
      geom_line(linewidth = 0.8) +
      facet_wrap(~year, scales = "free_x") +
      labs(
        title = paste0("Binned curve: spouse weekly hours vs ", m, " (", basis, ")"),
        x = paste0(m, " (", basis, ")"),
        y = "Weekly hours",
        color = "Spouse"
      ) +
      theme_minimal(base_size = 10)
    save_plot(paste0("ipums_binned_curve_", m, "_", basis, "_spouse_weekly_hours.png"), { print(p_hrs) }, width = 2400, height = 1400)

    work_long <- melt(
      sub,
      id.vars = c("year", "income_bin", "income_midpoint"),
      measure.vars = c("female_work_share_empstat", "male_work_share_empstat"),
      variable.name = "sex",
      value.name = "work_share"
    )
    work_long[, sex := fifelse(sex == "female_work_share_empstat", "Female", "Male")]

    p_work <- ggplot(work_long, aes(x = income_midpoint, y = work_share, color = sex)) +
      geom_line(linewidth = 0.8) +
      facet_wrap(~year, scales = "free_x") +
      labs(
        title = paste0("Binned curve: spouse working share vs ", m, " (", basis, ")"),
        x = paste0(m, " (", basis, ")"),
        y = "Working share (EMPSTAT==1)",
        color = "Spouse"
      ) +
      theme_minimal(base_size = 10)
    save_plot(paste0("ipums_binned_curve_", m, "_", basis, "_spouse_work_share_empstat.png"), { print(p_work) }, width = 2400, height = 1400)

    hsub <- heatmap_dt[conditioning_measure == m & dollar_basis == basis]
    if (nrow(hsub)) {
      save_heatmap(hsub, "female_share_hh_income_no_transfers_mean", paste0("Heatmap: female HH share (no transfers) | ", m, " (", basis, ")"), paste0("ipums_heatmap_", m, "_", basis, "_female_share_no_transfers.png"))
      save_heatmap(hsub, "weekly_hours_gap_female_minus_male", paste0("Heatmap: female-male weekly-hours gap | ", m, " (", basis, ")"), paste0("ipums_heatmap_", m, "_", basis, "_weekly_hours_gap.png"))
      save_heatmap(hsub, "both_share_empstat", paste0("Heatmap: dual-working share (EMPSTAT==1) | ", m, " (", basis, ")"), paste0("ipums_heatmap_", m, "_", basis, "_both_share_empstat.png"))
      save_heatmap(hsub, "female_male_work_ratio_empstat", paste0("Heatmap: female/male work-share ratio (EMPSTAT==1) | ", m, " (", basis, ")"), paste0("ipums_heatmap_", m, "_", basis, "_female_male_work_ratio_empstat.png"))
    }
  }
}

# Composition graphs (primary conditioning measure for visuals)
primary_measure <- "hh_income_no_transfers"
for (pref in c("emp", "lf", "hours")) {
  sub_over <- composition_dt[
    conditioning_measure == primary_measure &
      work_definition == pref &
      level == "overall"
  ]
  if (!nrow(sub_over)) next

  area_long <- melt(
    sub_over,
    id.vars = c("year"),
    measure.vars = c("zero_share", "male_only_share", "female_only_share", "both_share"),
    variable.name = "composition",
    value.name = "share"
  )
  area_long[, composition := factor(
    composition,
    levels = c("zero_share", "male_only_share", "female_only_share", "both_share"),
    labels = c("0 working spouses", "Male-only", "Female-only", "Both working")
  )]

  p_area <- ggplot(area_long, aes(x = year, y = share, fill = composition)) +
    geom_area() +
    labs(
      title = paste0("Married-household composition over time (", pref, ", ", primary_measure, ")"),
      x = "Year",
      y = "Household share",
      fill = "Composition"
    ) +
    theme_minimal(base_size = 11)
  save_plot(paste0("ipums_composition_", pref, "_stacked_area_over_time.png"), { print(p_area) }, width = 2000, height = 1200)

  sub_q <- composition_dt[
    conditioning_measure == primary_measure &
      work_definition == pref &
      level == "quintile"
  ]
  p_q <- ggplot(sub_q, aes(x = year, y = both_share, color = factor(income_quintile))) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.3) +
    labs(
      title = paste0("Both-working share by income quintile over time (", pref, ")"),
      x = "Year",
      y = "Both-working household share",
      color = "Income quintile"
    ) +
    theme_minimal(base_size = 11)
  save_plot(paste0("ipums_composition_", pref, "_both_working_by_quintile.png"), { print(p_q) }, width = 2000, height = 1200)
}

# =========================================================
# 11) Validation summaries
# =========================================================
check_identity <- data.table(
  check_name = c(
    "female_no_transfer_le_total",
    "male_no_transfer_le_total",
    "household_no_transfer_le_nominal",
    "female_hourly_component_inconsistent",
    "male_hourly_component_inconsistent",
    "female_share_total_in_range",
    "male_share_total_in_range"
  ),
  violations = c(
    dt[female_income_no_transfers > female_income_total_nonneg, .N],
    dt[male_income_no_transfers > male_income_total_nonneg, .N],
    dt[household_income_no_transfers > hhincome_nominal, .N],
    dt[female_hourly_component_issue == 1, .N],
    dt[male_hourly_component_issue == 1, .N],
    dt[!is.na(female_share_hh_income_total) & (female_share_hh_income_total < 0 | female_share_hh_income_total > 5), .N],
    dt[!is.na(male_share_hh_income_total) & (male_share_hh_income_total < 0 | male_share_hh_income_total > 5), .N]
  )
)

check_composition <- composition_dt[
  ,
  .(max_abs_err = max(abs((zero_share + male_only_share + female_only_share + both_share) - 1), na.rm = TRUE)),
  by = .(conditioning_measure, work_definition, level)
]

check_coverage <- county_pair_dt[
  conditioning_measure == "hh_income_no_transfers" & dollar_basis == "nominal" & level == "overall",
  .(
    n_county = .N,
    n_joined_lfpr = sum(!is.na(lfpr_ratio_male_to_female)),
    coverage = mean(!is.na(lfpr_ratio_male_to_female))
  ),
  by = .(YEAR)
][order(YEAR)]

validation_file <- file.path(results_dir, "ipums_suite_validation_summary.csv")
validation_dt <- rbindlist(
  list(
    check_identity[, .(section = "identity", item = check_name, value = as.character(violations))],
    check_composition[, .(section = "composition", item = paste(conditioning_measure, work_definition, level, sep = "|"), value = as.character(max_abs_err))],
    check_coverage[, .(section = "county_coverage", item = as.character(YEAR), value = as.character(coverage))]
  ),
  use.names = TRUE,
  fill = TRUE
)
fwrite(validation_dt, validation_file)

message("Married-household suite complete.")
message("Wrote: ", income_measure_grid_file)
message("Wrote: ", binned_curves_file)
message("Wrote: ", heatmap_file)
message("Wrote: ", county_pair_file)
message("Wrote: ", county_lfpr_ratio_file)
message("Wrote: ", composition_file)
message("Wrote: ", validation_file)
