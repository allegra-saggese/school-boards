library(data.table)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

source("R/paths.R")
source("functions.R")

pair_file <- data_path("processed", "panel", "ipums_married_oppositesex_spouse_pairs_with_kids.csv")
results_dir <- data_path("processed", "results")
ensure_dir(results_dir)
ensure_dir(graphs_dir())

if (!file.exists(pair_file)) {
  stop("Missing pair file: ", pair_file)
}

message("Loading selected columns from spouse pair panel...")
dt <- fread(
  pair_file,
  select = c(
    "YEAR",
    "income_quintile",
    "hhincome_nominal",
    "household_income_no_transfers",
    "male_income_no_transfers",
    "female_income_no_transfers"
  )
)

dt <- dt[
  !is.na(hhincome_nominal) &
    !is.na(male_income_no_transfers) &
    !is.na(female_income_no_transfers) &
    hhincome_nominal > 0
]

message("Building stratified sample for scatter plots...")
sample_dt <- dt[, .SD[sample(.N, min(.N, 6000L))], by = YEAR]
rm(dt)
gc()

sample_out <- file.path(results_dir, "ipums_household_income_vs_spouse_income_no_transfers_scatter_sample.csv")
fwrite(sample_dt, sample_out)

x_cap <- suppressWarnings(as.numeric(quantile(sample_dt$hhincome_nominal, probs = 0.99, na.rm = TRUE)))
y_cap_m <- suppressWarnings(as.numeric(quantile(sample_dt$male_income_no_transfers, probs = 0.99, na.rm = TRUE)))
y_cap_f <- suppressWarnings(as.numeric(quantile(sample_dt$female_income_no_transfers, probs = 0.99, na.rm = TRUE)))
y_cap_all <- max(y_cap_m, y_cap_f, na.rm = TRUE)

p_male <- ggplot(sample_dt, aes(x = hhincome_nominal, y = male_income_no_transfers)) +
  geom_point(alpha = 0.08, size = 0.6, color = "#1f77b4") +
  coord_cartesian(xlim = c(0, x_cap), ylim = c(0, y_cap_m)) +
  labs(
    title = "Male spouse personal income (no transfers) vs household income",
    subtitle = "Random stratified sample by year; axes clipped at 99th percentile",
    x = "Household income (nominal dollars)",
    y = "Male spouse personal income, no transfers (nominal dollars)"
  ) +
  theme_minimal(base_size = 11)
save_plot("ipums_scatter_household_income_vs_male_income_no_transfers.png", { print(p_male) }, width = 1800, height = 1200)

p_female <- ggplot(sample_dt, aes(x = hhincome_nominal, y = female_income_no_transfers)) +
  geom_point(alpha = 0.08, size = 0.6, color = "#d62728") +
  coord_cartesian(xlim = c(0, x_cap), ylim = c(0, y_cap_f)) +
  labs(
    title = "Female spouse personal income (no transfers) vs household income",
    subtitle = "Random stratified sample by year; axes clipped at 99th percentile",
    x = "Household income (nominal dollars)",
    y = "Female spouse personal income, no transfers (nominal dollars)"
  ) +
  theme_minimal(base_size = 11)
save_plot("ipums_scatter_household_income_vs_female_income_no_transfers.png", { print(p_female) }, width = 1800, height = 1200)

long_dt <- sample_dt %>%
  pivot_longer(
    cols = c(male_income_no_transfers, female_income_no_transfers),
    names_to = "sex",
    values_to = "spouse_income_no_transfers"
  ) %>%
  mutate(
    sex = recode(
      sex,
      male_income_no_transfers = "Male spouse",
      female_income_no_transfers = "Female spouse"
    )
  )

p_facet <- ggplot(long_dt, aes(x = hhincome_nominal, y = spouse_income_no_transfers, color = sex)) +
  geom_point(alpha = 0.06, size = 0.55) +
  coord_cartesian(xlim = c(0, x_cap), ylim = c(0, y_cap_all)) +
  facet_wrap(~sex, ncol = 2) +
  labs(
    title = "Spouse personal income (no transfers) vs household income",
    subtitle = "Random stratified sample by year; axes clipped at 99th percentile",
    x = "Household income (nominal dollars)",
    y = "Spouse personal income, no transfers (nominal dollars)",
    color = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")
save_plot("ipums_scatter_household_income_vs_spouse_income_no_transfers_facet.png", { print(p_facet) }, width = 2200, height = 1200)

message("Scatter sample file: ", sample_out)
