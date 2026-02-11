library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(knitr)

source("R/paths.R")
source("functions.R")

# =========================================================
# 0) Load latest panel
# =========================================================
panel_dir <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
ensure_dir(results_dir)

panel_files <- list.files(panel_dir, pattern = "_lfpr_panel\\.csv$", full.names = TRUE)
if (length(panel_files) == 0) {
  stop("No lfpr_panel file found in data/processed/panel.")
}
latest_panel <- sort(panel_files)[length(panel_files)]
lfpr_panel <- read_csv(latest_panel, show_col_types = FALSE)

# =========================================================
# 1) Build county-year group indicators
# =========================================================
# vote_margin > 0 means more Democratic votes; < 0 means more Republican votes.
lfpr_panel <- lfpr_panel %>%
  mutate(
    trad = as.integer(coalesce(income_quintile_state == 5 & vote_margin < -0.02, FALSE)),
    asp_trad = as.integer(coalesce(income_quintile_state == 1 & vote_margin < -0.02, FALSE)),
    dem_solid_poor = as.integer(coalesce(income_quintile_state == 1 & vote_margin > 0.02, FALSE)),
    dem_solid_rich = as.integer(coalesce(income_quintile_state == 5 & vote_margin > 0.02, FALSE))
  )

grouped_panel_out <- dated_path(panel_dir, "lfpr_panel_with_groups.csv")
write_csv(lfpr_panel, grouped_panel_out)

# =========================================================
# 2) Exploratory stats for each binary grouping (overall)
# =========================================================
group_specs <- tibble::tribble(
  ~group_var, ~group_name,
  "trad", "trad",
  "asp_trad", "asp_trad",
  "dem_solid_poor", "dem_solid_poor",
  "dem_solid_rich", "dem_solid_rich"
)

group_stats <- purrr::map_dfr(seq_len(nrow(group_specs)), function(i) {
  gvar <- group_specs$group_var[i]
  gname <- group_specs$group_name[i]

  d <- lfpr_panel %>% filter(.data[[gvar]] == 1)
  tibble::tibble(
    group = gname,
    n_county_year = nrow(d),
    n_county = dplyr::n_distinct(d$fips),
    female_lfpr_mean = mean(d$lfpr_female, na.rm = TRUE),
    female_lfpr_median = median(d$lfpr_female, na.rm = TRUE),
    female_lfpr_sd = sd(d$lfpr_female, na.rm = TRUE),
    female_lfpr_min = min(d$lfpr_female, na.rm = TRUE),
    female_lfpr_max = max(d$lfpr_female, na.rm = TRUE),
    gap_mean = mean(d$lfpr_gap, na.rm = TRUE),
    gap_median = median(d$lfpr_gap, na.rm = TRUE),
    gap_sd = sd(d$lfpr_gap, na.rm = TRUE),
    total_lfpr_mean = mean(d$lfpr_total, na.rm = TRUE)
  )
})

stats_csv <- dated_path(results_dir, "lfpr_group_stats.csv")
write_csv(group_stats, stats_csv)

stats_tex <- dated_path(results_dir, "lfpr_group_stats.tex")
sink(stats_tex)
cat("% Auto-generated group summary statistics\n")
print(kable(group_stats, format = "latex", booktabs = TRUE, digits = 3))
sink()

# =========================================================
# 3) Exploratory stats by year and group (panel format)
# =========================================================
group_year_stats <- purrr::map_dfr(seq_len(nrow(group_specs)), function(i) {
  gvar <- group_specs$group_var[i]
  gname <- group_specs$group_name[i]

  lfpr_panel %>%
    filter(.data[[gvar]] == 1) %>%
    group_by(year) %>%
    summarise(
      group = gname,
      n_county_year = n(),
      n_county = dplyr::n_distinct(fips),
      female_lfpr_mean = mean(lfpr_female, na.rm = TRUE),
      female_lfpr_median = median(lfpr_female, na.rm = TRUE),
      female_lfpr_sd = sd(lfpr_female, na.rm = TRUE),
      female_lfpr_min = min(lfpr_female, na.rm = TRUE),
      female_lfpr_max = max(lfpr_female, na.rm = TRUE),
      male_lfpr_mean = mean(lfpr_male, na.rm = TRUE),
      male_lfpr_median = median(lfpr_male, na.rm = TRUE),
      male_lfpr_sd = sd(lfpr_male, na.rm = TRUE),
      male_lfpr_min = min(lfpr_male, na.rm = TRUE),
      male_lfpr_max = max(lfpr_male, na.rm = TRUE),
      gap_mean = mean(lfpr_gap, na.rm = TRUE),
      gap_median = median(lfpr_gap, na.rm = TRUE),
      gap_sd = sd(lfpr_gap, na.rm = TRUE),
      total_lfpr_mean = mean(lfpr_total, na.rm = TRUE),
      rep_percent_mean = mean(rep_percent, na.rm = TRUE),
      rep_percent_sum = sum(rep_percent, na.rm = TRUE),
      dem_percent_mean = mean(dem_percent, na.rm = TRUE),
      dem_percent_sum = sum(dem_percent, na.rm = TRUE),
      median_hh_income_median = median(median_hh_income, na.rm = TRUE),
      median_hh_income_mean = mean(median_hh_income, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    relocate(group, year)
})

# Ensure one row per group-year even when a group has zero county-years in a year.
all_years <- sort(unique(lfpr_panel$year))
group_year_stats <- tidyr::expand_grid(
  group = group_specs$group_name,
  year = all_years
) %>%
  left_join(group_year_stats, by = c("group", "year")) %>%
  mutate(
    n_county_year = ifelse(is.na(n_county_year), 0, n_county_year),
    n_county = ifelse(is.na(n_county), 0, n_county)
  ) %>%
  arrange(group, year)

group_year_csv <- dated_path(results_dir, "lfpr_group_year_stats.csv")
write_csv(group_year_stats, group_year_csv)

group_year_tex <- dated_path(results_dir, "lfpr_group_year_stats.tex")
sink(group_year_tex)
cat("% Auto-generated group-by-year summary statistics\n")
print(kable(group_year_stats, format = "latex", booktabs = TRUE, longtable = TRUE, digits = 3))
sink()

# =========================================================
# 4) National LFPR over time plots (two groups per plot)
# =========================================================
plot_group_trends <- function(df, group_a_var, group_a_name, group_b_var, group_b_name, filename) {
  d <- df %>%
    mutate(
      group = dplyr::case_when(
        .data[[group_a_var]] == 1 ~ group_a_name,
        .data[[group_b_var]] == 1 ~ group_b_name,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(group))

  yearly <- d %>%
    group_by(year, group) %>%
    summarise(
      lfpr_total_mean = mean(lfpr_total, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )

  p <- ggplot(yearly, aes(x = year, y = lfpr_total_mean, color = group)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2) +
    labs(
      title = paste("National LFPR over time:", group_a_name, "vs", group_b_name),
      x = "Year",
      y = "Mean LFPR (total, 20-64)",
      color = "Group"
    ) +
    theme_minimal(base_size = 12)

  save_plot(filename, { print(p) }, width = 1800, height = 1100)
}

plot_group_trends(
  lfpr_panel,
  group_a_var = "trad",
  group_a_name = "trad (top quintile, R+2pp)",
  group_b_var = "asp_trad",
  group_b_name = "asp_trad (bottom quintile, R+2pp)",
  filename = "national_lfpr_trad_vs_asp_trad.png"
)

plot_group_trends(
  lfpr_panel,
  group_a_var = "dem_solid_rich",
  group_a_name = "dem_solid_rich (top quintile, D+2pp)",
  group_b_var = "dem_solid_poor",
  group_b_name = "dem_solid_poor (bottom quintile, D+2pp)",
  filename = "national_lfpr_dem_solid_rich_vs_poor.png"
)

message("Grouping analysis complete.")
message("Input panel: ", latest_panel)
message("Panel with groups: ", grouped_panel_out)
message("Stats CSV: ", stats_csv)
message("Stats-by-year CSV: ", group_year_csv)

# =========================================================
# 5) Additional visuals requested
# =========================================================

# 5a) Box plot: female LFPR distributions for the four binary groups
plot_data_groups <- lfpr_panel %>%
  mutate(
    group_label = case_when(
      trad == 1 ~ "trad",
      asp_trad == 1 ~ "asp_trad",
      dem_solid_poor == 1 ~ "dem_solid_poor",
      dem_solid_rich == 1 ~ "dem_solid_rich",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(group_label), !is.na(lfpr_female))

p_box <- ggplot(plot_data_groups, aes(x = group_label, y = lfpr_female, fill = group_label)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.25) +
  labs(
    title = "Female LFPR distribution by county-year grouping",
    x = "Group",
    y = "Female LFPR (20-64)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

save_plot("female_lfpr_boxplot_four_groups.png", { print(p_box) }, width = 1600, height = 1100)

# 5b) Time trends for trad group: female LFPR and LFPR gap with uncertainty range
trad_year <- lfpr_panel %>%
  filter(trad == 1) %>%
  group_by(year) %>%
  summarise(
    female_mean = mean(lfpr_female, na.rm = TRUE),
    female_sd = sd(lfpr_female, na.rm = TRUE),
    female_min = min(lfpr_female, na.rm = TRUE),
    female_max = max(lfpr_female, na.rm = TRUE),
    gap_mean = mean(lfpr_gap, na.rm = TRUE),
    gap_sd = sd(lfpr_gap, na.rm = TRUE),
    gap_min = min(lfpr_gap, na.rm = TRUE),
    gap_max = max(lfpr_gap, na.rm = TRUE),
    .groups = "drop"
  )

p_trad_female <- ggplot(trad_year, aes(x = year, y = female_mean)) +
  geom_ribbon(aes(ymin = female_mean - female_sd, ymax = female_mean + female_sd), alpha = 0.20, fill = "steelblue") +
  geom_line(linewidth = 1, color = "steelblue4") +
  geom_point(size = 2, color = "steelblue4") +
  labs(
    title = "Trad counties: female LFPR over time (mean +/- 1 SD)",
    x = "Year",
    y = "Female LFPR (20-64)"
  ) +
  theme_minimal(base_size = 12)

save_plot("trad_female_lfpr_time_trend_with_sd.png", { print(p_trad_female) }, width = 1700, height = 1100)

p_trad_gap <- ggplot(trad_year, aes(x = year, y = gap_mean)) +
  geom_ribbon(aes(ymin = gap_mean - gap_sd, ymax = gap_mean + gap_sd), alpha = 0.20, fill = "darkorange") +
  geom_line(linewidth = 1, color = "darkorange4") +
  geom_point(size = 2, color = "darkorange4") +
  labs(
    title = "Trad counties: LFPR gap over time (mean +/- 1 SD)",
    x = "Year",
    y = "LFPR gap (male - female)"
  ) +
  theme_minimal(base_size = 12)

save_plot("trad_lfpr_gap_time_trend_with_sd.png", { print(p_trad_gap) }, width = 1700, height = 1100)

# 5c) Trad group: median HH income vs female LFPR, colored by year
trad_scatter_year <- lfpr_panel %>%
  filter(trad == 1) %>%
  group_by(year) %>%
  summarise(
    median_hh_income_median = median(median_hh_income, na.rm = TRUE),
    female_lfpr_mean = mean(lfpr_female, na.rm = TRUE),
    .groups = "drop"
  )

p_trad_income_female <- ggplot(trad_scatter_year, aes(x = median_hh_income_median, y = female_lfpr_mean, color = year)) +
  geom_path(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 3) +
  labs(
    title = "Trad counties: median HH income vs female LFPR by year",
    x = "Median household income (median across trad counties)",
    y = "Female LFPR mean (trad counties)",
    color = "Year"
  ) +
  theme_minimal(base_size = 12)

save_plot("trad_median_income_vs_female_lfpr_by_year.png", { print(p_trad_income_female) }, width = 1700, height = 1100)

# 5d) Female LFPR vs log income, grouped comparisons
group_scatter_data <- lfpr_panel %>%
  mutate(
    group_label = case_when(
      trad == 1 ~ "trad",
      asp_trad == 1 ~ "asp_trad",
      dem_solid_poor == 1 ~ "dem_solid_poor",
      dem_solid_rich == 1 ~ "dem_solid_rich",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(group_label), !is.na(lfpr_female), is.finite(log_income))

p_groups_pooled <- ggplot(group_scatter_data, aes(x = log_income, y = lfpr_female, color = group_label)) +
  geom_point(alpha = 0.25, size = 0.9) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, linewidth = 0.9) +
  labs(
    title = "Female LFPR vs log income by grouping (all years pooled)",
    x = "log(median household income)",
    y = "Female LFPR (20-64)",
    color = "Group"
  ) +
  theme_minimal(base_size = 12)

save_plot("female_lfpr_vs_logincome_groups_pooled.png", { print(p_groups_pooled) }, width = 1900, height = 1200)

p_groups_facet_year <- ggplot(group_scatter_data, aes(x = log_income, y = lfpr_female, color = group_label)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, linewidth = 0.7) +
  facet_wrap(~year, ncol = 5) +
  labs(
    title = "Female LFPR vs log income by grouping (faceted by year)",
    x = "log(median household income)",
    y = "Female LFPR (20-64)",
    color = "Group"
  ) +
  theme_minimal(base_size = 11)

save_plot("female_lfpr_vs_logincome_groups_facet_year.png", { print(p_groups_facet_year) }, width = 2400, height = 1500)

p_gap_groups_facet_year <- ggplot(group_scatter_data, aes(x = log_income, y = lfpr_gap, color = group_label)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, linewidth = 0.7) +
  facet_wrap(~year, ncol = 5) +
  labs(
    title = "LFPR gap vs log income by grouping (faceted by year)",
    x = "log(median household income)",
    y = "LFPR gap (male - female, 20-64)",
    color = "Group"
  ) +
  theme_minimal(base_size = 11)

save_plot("lfpr_gap_vs_logincome_groups_facet_year.png", { print(p_gap_groups_facet_year) }, width = 2400, height = 1500)

group_scatter_data_two <- group_scatter_data %>%
  filter(group_label %in% c("trad", "dem_solid_rich"))

p_groups_two_facet_year <- ggplot(group_scatter_data_two, aes(x = log_income, y = lfpr_female, color = group_label)) +
  geom_point(alpha = 0.28, size = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, linewidth = 0.8) +
  facet_wrap(~year, ncol = 5) +
  labs(
    title = "Female LFPR vs log income: trad vs dem_solid_rich (faceted by year)",
    x = "log(median household income)",
    y = "Female LFPR (20-64)",
    color = "Group"
  ) +
  theme_minimal(base_size = 11)

save_plot("female_lfpr_vs_logincome_trad_vs_demsolidrich_facet_year.png", { print(p_groups_two_facet_year) }, width = 2400, height = 1500)

# 5e) For each group, plot male and female LFPR vs log income (faceted by year)
plot_male_female_by_group <- function(df, group_var, group_name, filename) {
  d <- df %>%
    filter(.data[[group_var]] == 1, is.finite(log_income), !is.na(lfpr_male), !is.na(lfpr_female)) %>%
    select(year, log_income, lfpr_male, lfpr_female) %>%
    pivot_longer(
      cols = c(lfpr_male, lfpr_female),
      names_to = "sex",
      values_to = "lfpr"
    ) %>%
    mutate(
      sex = recode(sex, lfpr_male = "male", lfpr_female = "female")
    )

  p <- ggplot(d, aes(x = log_income, y = lfpr, color = sex)) +
    geom_point(alpha = 0.12, size = 0.6) +
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, linewidth = 0.8) +
    facet_wrap(~year, ncol = 5) +
    scale_color_manual(values = c(male = "steelblue4", female = "tomato3")) +
    labs(
      title = paste("Male vs female LFPR vs log income:", group_name),
      x = "log(median household income)",
      y = "LFPR (20-64)",
      color = "Series"
    ) +
    theme_minimal(base_size = 11)

  save_plot(filename, { print(p) }, width = 2400, height = 1500)
}

plot_male_female_by_group(
  lfpr_panel,
  group_var = "trad",
  group_name = "trad",
  filename = "male_female_lfpr_vs_logincome_trad_facet_year.png"
)

plot_male_female_by_group(
  lfpr_panel,
  group_var = "asp_trad",
  group_name = "asp_trad",
  filename = "male_female_lfpr_vs_logincome_asp_trad_facet_year.png"
)

plot_male_female_by_group(
  lfpr_panel,
  group_var = "dem_solid_poor",
  group_name = "dem_solid_poor",
  filename = "male_female_lfpr_vs_logincome_dem_solid_poor_facet_year.png"
)

plot_male_female_by_group(
  lfpr_panel,
  group_var = "dem_solid_rich",
  group_name = "dem_solid_rich",
  filename = "male_female_lfpr_vs_logincome_dem_solid_rich_facet_year.png"
)
