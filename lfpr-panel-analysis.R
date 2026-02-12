library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(ggplot2)
library(knitr)
library(tidycensus)

source("R/paths.R")
source("functions.R")

# =========================================================
# 0) Configuration
# =========================================================
start_year <- 2010
end_year <- 2020
survey_type <- "acs5" # full county coverage; endpoints overlap by design
panel_years <- start_year:end_year
income_base_year <- 2023

# Annual average R-CPI-U-RS values (MONTH == 13) from Census inflation API.
# This matches ACS guidance for rebasing dollar-denominated estimates.
cpi_u_rs_annual <- tibble::tribble(
  ~year, ~cpi_u_rs,
  2010, 236.876,
  2011, 241.949,
  2012, 245.245,
  2013, 248.004,
  2014, 252.156,
  2015, 252.553,
  2016, 255.401,
  2017, 260.609,
  2018, 266.818,
  2019, 271.917,
  2020, 275.665,
  2021, 289.268,
  2022, 313.761,
  2023, 329.725,
  2024, 344.667
)

panel_dir <- data_path("processed", "panel")
results_dir <- data_path("processed", "results")
ensure_dir(panel_dir)
ensure_dir(results_dir)

# =========================================================
# 1) Pull county LFPR + household income panel from Census
# =========================================================
# Build consistent 20-64 LFPR from detailed counts in B23001:
# LFPR = (in labor force count / total population count) * 100
# This avoids subject-table definition shifts across years.
get_b23001_lfpr_var_ids <- function(year, survey = "acs5") {
  ages_re <- paste(
    c(
      "20 (and|to) 21 years", "22 to 24 years", "25 to 29 years",
      "30 to 34 years", "35 to 44 years", "45 to 54 years",
      "55 to 59 years", "60 (and|to) 61 years", "62 to 64 years"
    ),
    collapse = "|"
  )

  vars <- load_variables(year, survey, cache = TRUE) %>%
    filter(str_detect(name, "^B23001_")) %>%
    select(name, label)

  pick_ids <- function(sex, labor_force_only = FALSE) {
    if (labor_force_only) {
      base <- vars %>%
        filter(str_detect(label, paste0("^Estimate!!Total:?!!", sex, ":?!!(", ages_re, "):?!!In labor force:?$")))
    } else {
      base <- vars %>%
        filter(str_detect(label, paste0("^Estimate!!Total:?!!", sex, ":?!!(", ages_re, "):?$")))
    }
    unique(base$name)
  }

  out <- list(
    male_denom = pick_ids("Male", labor_force_only = FALSE),
    male_num = pick_ids("Male", labor_force_only = TRUE),
    female_denom = pick_ids("Female", labor_force_only = FALSE),
    female_num = pick_ids("Female", labor_force_only = TRUE)
  )

  if (any(lengths(out) == 0)) {
    stop(sprintf("B23001 mapping failed for year %s. Could not identify all sex-age LFPR components.", year))
  }
  out
}

pull_lfpr_income_panel <- function(years = 2010:2020, survey = "acs5") {
  income_var <- c(median_hh_income = "B19013_001")

  map_dfr(years, function(y) {
    message("Pulling ACS county panel for endpoint year: ", y)
    ids <- get_b23001_lfpr_var_ids(y, survey = survey)

    var_ids <- unique(c(
      income_var,
      ids$male_denom, ids$male_num,
      ids$female_denom, ids$female_num
    ))
    var_ids <- var_ids[!is.na(var_ids) & nzchar(var_ids)]

    named_vars <- c(median_hh_income = "B19013_001")
    for (id in setdiff(var_ids, "B19013_001")) {
      named_vars[id] <- id
    }

    acs_pull <- get_acs(
      geography = "county",
      variables = named_vars,
      survey = survey,
      year = y,
      output = "wide"
    )

    male_denom_cols <- paste0(ids$male_denom, "E")
    male_num_cols <- paste0(ids$male_num, "E")
    female_denom_cols <- paste0(ids$female_denom, "E")
    female_num_cols <- paste0(ids$female_num, "E")

    out <- acs_pull %>%
      mutate(
        male_denom = rowSums(across(all_of(male_denom_cols)), na.rm = TRUE),
        male_num = rowSums(across(all_of(male_num_cols)), na.rm = TRUE),
        female_denom = rowSums(across(all_of(female_denom_cols)), na.rm = TRUE),
        female_num = rowSums(across(all_of(female_num_cols)), na.rm = TRUE)
      ) %>%
      transmute(
        year = y,
        fips = str_pad(GEOID, 5, pad = "0"),
        county = toupper(gsub(" COUNTY, .*", "", NAME)),
        state = toupper(sub(".*, ", "", NAME)),
        lfpr_male = ifelse(male_denom > 0, 100 * male_num / male_denom, NA_real_),
        lfpr_female = ifelse(female_denom > 0, 100 * female_num / female_denom, NA_real_),
        lfpr_total = ifelse(
          (male_denom + female_denom) > 0,
          100 * (male_num + female_num) / (male_denom + female_denom),
          NA_real_
        ),
        lfpr_female_kids = NA_real_,
        lfpr_female_kids_u6 = NA_real_,
        lfpr_female_kids_u6_17 = NA_real_,
        lfpr_female_kids_6_17 = NA_real_,
        median_hh_income = median_hh_incomeE,
        source = paste0("ACS_", survey, "_B23001_B19013")
      ) %>%
      mutate(
        lfpr_gap = lfpr_male - lfpr_female
      )

    out
  })
}

lfpr_panel <- pull_lfpr_income_panel(years = panel_years, survey = survey_type)

# Convert median household income to a constant-dollar series.
if (!income_base_year %in% cpi_u_rs_annual$year) {
  stop(sprintf("income_base_year=%s not found in CPI lookup. Extend cpi_u_rs_annual.", income_base_year))
}

missing_cpi_years <- setdiff(unique(lfpr_panel$year), cpi_u_rs_annual$year)
if (length(missing_cpi_years) > 0) {
  stop(sprintf(
    "Missing CPI values for panel year(s): %s. Extend cpi_u_rs_annual.",
    paste(sort(missing_cpi_years), collapse = ", ")
  ))
}

base_cpi <- cpi_u_rs_annual$cpi_u_rs[cpi_u_rs_annual$year == income_base_year][1]

lfpr_panel <- lfpr_panel %>%
  left_join(cpi_u_rs_annual, by = "year") %>%
  mutate(
    median_hh_income_nominal = median_hh_income,
    median_hh_income_real = median_hh_income_nominal * (base_cpi / cpi_u_rs),
    income_base_year = income_base_year,
    # Use constant-dollar income in regressions and plots.
    log_income = log(median_hh_income_real)
  )

# =========================================================
# 2) Merge presidential vote metrics and carry forward (LOCF)
# =========================================================
pres_path <- data_path("raw", "countypres_2000-2024.csv")
pres_df <- read_csv(pres_path, show_col_types = FALSE)

pres_summary <- pres_df %>%
  group_by(year, county_fips, party) %>%
  summarise(
    totalcanvotes = sum(candidatevotes, na.rm = TRUE),
    totalvotes = max(totalvotes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(party), party != "")

pres_wide <- pres_summary %>%
  pivot_wider(names_from = party, values_from = totalcanvotes, values_fill = 0) %>%
  group_by(year) %>%
  mutate(
    fips = str_pad(as.character(county_fips), 5, pad = "0"),
    vote_spread = DEMOCRAT - REPUBLICAN,
    vote_margin = ifelse((DEMOCRAT + REPUBLICAN) > 0, (DEMOCRAT - REPUBLICAN) / (DEMOCRAT + REPUBLICAN), NA_real_),
    dem_percent = ifelse(totalvotes > 0, DEMOCRAT / totalvotes, NA_real_),
    rep_percent = ifelse(totalvotes > 0, REPUBLICAN / totalvotes, NA_real_),
    # Signed within-year normalization to [-1, 1] for comparable color scales.
    vote_spread_norm = {
      max_abs_spread <- suppressWarnings(max(abs(vote_spread), na.rm = TRUE))
      if (is.finite(max_abs_spread) && max_abs_spread > 0) {
        vote_spread / max_abs_spread
      } else {
        rep(NA_real_, dplyr::n())
      }
    }
  ) %>%
  ungroup() %>%
  select(fips, year, vote_spread, vote_spread_norm, vote_margin, dem_percent, rep_percent)

pres_locf <- expand_grid(
  fips = unique(lfpr_panel$fips),
  year = panel_years
) %>%
  left_join(pres_wide, by = c("fips", "year")) %>%
  group_by(fips) %>%
  arrange(year, .by_group = TRUE) %>%
  fill(vote_spread, vote_spread_norm, vote_margin, dem_percent, rep_percent, .direction = "down") %>%
  ungroup()

lfpr_panel <- lfpr_panel %>%
  left_join(pres_locf, by = c("fips", "year")) %>%
  group_by(year) %>%
  mutate(income_quintile_national = ntile(median_hh_income_real, 5)) %>%
  ungroup() %>%
  group_by(state, year) %>%
  mutate(
    income_quintile_state = if (sum(!is.na(median_hh_income_real)) >= 5) {
      ntile(median_hh_income_real, 5)
    } else {
      rep(NA_integer_, n())
    }
  ) %>%
  ungroup()

panel_file <- dated_path(panel_dir, "lfpr_panel.csv")
write_csv(lfpr_panel, panel_file)

# =========================================================
# 3) Year-specific regressions (linear + quadratic)
# =========================================================
safe_lm <- function(df, y_var, spec = c("linear", "quadratic")) {
  spec <- match.arg(spec)
  if (spec == "linear") {
    frm <- as.formula(paste(y_var, "~ log_income"))
  } else {
    frm <- as.formula(paste(y_var, "~ log_income + I(log_income^2)"))
  }
  lm(frm, data = df)
}

extract_model_tables <- function(model, year, outcome, spec) {
  sm <- summary(model)
  coefs <- as.data.frame(sm$coefficients)
  coefs$term <- rownames(coefs)
  rownames(coefs) <- NULL

  coef_tbl <- coefs %>%
    transmute(
      year = year,
      outcome = outcome,
      spec = spec,
      term = term,
      estimate = Estimate,
      std_error = `Std. Error`,
      statistic = `t value`,
      p_value = `Pr(>|t|)`,
      n = length(model$fitted.values),
      r2 = sm$r.squared,
      adj_r2 = sm$adj.r.squared
    )

  summary_tbl <- tibble(
    year = year,
    outcome = outcome,
    spec = spec,
    n = length(model$fitted.values),
    r2 = sm$r.squared,
    adj_r2 = sm$adj.r.squared,
    sigma = sm$sigma
  )

  list(coef_tbl = coef_tbl, summary_tbl = summary_tbl)
}

outcomes <- c("lfpr_total", "lfpr_gap", "lfpr_female")
specs <- c("linear", "quadratic")

model_jobs <- expand_grid(year = panel_years, outcome = outcomes, spec = specs)

model_results <- pmap(model_jobs, function(year, outcome, spec) {
  df_year <- lfpr_panel %>%
    filter(year == !!year) %>%
    filter(!is.na(.data[[outcome]]), is.finite(log_income), !is.na(log_income))

  if (nrow(df_year) < 30) {
    return(list(
      coef_tbl = tibble(),
      summary_tbl = tibble(
        year = year, outcome = outcome, spec = spec, n = nrow(df_year),
        r2 = NA_real_, adj_r2 = NA_real_, sigma = NA_real_
      )
    ))
  }

  model <- safe_lm(df_year, y_var = outcome, spec = spec)
  extract_model_tables(model, year = year, outcome = outcome, spec = spec)
})

coef_table <- bind_rows(map(model_results, "coef_tbl"))
model_summary <- bind_rows(map(model_results, "summary_tbl"))

coef_csv <- dated_path(results_dir, "lfpr_model_coefficients.csv")
summary_csv <- dated_path(results_dir, "lfpr_model_summary.csv")
write_csv(coef_table, coef_csv)
write_csv(model_summary, summary_csv)

tex_file <- dated_path(results_dir, "lfpr_model_tables.tex")
sink(tex_file)
cat("% Auto-generated model tables\n")
cat("\\section*{LFPR Panel Model Summary}\n")
print(kable(model_summary, format = "latex", booktabs = TRUE, longtable = TRUE, digits = 4))
cat("\n\\section*{LFPR Panel Coefficients}\n")
print(kable(coef_table, format = "latex", booktabs = TRUE, longtable = TRUE, digits = 4))
sink()

# =========================================================
# 4) State-level yearly facets
# =========================================================
fit_formula <- y ~ x + I(x^2)

plot_state_year_facets <- function(df, state_name, outcome, color_mode = c("none", "vote_margin", "vote_spread", "q_nat", "q_state")) {
  color_mode <- match.arg(color_mode)
  state_df <- df %>% filter(state == state_name, !is.na(.data[[outcome]]), is.finite(log_income))
  if (nrow(state_df) == 0) {
    return(invisible(NULL))
  }

  p <- ggplot(state_df, aes(x = log_income, y = .data[[outcome]])) +
    geom_point(alpha = 0.6, size = 1.0) +
    geom_smooth(method = "lm", formula = fit_formula, se = FALSE, color = "black", linewidth = 0.6) +
    facet_wrap(~year, ncol = 5) +
    labs(
      title = paste(state_name, "-", outcome, "-", color_mode),
      x = "log(median household income)",
      y = outcome
    ) +
    theme_minimal(base_size = 11)

  if (color_mode == "vote_margin") {
    p <- ggplot(state_df, aes(x = log_income, y = .data[[outcome]], color = vote_margin)) +
      geom_point(alpha = 0.7, size = 1.0) +
      geom_smooth(method = "lm", formula = fit_formula, se = FALSE, color = "black", linewidth = 0.6) +
      scale_color_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "grey70") +
      facet_wrap(~year, ncol = 5) +
      labs(title = paste(state_name, "-", outcome, "- vote_margin"), x = "log(median household income)", y = outcome, color = "vote_margin") +
      theme_minimal(base_size = 11)
  }

  if (color_mode == "vote_spread") {
    p <- ggplot(state_df, aes(x = log_income, y = .data[[outcome]], color = vote_spread_norm)) +
      geom_point(alpha = 0.7, size = 1.0) +
      geom_smooth(method = "lm", formula = fit_formula, se = FALSE, color = "black", linewidth = 0.6) +
      scale_color_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "grey70") +
      facet_wrap(~year, ncol = 5) +
      labs(title = paste(state_name, "-", outcome, "- vote_spread (normalized)"), x = "log(median household income)", y = outcome, color = "vote_spread_norm") +
      theme_minimal(base_size = 11)
  }

  if (color_mode == "q_nat") {
    p <- ggplot(state_df, aes(x = log_income, y = .data[[outcome]], color = factor(income_quintile_national))) +
      geom_point(alpha = 0.7, size = 1.0) +
      geom_smooth(method = "lm", formula = fit_formula, se = FALSE, color = "black", linewidth = 0.6) +
      facet_wrap(~year, ncol = 5) +
      labs(title = paste(state_name, "-", outcome, "- national income quintile"), x = "log(median household income)", y = outcome, color = "nat quintile") +
      theme_minimal(base_size = 11)
  }

  if (color_mode == "q_state") {
    p <- ggplot(state_df, aes(x = log_income, y = .data[[outcome]], color = factor(income_quintile_state))) +
      geom_point(alpha = 0.7, size = 1.0) +
      geom_smooth(method = "lm", formula = fit_formula, se = FALSE, color = "black", linewidth = 0.6) +
      facet_wrap(~year, ncol = 5) +
      labs(title = paste(state_name, "-", outcome, "- state income quintile"), x = "log(median household income)", y = outcome, color = "state quintile") +
      theme_minimal(base_size = 11)
  }

  safe_state <- gsub("[^A-Za-z0-9]+", "_", state_name)
  save_plot(paste0("facet_", safe_state, "_", outcome, "_", color_mode, ".png"), { print(p) }, width = 2200, height = 1500)
}

states <- sort(unique(lfpr_panel$state))
for (s in states) {
  for (outcome in outcomes) {
    plot_state_year_facets(lfpr_panel, s, outcome, "none")
    plot_state_year_facets(lfpr_panel, s, outcome, "vote_margin")
    plot_state_year_facets(lfpr_panel, s, outcome, "vote_spread")
    plot_state_year_facets(lfpr_panel, s, outcome, "q_nat")
    plot_state_year_facets(lfpr_panel, s, outcome, "q_state")
  }
}

# =========================================================
# 5) 2010 to 2020 vector plots
# =========================================================
plot_vectors_2010_2020 <- function(df, outcome, color_mode = c("vote_margin", "vote_spread", "q_nat", "q_state")) {
  color_mode <- match.arg(color_mode)
  start_y <- min(panel_years)
  end_y <- max(panel_years)

  tmp <- df %>%
    filter(year %in% c(start_y, end_y)) %>%
    select(fips, state, county, year, log_income, all_of(outcome), vote_margin, vote_spread, vote_spread_norm, income_quintile_national, income_quintile_state) %>%
    pivot_wider(
      names_from = year,
      values_from = c(log_income, all_of(outcome), vote_margin, vote_spread, vote_spread_norm, income_quintile_national, income_quintile_state),
      names_sep = "_"
    ) %>%
    filter(!is.na(.data[[paste0("log_income_", start_y)]]), !is.na(.data[[paste0("log_income_", end_y)]])) %>%
    filter(!is.na(.data[[paste0(outcome, "_", start_y)]]), !is.na(.data[[paste0(outcome, "_", end_y)]]))

  if (nrow(tmp) == 0) {
    return(invisible(NULL))
  }

  p <- ggplot(tmp) +
    geom_segment(
      aes(
        x = .data[[paste0("log_income_", start_y)]],
        y = .data[[paste0(outcome, "_", start_y)]],
        xend = .data[[paste0("log_income_", end_y)]],
        yend = .data[[paste0(outcome, "_", end_y)]]
      ),
      arrow = arrow(length = unit(0.08, "inches")),
      alpha = 0.45
    ) +
    labs(
      title = paste(outcome, "movement:", start_y, "to", end_y, "-", color_mode),
      x = paste0("log(median household income), ", start_y, " to ", end_y),
      y = paste0(outcome, ", ", start_y, " to ", end_y)
    ) +
    theme_minimal(base_size = 11)

  if (color_mode == "vote_margin") {
    p <- p +
      geom_point(
        aes(
          x = .data[[paste0("log_income_", end_y)]],
          y = .data[[paste0(outcome, "_", end_y)]],
          color = .data[[paste0("vote_margin_", end_y)]]
        ),
        size = 1.2
      ) +
      scale_color_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "grey70")
  }

  if (color_mode == "vote_spread") {
    p <- p +
      geom_point(
        aes(
          x = .data[[paste0("log_income_", end_y)]],
          y = .data[[paste0(outcome, "_", end_y)]],
          color = .data[[paste0("vote_spread_norm_", end_y)]]
        ),
        size = 1.2
      ) +
      scale_color_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "grey70")
  }

  if (color_mode == "q_nat") {
    p <- p +
      geom_point(
        aes(
          x = .data[[paste0("log_income_", end_y)]],
          y = .data[[paste0(outcome, "_", end_y)]],
          color = factor(.data[[paste0("income_quintile_national_", end_y)]])
        ),
        size = 1.2
      )
  }

  if (color_mode == "q_state") {
    p <- p +
      geom_point(
        aes(
          x = .data[[paste0("log_income_", end_y)]],
          y = .data[[paste0(outcome, "_", end_y)]],
          color = factor(.data[[paste0("income_quintile_state_", end_y)]])
        ),
        size = 1.2
      )
  }

  save_plot(paste0("vector_", outcome, "_", color_mode, "_", start_y, "_", end_y, ".png"), { print(p) }, width = 1800, height = 1200)
}

for (outcome in outcomes) {
  plot_vectors_2010_2020(lfpr_panel, outcome, "vote_margin")
  plot_vectors_2010_2020(lfpr_panel, outcome, "vote_spread")
  plot_vectors_2010_2020(lfpr_panel, outcome, "q_nat")
  plot_vectors_2010_2020(lfpr_panel, outcome, "q_state")
}

# =========================================================
# 6) National election-year comparison (2012, 2016, 2020)
# =========================================================
plot_national_election_years <- function(df, outcome, color_var) {
  years_keep <- c(2012, 2016, 2020)
  d <- df %>%
    filter(year %in% years_keep, !is.na(.data[[outcome]]), is.finite(log_income), !is.na(.data[[color_var]]))

  if (nrow(d) == 0) {
    return(invisible(NULL))
  }

  quintile_lines <- d %>%
    group_by(year) %>%
    summarise(
      q20 = quantile(log_income, 0.2, na.rm = TRUE),
      q40 = quantile(log_income, 0.4, na.rm = TRUE),
      q60 = quantile(log_income, 0.6, na.rm = TRUE),
      q80 = quantile(log_income, 0.8, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(q20, q40, q60, q80), names_to = "q", values_to = "cutoff")

  p <- ggplot(d, aes(x = log_income, y = .data[[outcome]], color = .data[[color_var]])) +
    geom_point(alpha = 0.6, size = 0.8) +
    geom_smooth(method = "lm", formula = fit_formula, se = FALSE, color = "black", linewidth = 0.7) +
    geom_vline(data = quintile_lines, aes(xintercept = cutoff), inherit.aes = FALSE, linetype = "dashed", color = "grey35", linewidth = 0.4) +
    facet_wrap(~year, ncol = 3) +
    labs(
      title = paste("National counties:", outcome, "-", color_var, "(election years)"),
      x = "log(median household income)",
      y = outcome,
      color = color_var
    ) +
    theme_minimal(base_size = 11)

  if (color_var %in% c("vote_margin", "vote_spread")) {
    p <- p + scale_color_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "grey70")
  } else {
    p <- p + scale_color_gradient(low = "red", high = "blue", na.value = "grey70")
  }

  save_plot(paste0("national_election_years_", outcome, "_", color_var, ".png"), { print(p) }, width = 2200, height = 900)
}

for (outcome in outcomes) {
  plot_national_election_years(lfpr_panel, outcome, "vote_spread_norm")
  plot_national_election_years(lfpr_panel, outcome, "vote_margin")
  plot_national_election_years(lfpr_panel, outcome, "rep_percent")
  plot_national_election_years(lfpr_panel, outcome, "dem_percent")
}

# =========================================================
# 8) Requested non-log income visuals
# =========================================================
plot_vector_nonlog_income <- function(df, outcome, color_mode = c("q_state")) {
  color_mode <- match.arg(color_mode)
  start_y <- min(panel_years)
  end_y <- max(panel_years)

  tmp <- df %>%
    filter(year %in% c(start_y, end_y)) %>%
    select(
      fips, state, county, year,
      median_hh_income_real, all_of(outcome),
      income_quintile_state
    ) %>%
    pivot_wider(
      names_from = year,
      values_from = c(median_hh_income_real, all_of(outcome), income_quintile_state),
      names_sep = "_"
    ) %>%
    filter(
      !is.na(.data[[paste0("median_hh_income_real_", start_y)]]),
      !is.na(.data[[paste0("median_hh_income_real_", end_y)]]),
      !is.na(.data[[paste0(outcome, "_", start_y)]]),
      !is.na(.data[[paste0(outcome, "_", end_y)]])
    )

  if (nrow(tmp) == 0) {
    return(invisible(NULL))
  }

  p <- ggplot(tmp) +
    geom_segment(
      aes(
        x = .data[[paste0("median_hh_income_real_", start_y)]],
        y = .data[[paste0(outcome, "_", start_y)]],
        xend = .data[[paste0("median_hh_income_real_", end_y)]],
        yend = .data[[paste0(outcome, "_", end_y)]]
      ),
      arrow = arrow(length = unit(0.08, "inches")),
      alpha = 0.45
    ) +
    geom_point(
      aes(
        x = .data[[paste0("median_hh_income_real_", end_y)]],
        y = .data[[paste0(outcome, "_", end_y)]],
        color = factor(.data[[paste0("income_quintile_state_", end_y)]])
      ),
      size = 1.2
    ) +
    scale_x_continuous(labels = scales::label_dollar()) +
    labs(
      title = paste(outcome, "movement:", start_y, "to", end_y, "- q_state (non-log income)"),
      x = paste0("median household income (", income_base_year, " dollars), ", start_y, " to ", end_y),
      y = paste0(outcome, ", ", start_y, " to ", end_y),
      color = "state quintile"
    ) +
    theme_minimal(base_size = 11)

  save_plot(
    paste0("vector_", outcome, "_q_state_", start_y, "_", end_y, "_nonlog_income.png"),
    { print(p) },
    width = 1800,
    height = 1200
  )
}

plot_national_election_years_nonlog_income <- function(df, outcome, color_var) {
  years_keep <- c(2012, 2016, 2020)
  d <- df %>%
    filter(
      year %in% years_keep,
      !is.na(.data[[outcome]]),
      is.finite(median_hh_income_real),
      !is.na(.data[[color_var]])
    )

  if (nrow(d) == 0) {
    return(invisible(NULL))
  }

  quintile_lines <- d %>%
    group_by(year) %>%
    summarise(
      q20 = quantile(median_hh_income_real, 0.2, na.rm = TRUE),
      q40 = quantile(median_hh_income_real, 0.4, na.rm = TRUE),
      q60 = quantile(median_hh_income_real, 0.6, na.rm = TRUE),
      q80 = quantile(median_hh_income_real, 0.8, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(q20, q40, q60, q80), names_to = "q", values_to = "cutoff")

  p <- ggplot(d, aes(x = median_hh_income_real, y = .data[[outcome]], color = .data[[color_var]])) +
    geom_point(alpha = 0.6, size = 0.8) +
    geom_smooth(method = "lm", formula = fit_formula, se = FALSE, color = "black", linewidth = 0.7) +
    geom_vline(
      data = quintile_lines,
      aes(xintercept = cutoff),
      linetype = "dashed",
      color = "grey35",
      linewidth = 0.4
    ) +
    scale_x_continuous(labels = scales::label_dollar()) +
    facet_wrap(~year, ncol = 3) +
    labs(
      title = paste("National counties:", outcome, "-", color_var, "(election years, non-log income)"),
      x = paste0("median household income (", income_base_year, " dollars)"),
      y = outcome,
      color = color_var
    ) +
    theme_minimal(base_size = 11)

  if (color_var %in% c("vote_margin", "vote_spread_norm")) {
    p <- p + scale_color_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "grey70")
  } else {
    p <- p + scale_color_gradient(low = "red", high = "blue", na.value = "grey70")
  }

  save_plot(
    paste0("national_election_years_", outcome, "_", color_var, "_nonlog_income.png"),
    { print(p) },
    width = 2200,
    height = 900
  )
}

plot_vector_nonlog_income(lfpr_panel, "lfpr_total", "q_state")
for (outcome in c("lfpr_total", "lfpr_gap")) {
  plot_national_election_years_nonlog_income(lfpr_panel, outcome, "vote_spread_norm")
  plot_national_election_years_nonlog_income(lfpr_panel, outcome, "vote_margin")
  plot_national_election_years_nonlog_income(lfpr_panel, outcome, "rep_percent")
  plot_national_election_years_nonlog_income(lfpr_panel, outcome, "dem_percent")
}

message("Pipeline complete.")
message("Panel file: ", panel_file)
message("Model tables: ", tex_file)
