library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(lubridate)
library(fredr)

# ---- Config ----
start_year <- 2000
end_year <- 2023
sleep_seconds <- 0.6
out_dir <- "data/fred"

fred_key <- Sys.getenv("FRED_API_KEY")
if (fred_key == "") {
  stop("FRED_API_KEY is not set. Set it in your environment before running.")
}
fredr_set_key(fred_key)

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

start_date <- as.Date(sprintf("%d-01-01", start_year))
end_date <- as.Date(sprintf("%d-12-31", end_year))

fred_search_all <- function(search_fun, ...) {
  limit <- 1000
  offset <- 0
  out <- list()
  repeat {
    res <- search_fun(..., limit = limit, offset = offset)
    out[[length(out) + 1]] <- res
    if (nrow(res) < limit) break
    offset <- offset + limit
  }
  bind_rows(out)
}

# ---- Series discovery ----
# County labor force series via tags to avoid 5000-result search limit
lf_series <- fred_search_all(
  fredr_tags_series,
  tag_names = "county;labor force"
) %>%
  filter(frequency == "Annual") %>%
  filter(str_detect(id, "^LAUCN\\d{15}A$")) %>%
  mutate(fips = substr(id, 6, 10))

write_csv(lf_series, file.path(out_dir, "series_labor_force.csv"))

# County per capita personal income series (PCPI + 5-digit FIPS)
pcpi_series <- fred_search_all(
  fredr_series_search_id,
  search_text = "PCPI",
  filter_variable = "frequency",
  filter_value = "Annual"
) %>%
  filter(str_detect(id, "^PCPI\\d{5}$")) %>%
  mutate(fips = substr(id, 5, 9))

write_csv(pcpi_series, file.path(out_dir, "series_pcpi.csv"))

# County population series (used to compute LFPR if found)
pop_series <- fred_search_all(
  fredr_tags_series,
  tag_names = "county;population"
) %>%
  filter(frequency == "Annual") %>%
  filter(str_detect(id, "\\d{5}$")) %>%
  mutate(fips = substr(id, nchar(id) - 4, nchar(id)))

write_csv(pop_series, file.path(out_dir, "series_population.csv"))

get_series_obs <- function(series_ids, start_date, end_date, sleep_seconds = 0.1) {
  map_dfr(series_ids, function(id) {
    Sys.sleep(sleep_seconds)
    fredr(
      series_id = id,
      observation_start = start_date,
      observation_end = end_date
    ) %>%
      mutate(series_id = id)
  })
}

# ---- Observations ----
lf_obs <- get_series_obs(lf_series$id, start_date, end_date, sleep_seconds) %>%
  transmute(
    fips = substr(series_id, 6, 10),
    year = year(date),
    labor_force = value
  )

pcpi_obs <- get_series_obs(pcpi_series$id, start_date, end_date, sleep_seconds) %>%
  transmute(
    fips = substr(series_id, 5, 9),
    year = year(date),
    pcpi = value
  )

pop_obs <- tibble()
if (nrow(pop_series) > 0) {
  pop_obs <- get_series_obs(pop_series$id, start_date, end_date, sleep_seconds) %>%
    transmute(
      fips = substr(series_id, nchar(series_id) - 4, nchar(series_id)),
      year = year(date),
      population = value
    )
}

county_panel <- full_join(lf_obs, pcpi_obs, by = c("fips", "year")) %>%
  full_join(pop_obs, by = c("fips", "year")) %>%
  mutate(
    lfpr = ifelse(!is.na(population) & population > 0, 100 * labor_force / population, NA_real_)
  ) %>%
  arrange(fips, year)

write_csv(county_panel, file.path(out_dir, "fred_county_laborforce_pcpi_panel.csv"))

# ---- Notes ----
# FRED county labor force is total civilian labor force (not gender-specific).
# The computed lfpr uses the population series if available; verify its definition.
