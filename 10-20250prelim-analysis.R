library(tidyr)
library(base)
library(readxl)
library(dplyr)
library(labelled)
library(purrr)


########## LOAD IN RAW DATA ###############
getwd()
# load census data 
census_df = read.csv("/Users/allegrasaggese/Library/CloudStorage/Dropbox/Tradwives/data/Census-ACSST5Y2023.S2301-Data.csv")

# load BEA data 
bea_Y_df_raw = read_excel("/Users/allegrasaggese/Library/CloudStorage/Dropbox/Tradwives/data/BEA-county-per-capita-income.xlsx", 
                      sheet = 1,
                      skip = 3)

bea_Y_df = read_excel("/Users/allegrasaggese/Library/CloudStorage/Dropbox/Tradwives/data/BEA-county-per-capita-income_edited.xlsx", 
                      sheet = 1,
                      skip = 3)

bea_GDP_df_raw = read_excel("/Users/allegrasaggese/Library/CloudStorage/Dropbox/Tradwives/data/BEA-county-percapita-GDP.xlsx", 
                      sheet = 1,
                      skip = 3)

bea_GDP_df = read_excel("/Users/allegrasaggese/Library/CloudStorage/Dropbox/Tradwives/data/BEA-county-percapita-GDP-edits.xlsx", 
                        sheet = 1,
                        skip = 3)


########## CLEAN RAW DATA ###############
# subset census data 
keep_var <- c("S2301_C02_022E", "S2301_C02_023E", "S2301_C02_024E", "S2301_C02_025E",
              "S2301_C02_026E", "S2301_C02_027E", "S2301_C03_023E", "S2301_C03_024E",
              "S2301_C03_025E", "S2301_C03_026E", "S2301_C03_027E", "GEO_ID", "NAME")

census_short <- subset(census_df, select = keep_var)
census_short$year <- "2023"

# separate county and year for grouping
census_short_2 <- census_short %>% separate(NAME, into = c("county", "state"), sep = ",\\s*")
census_short_2 <- census_short_2 %>%
  mutate(
    county = toupper(trimws(county)),
    state  = toupper(trimws(state)),
    county = sub("\\s*COUNTY\\s*$", "", county)
  )

description_cols <- as.vector(as.character(unlist(census_short[1, ])))
print(description_cols)

census_clean <- census_short_2[-1, ] # drop the first naming row from excel 
pre_col_list <-colnames(census_clean)

# label vars 
census_clean <- set_variable_labels(census_clean,
  S2301_C02_022E = "LFPR MALE 20-64",
  S2301_C02_023E = "LFPR FEMALE 20-64",
  S2301_C02_024E = "LFPR FEMALE 20-64 with children under 18 years",
  S2301_C02_025E = "LFPR FEMALE 20-64 with children under 18 years - under 6 years only",
  S2301_C02_026E = "LFPR FEMALE 20-64 with children under 18 years - under 6 and 6-17 years",
  S2301_C02_027E = "LFPR FEMALE 20-64 with children under 18 years - 6-17 years only",
  S2301_C03_023E = "Employment / Population ratio FEMALE 20-64",
  S2301_C03_024E = "Employment / Population ratio FEMALE 20-64 with children under 18",
  S2301_C03_025E = "Employment / Population ratio FEMALE 20-64 with children under 18 - under 6 years only",
  S2301_C03_026E = "Employment / Population ratio FEMALE 20-64 with children under 18 - under 6 and 6-17 years",
  S2301_C03_027E = "Employment / Population ratio FEMALE 20-64 with children under 18 - 6-17 years only",
  GEO_ID = "Geographic unit code provided b y the census, ending in FIPS code",
  county = "County (dropped county ending)",
  state = "State (full name)",
  year = "Year of census data collection",
)

rm(census_df, census_short, census_short_2, keep_var)

# CLEAN BEA DATA 
# drop observations that are states 
bea_GDP_df_2 <- bea_GDP_df %>%
  mutate(
    across(1:2, \(x) toupper(trimws(x)))
  ) %>%
  filter(
    !is.na(.[[1]]),
    .[[2]] != "UNITED STATES",
    .[[1]] != "",
    .[[2]] != ""
  )

pre_col_list_GDP_df <-colnames(bea_GDP_df_2)
new_colnames <- c("state", "county", "real_GDP_2017_dollars_2020", "real_GDP_2017_dollars_2021",
                  "real_GDP_2017_dollars_2022", "real_GDP_2017_dollars_2023",
                  "state_GDP_rank_2023", "YoY_change_percentage_GDP_2021", 
                  "YoY_change_percentage_GDP_2022",
                  "YoY_change_percentage_GDP_2023","change_percentage_change_rank_2023")

colnames(bea_GDP_df_2) <- new_colnames

# pivot long
bea_GDP_df_long <- bea_GDP_df_2 %>%
  pivot_longer(
    cols = -c("state", "county"),                                       # exclude id col(s)
    names_to = c(".value", "year"),                       # base name becomes a column
    names_pattern = "^(.*)_(20(?:20|21|22|23))$"          # split at _YYYY
  ) %>%
  mutate(year = as.integer(year))

# keep only 2023 data and drop data without observations
bea_GDP_df_long_2 <- bea_GDP_df_long %>% filter(year == 2023)
bea_GDP_clean <- bea_GDP_df_long_2[rowSums(is.na(bea_GDP_df_long_2)) < 6, ] #see how many were dropped 

rm(bea_GDP_df, bea_GDP_df_2, bea_GDP_df_long, bea_GDP_df_long_2, bea_GDP_df_raw) # clean up interim DFs 


## CLEANING - same thing for income data as for GDP data
dropped_rows <- bea_Y_df %>%
  filter(
    is.na(.[[1]]) |
      .[[2]] == "UNITED STATES" |
      .[[1]] == "" |
      .[[2]] == ""
  )

bea_Y_df_2 <- bea_Y_df %>%
  mutate(across(1:2, \(x) toupper(trimws(x)))) %>%
  filter(
    !is.na(.[[1]]),
    .[[2]] != "UNITED STATES",
    .[[1]] != "",
    .[[2]] != ""
  )

n_dropped <- nrow(dropped_rows)
print(n_dropped)


# fix colnames based on excel (original file)
pre_col_list_Y_df <-colnames(bea_Y_df_2)
new_colnames_Y <- c("state", "county", "per_capita_income_2021", "per_capita_income_2022", 
                  "per_capita_income_2023", "rank_income_state_2023", "percent_change_income_2022", 
                  "percent_change_income_2023", "rank_percent_change_in_state_2023")

colnames(bea_Y_df_2) <- new_colnames_Y


# pivot long
bea_Y_df_1_long <- bea_Y_df_2 %>%
  pivot_longer(
    cols = -c("state", "county"),                                       # exclude id col(s)
    names_to = c(".value", "year"),                       # base name becomes a column
    names_pattern = "^(.*)_(20(?:20|21|22|23))$"          # split at _YYYY
  ) %>%
  mutate(year = as.integer(year))

bea_Y_df_2_long <- bea_Y_df_1_long %>% filter(year == 2023)
bea_Y_clean <- bea_Y_df_2_long[rowSums(is.na(bea_Y_df_2_long)) < 6, ] #no drops 

rm(bea_Y_df, bea_Y_df_1_long, bea_Y_df, bea_Y_df_2, 
   bea_Y_df_2_long, bea_Y_df_raw, bea_Y_state, df) # clean up interim DFs 


############### Merge data together (3 data sets) ###################
merged_bea <- bea_Y_clean %>%
  full_join(bea_GDP_clean, by = c("county", "state"), suffix = c(".df1", ".df2"))

# test the warning message (think its year?)
look <- bea_GDP_clean %>% distinct(county, state, .keep_all = TRUE)

# FOUND DUPLICATES - ERROR IN THE MANUAL ASSIGNMENT OF STATE TO CITIES IN BEA DATA --- NEED TO REVIEW 
dup_keys <- bea_GDP_clean %>%
  count(county, state, sort = TRUE) %>%
  filter(n > 1)


merged <- bea_Y_clea %>%
  full_join(look, by = c("county","state"), suffix = c(".df1",".df2"),
            relationship = "many-to-one")


