library(tidyr)
library(base)
library(readxl)
library(dplyr)
library(labelled)


getwd()
# load census data 
census_df = read.csv("/Users/allegrasaggese/Library/CloudStorage/Dropbox/Tradwives/data/Census-ACSST5Y2023.S2301-Data.csv")

# load BEA data 
bea_Y_df = read_excel("/Users/allegrasaggese/Library/CloudStorage/Dropbox/Tradwives/data/BEA-county-per-capita-income.xlsx", 
                      sheet = 1,
                      skip = 3)

bea_GDP_df = read_excel("/Users/allegrasaggese/Library/CloudStorage/Dropbox/Tradwives/data/BEA-county-percapita-GDP.xlsx", 
                      sheet = 1,
                      skip = 3)

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
census_clean <- set_var_labels(census_clean,
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
))


# calculate LFPR per county for women 

