library(tidyr)
library(base)
library(readxl)
library(dplyr)
library(labelled)
library(purrr)
library(knitr)


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


str(census_clean)
census_clean <- census_clean %>%
  mutate(across(starts_with("S2301"), ~ as.numeric(as.character(.))))

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
   bea_Y_df_2_long, bea_Y_df_raw) # clean up interim DFs 


############### Merge data together (3 data sets) ###################
merged_bea <- bea_Y_clean %>%
  full_join(bea_GDP_clean, by = c("county", "state"), suffix = c(".df1", ".df2"))

# test the warning message (think its year?)
look <- bea_GDP_clean %>% distinct(county, state, .keep_all = TRUE)

# FOUND DUPLICATES - ERROR IN THE MANUAL ASSIGNMENT OF STATE TO CITIES IN BEA DATA --- NEED TO REVIEW 
dup_keys <- bea_GDP_clean %>%
  count(county, state, sort = TRUE) %>%
  filter(n > 1)
# AFTER QA --- NO DUPES! 

colnames(merged_bea)
# drop year indicator (all the same year)
merged_bea <- merged_bea %>% select(-year.df1, -year.df2)

df_full <- census_clean %>% full_join(merged_bea, by = c("county", "state"))



############ REGRESSION ANALYSIS ####################

model_v1 <- lm(S2301_C02_023E ~ log(per_capita_income), data = df_full)
summary(model_v1)


model_v2 <- lm(S2301_C02_023E ~ log(per_capita_income) + I(log(per_capita_income)^2),
               data = df_full)
summary(model_v2)


# plot the basic u-curve at the county level 
plot1 <- plot(log(df_full$per_capita_income), df_full$S2301_C02_023E,
     xlab = "log(per capita income)", ylab = "Labor Force Participation Rate")
curve(coef(model_v2)[1] + coef(model_v2)[2]*x + coef(model_v2)[3]*x^2,
      add = TRUE, col = "red", lwd = 2)

# now interrogate the data (summary stats, data type)
stats_table <- df_full %>%
  summarise(
    Mean_LFPR = mean(S2301_C02_023E, na.rm = TRUE),
    Median_LFPR = median(S2301_C02_023E, na.rm = TRUE),
    Mode_LFPR = as.numeric(names(sort(table(S2301_C02_023E), decreasing = TRUE)[1])),
    Range_LFPR = diff(range(S2301_C02_023E, na.rm = TRUE)),
    Mean_Income = mean(per_capita_income, na.rm = TRUE),
    Median_Income = median(per_capita_income, na.rm = TRUE),
    Mode_Income = as.numeric(names(sort(table(per_capita_income), decreasing = TRUE)[1])),
    Range_Income = diff(range(per_capita_income, na.rm = TRUE))
  )

kable(stats_table, digits = 2, caption = "Summary Statistics")
 
# trimming extrema 
df_full_v2 <- df_full %>%
  filter(S2301_C02_023E <= quantile(S2301_C02_023E, 0.99, na.rm = TRUE),
         per_capita_income <= quantile(per_capita_income, 0.99, na.rm = TRUE))

# rerun the log and log^2 specification of the u-curve reg
model_v3 <- lm(S2301_C02_023E ~ log(per_capita_income) + I(log(per_capita_income)^2),
               data = df_full_v2)
summary(model_v3)

plot2 <- plot(log(df_full_v2$per_capita_income), df_full_v2$S2301_C02_023E,
     xlab = "log(per capita income)", ylab = "Labor Force Participation Rate")
curve(coef(model_v3)[1] + coef(model_v3)[2]*x + coef(model_v3)[3]*x^2,
      add = TRUE, col = "pink", lwd = 2)

#further trim 
df_full_v3 <- df_full %>%
  filter(S2301_C02_023E <= quantile(S2301_C02_023E, 0.995, na.rm = TRUE),
         per_capita_income <= quantile(per_capita_income, 0.995, na.rm = TRUE))

model_v4 <- lm(S2301_C02_023E ~ log(per_capita_income) + I(log(per_capita_income)^2),
               data = df_full_v3)
summary(model_v4)

plot3 <- plot(log(df_full_v3$per_capita_income), df_full_v3$S2301_C02_023E,
              xlab = "log(per capita income)", ylab = "Labor Force Participation Rate")
curve(coef(model_v4)[1] + coef(model_v4)[2]*x + coef(model_v4)[3]*x^2,
      add = TRUE, col = "orange", lwd = 2)


# ROBUSTNESS CHECK - check against male LFPR 
model_v4_MALE <- lm(S2301_C02_022E ~ log(per_capita_income) + I(log(per_capita_income)^2),
               data = df_full_v3)
summary(model_v4_MALE)

plot3b <- plot(log(df_full_v3$per_capita_income), df_full_v3$S2301_C02_022E,
              xlab = "log(per capita income)", ylab = "Labor Force Participation Rate (male)")
curve(coef(model_v4_MALE)[1] + coef(model_v4_MALE)[2]*x + coef(model_v4_MALE)[3]*x^2,
      add = TRUE, col = "blue", lwd = 2)
# IT DOES HOLD FOR MALE - NOT GREAT 

# check female - male 
model_v4_FVM <- lm(I(S2301_C02_022E-S2301_C02_023E) ~ log(per_capita_income) + I(log(per_capita_income)^2),
                    data = df_full_v3)
summary(model_v4_FVM)

plot3c <- plot(log(df_full_v3$per_capita_income), (df_full_v3$S2301_C02_022E - df_full_v3$S2301_C02_023E),
              xlab = "log(per capita income)", ylab = "Labor Force Participation Rate (difference)")
curve(coef(model_v4_FVM)[1] + coef(model_v4_FVM)[2]*x + coef(model_v4_FVM)[3]*x^2,
      add = TRUE, col = "green", lwd = 2)
# but against the difference, we see something - even though it is flat 

# CALIFORNIA ONLY 
df_CA <- subset(df_full_v3, state=="CALIFORNIA")

# male v female in CA 
model_v4_FVM_CA <- lm(I(S2301_C02_022E-S2301_C02_023E) ~ log(per_capita_income) + I(log(per_capita_income)^2),
                   data = df_CA)
summary(model_v4_FVM_CA)

plotca1 <- plot(log(df_CA$per_capita_income), (df_CA$S2301_C02_022E - df_CA$S2301_C02_023E),
               xlab = "log(per capita income)", ylab = "Labor Force Participation Rate (difference)")
curve(coef(model_v4_FVM_CA)[1] + coef(model_v4_FVM_CA)[2]*x + coef(model_v4_FVM_CA)[3]*x^2,
      add = TRUE, col = "blue", lwd = 2)

# female only in CA 
model_v3_CA <- lm(S2301_C02_023E ~ log(per_capita_income) + I(log(per_capita_income)^2),
                      data = df_CA)
summary(model_v3_CA)

plotca2 <- plot(log(df_CA$per_capita_income), (df_CA$S2301_C02_023E),
               xlab = "log(per capita income)", ylab = "Labor Force Participation Rate (female)")
curve(coef(model_v3_CA)[1] + coef(model_v3_CA)[2]*x + coef(model_v3_CA)[3]*x^2,
      add = TRUE, col = "blue", lwd = 2)

# male only in CA 
model_male_CA <- lm(S2301_C02_022E ~ log(per_capita_income) + I(log(per_capita_income)^2),
                  data = df_CA)
summary(model_male_CA)

plotca2 <- plot(log(df_CA$per_capita_income), (df_CA$S2301_C02_022E),
                xlab = "log(per capita income)", ylab = "Labor Force Participation Rate (male)")
curve(coef(model_male_CA)[1] + coef(model_male_CA)[2]*x + coef(model_male_CA)[3]*x^2,
      add = TRUE, col = "blue", lwd = 2)



# STATE LOOP 
states <- unique(df_full_v3$state)

for(s in states) {
  df_state <- subset(df_full_v3, state == s)
  
  model <- lm(I(S2301_C02_022E - S2301_C02_023E) ~ log(per_capita_income) + I(log(per_capita_income)^2),
              data = df_state)
  
  print(paste("State:", s))
  print(summary(model))
  
  plot(log(df_state$per_capita_income),
       (df_state$S2301_C02_022E - df_state$S2301_C02_023E),
       xlab = "log(per capita income)",
       ylab = "Labor Force Participation Rate (difference)",
       main = paste("State:", s))
  
  curve(coef(model)[1] + coef(model)[2]*x + coef(model)[3]*x^2,
        add = TRUE, col = "red", lwd = 2)
}


###### ADDING DEMOGRAPHIC DATA 
census_demo_data = read.csv("/Users/allegrasaggese/Library/CloudStorage/Dropbox/Tradwives/data/Census-Demographics-bycounty.csv")

census_demo_collapse <- census_demo_data %>%
  mutate(AGEGRP = ifelse(AGEGRP == 0, 0, 1)) %>%
  group_by(CTYNAME, STNAME, AGEGRP) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")

collapsed_2 <- census_demo_collapse  %>%
  distinct(CTYNAME, STNAME, .keep_all = TRUE)

collapsed_2 <- collapsed_2 %>%
  rename(county = CTYNAME,
         state = STNAME)

collapsed_2 <- collapsed_2 %>%
  mutate(
    county = iconv(county, from = "", to = "UTF-8"),
    state = iconv(state, from = "", to = "UTF-8"),
    county = toupper(gsub(" County", "", trimws(county), ignore.case = TRUE)),
    state = toupper(trimws(state))
  )

# merge on county / state 
df_full_demo <- df_full_v3 %>% full_join(collapsed_2, by = c("county", "state"))

##### WE HAVE POP BY DEMO (not LFPR)
