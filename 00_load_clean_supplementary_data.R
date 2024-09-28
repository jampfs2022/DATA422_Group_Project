#-------------------------------------------------------------------------------
# Author: Naomi Fleming
# Date: 28 Sep 2023
#-------------------------------------------------------------------------------
# INSTALL & LOAD PACKAGES
#-------------------------------------------------------------------------------

# Install packages if required
#------------------------------

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

if (!requireNamespace("summarytools", quietly = TRUE)) {
  install.packages("tidyverse")
}

# Load packages
library(tidyverse)
library(summarytools)

#-------------------------------------------------------------------------------
# LOAD IN DATA
#-------------------------------------------------------------------------------

# Classification Report: Statistical Area 2 2023
sa2 <- read_csv("sa2_2023.csv",
                skip = 7,
                col_names = c("sa2_code", "sa2_name", "unknown")
)
head(sa2)

#------------------------------
# Concordance Report: Statistical Area 2 2023 to Territorial Authority 2023

sa2_to_ta <- read_csv("sa2_ta_concord_2023.csv",
                      skip = 7,
                      col_names = c("sa2_code", "sa2_name", "Mapping", "ta_code","ta_name"))
head(sa2_to_ta)

#------------------------------
# Concordance report: Statistical Area 2 2023 to Urban Rural 2023

sa2_to_ur <- read_csv("urban_rural_to_sa2_concord_2023.csv",
                      skip = 7,
                      col_names = c("sa2_code", "sa2_name", "Mapping", "ur_code","ur_name"))
head(sa2_to_ur)

#------------------------------
# Concordance Report: Urban Rural 2023 to Urban Rural Indicator

ur_to_uri <- read_csv("urban_rural_to_indicator_2023.csv",
                      skip = 7,
                      col_names = c("ur_code", "ur_name", "Mapping", "uri_code","uri_name"))
head(ur_to_uri)

#------------------------------
# Sub-national Population Estimates

subnational_population <- read_csv("subnational_pop_ests.csv")
head(subnational_population)

#-------------------------------------------------------------------------------
# CHECK DATA
#-------------------------------------------------------------------------------

dfSummary(sa2)
# dimensions: 2395 x 2
# no duplicates
# sa2_code, sa2_name
# 2395 distinct values for both columns (which matches NZStats into for SA2 regions 2023)
# No missing values

dfSummary(sa2_to_ta)
# dimensions: 2395 x 5
# no duplicates
# sa2_code, sa2_name, Mapping, ta_code, ta_name,
# 2395 distinct values for sa2_code & sa2_name
# Mapping is a constant column
# ta_code & ta_name each have 68 distinct values (which matches number of TAs in NZ)
# No missing values

dfSummary(sa2_to_ur)
# dimensions: 2395 x 5
# no duplicates
# sa2_code, sa2_name, Mapping, ur_code, ur_name
# 2395 distinct values for sa2_code & sa2_name
# 4 distinct values in Mapping
# 745 distinct values for ur_code and ur_name which matches number of ur's in NZ
# No missing values


dfSummary(ur_to_uri)
# dimensions: 745 x 5
# no duplicates
# ur_code, ur_name, Mapping, uri_code, uri_name
# 745 distinct values for ur_code and ur_name
# Mapping is a constant column "1. Many to One Map"
# 9 distinct values for uri_code and uri_name
# No missing values

dfSummary(subnational_population)
#  10 constant columns:
# STRUCTURE = DATAFLOW
# STRUCTURE_ID = STATSNZ:POPES_SUB_006(1.0)
# STRUCTURE_NAME = Subnational population estimates (TA, SA2), by age and sex, at 30 June 1996-2023 (2023 boundaries)
# ACTION = I
# YEAR_POPES_SUB_006 = 2023
# `Year at 30 June` = 2023
# SEX_POPES_SUB_006 = SEX3
# Sex = Total people, sex
# Area = (all NA's)
# Observation vallue = (all NA's)

# 2 distinct values for AGE_POPES_SUB_006: AGE0014 & TOTALALLAGES
# 2 distinct values for Age: 0-14 Years & Total people, age
# 2485 distinct values for AREA_POPES_SUB_006 (each with 2 entries?)
# OBS_VALUE min < med < max: 0 < 770 < 5223100

#-------------------------------------------------------------------------------
# TIDY DATA
#-------------------------------------------------------------------------------

# Convert sa2_code to character to match population (and allow join)
sa2_tidy <- sa2 %>%
  mutate(sa2_code = as.numeric(sa2_code)) %>%
  select(-unknown)

#------------------------------
# Remove constant columns
sa2_to_ta_tidy <- sa2_to_ta %>%
  select(-Mapping)

ur_to_uri_tidy <- ur_to_uri %>%
  select(-Mapping)

population_tidy <- subnational_population %>%
  select(where(~ n_distinct(.) > 1))
head(population_tidy)

#------------------------------
# Rename remaining Mapping column
sa2_to_ur_tidy <- sa2_to_ur %>%
  rename(sa2_to_ur_mapping = Mapping)

#------------------------------
# Tidy population data further

# - remove redundant Age column
# - Rename columns to shorter variants
# - remove the two total rows
# - reshape into tidy format
# - rename new columns to more readable names

population_tidier <- population_tidy %>%
  select(-Age) %>%
  rename(age = "AGE_POPES_SUB_006", sa2_code = "AREA_POPES_SUB_006", count = "OBS_VALUE") %>%
  filter(!(sa2_code == "NZTA")) %>%
  pivot_wider(names_from = age, values_from = count) %>%
  rename(child_count = "AGE0014", total_count = "TOTALALLAGES")

head(population_tidier)

dfSummary(population_tidier)
# Dimensions: 2484 x 3
# no duplicates
# sa2_code, child_count, total_count
# 2484 distinct values for sa2_code
# no missing values

#-------------------------------------------------------------------------------
# JOIN DATA
#-------------------------------------------------------------------------------

# First join the concordances
concordances <- sa2_to_ta_tidy %>%
  full_join(sa2_to_ur_tidy %>% select(sa2_code, ur_code, ur_name, sa2_to_ur_mapping), by = "sa2_code") %>%
  full_join(ur_to_uri_tidy  %>% select(ur_code, uri_code, uri_name), by = "ur_code")

head(concordances)

# note: didn't actually need sa2.csv

# Now join the population, noting that sa2_code is numeric in the concordances, but character in the population df

population_by_area <- concordances %>%
  left_join(population_tidier %>% mutate(sa2_code = as.numeric(sa2_code)), by = "sa2_code")

head(population_by_area)

dfSummary(population_by_area)
# 2815 x 11 (note need to deal with duplicates sa2_area, where one area has two (or more? ur_codes))
# as sa2_code is not unique at the moment.
# Vars:
# "sa2_code"          "sa2_name"          "ta_code"           "ta_name"           "ur_code"           "ur_name"
# "sa2_to_ur_mapping" "uri_code"          "uri_name"          "child_count"       "total_count"
