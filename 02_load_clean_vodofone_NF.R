#-------------------------------------------------------------------------------
# load packages
#-------------------------------------------------------------------------------

library(tidyverse)
library(arrow)

#-------------------------------------------------------------------------------
# load data
#-------------------------------------------------------------------------------


vodafone <- read_parquet("C:/rfiles/DATA422/Project/vf_data.parquet")
head(vodafone)

str(vodafone)
# [811,752 × 3]

vodafone_clean <- vodafone  %>%
  rename(time_stamp = "dt", sa2_code = "area", vodafone_devices = "devices")
head(vodafone_clean)
str(vodafone_clean)

#-------------------------------------------------------------------------------
# Check for duplicates
#-------------------------------------------------------------------------------

# Identify and view duplicate rows

duplicate_count <- vodafone_clean %>%
  filter(duplicated(.)) %>%
  nrow()
print(duplicate_count)
#6048 duplicate rows


# Remove duplicates

vodafone_clean <- vodafone_clean %>%
  distinct()

str(vodafone_clean)
# [805,704 × 3]


#-------------------------------------------------------------------------------
# check for NA
#-------------------------------------------------------------------------------

na_columns <- vodafone_clean %>%
  map_lgl(~ any(is.na(.))) %>%
  enframe(name = "column", value = "has_na") %>%
  filter(has_na)
print(na_columns)

# so yes, there are NA values. How many?

na_count <- vodafone_clean %>%
  summarise(
    na_count = sum(is.na(vodafone_devices)),
    total_count = n(),
    na_percentage = (na_count / total_count) * 100
  )
print(na_count)

# 14157 NA values, so will need a missing value strategy.
# This is 1.76% of the data


#-------------------------------------------------------------------------------
# Plot histogram to check distribution
#-------------------------------------------------------------------------------
