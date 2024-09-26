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

# Table 1: Count of NA values
missing_count <- vodafone_clean %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# Table 2: Percentage of NA values
missing_percentage <- vodafone_clean %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100))

# Print the two tables
print(missing_count)
print(missing_percentage)

# 14157 NA values, so will need a missing value strategy.
# This is 1.76% of the data


#-------------------------------------------------------------------------------
# Statistics
#-------------------------------------------------------------------------------

summary(vodafone_clean)

#-------------------------------------------------------------------------------
# Plot histogram to check distribution
#-------------------------------------------------------------------------------

ggplot(vodafone_clean, aes(x = vodafone_devices)) +
  geom_histogram(binwidth = 10, fill = "blue", colour = "darkred") +
  labs(title = "Histogram of Spark Devices",
       x = "Number of Spark Devices",
       y = "Frequency"
  ) +
  theme_minimal()


# show counts

# Counting distinct values in the spark_devices column
distinct_counts <- vodafone_clean %>%
  count(vodafone_devices, sort = TRUE)
print(distinct_counts)

# 0:   37148
# NA:  14112
# NaN: 45
# remove these values and plot again.

vodafone_cut <- vodafone_clean %>%
  filter(!is.nan(vodafone_devices) & vodafone_devices != 0 & !is.na(vodafone_devices))


# Counting distinct values in the spark_devices column
distinct_counts <- vodafone_cut %>%
  count(vodafone_devices, sort = TRUE)
print(distinct_counts)

ggplot(vodafone_cut, aes(x = vodafone_devices)) +
  geom_histogram(binwidth = 10, fill = "magenta", colour = "black") +
  labs(title = "Histogram of Spark Devices",
       x = "Number of Spark Devices",
       y = "Frequency"
  ) +
  theme_minimal()
