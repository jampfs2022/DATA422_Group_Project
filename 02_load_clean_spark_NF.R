#-------------------------------------------------------------------------------
# load packages
#-------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(plotly)
library(summarytools)

#-------------------------------------------------------------------------------
# load data
#-------------------------------------------------------------------------------

spark <-  read_csv("sp_data.csv.gz")
str(data)
head(as.data.frame(spark), 20)
# [811,776 x 3]

spark_tidy1 <- spark %>%
  mutate(ts = ymd_hms(ts))
  rename(time_stamp = "ts", sa2_code = "sa2", spark_devices = "cnt") %>%
  mutate(spark_devices = round(spark_devices))

head(spark_tidy1)

#-------------------------------------------------------------------------------
# Use to view a slice of the code
#-------------------------------------------------------------------------------

rows_to_display <- spark_tidy1 %>%
  slice(811500:811776)
print(rows_to_display, n = 200)

#-------------------------------------------------------------------------------
# Explore duplicates
#-------------------------------------------------------------------------------

dfSummary(spark_tidy1)
# dim: 811,776 x 3
# duplicates: 6021

# There are 336 hours in time period, and 2395 sa2 regions.
# therefore should only be 804,720 rows:

distinct_combinations <- spark_tidy1 %>%
  distinct(sa2_code, time_stamp) %>%
  nrow()
print(distinct_combinations)
# 804720


# ------- look by timestamp---------
# how many sa2 rows per timestamp?
sa2s_per_time <- spark_tidy1 %>%
  group_by(time_stamp) %>%
  summarise(count = n(), .groups = "drop")
print(sa2s_per_time, n = 340)
# and:
distinct_counts <- sa2s_per_time %>%
  distinct(count)
print(distinct_counts)

# i.e. there are 2416 rows for each timestamp, when there should only be 2395.
# This is 21 rows extra

# ------- look by sa2_code ---------

times_per_sa2 <- spark_tidy1 %>%
  group_by(sa2_code) %>%
  summarise(count = n(), .groups = "drop")
print(times_per_sa2, n = 10)
# and:
distinct_counts <- times_per_sa2 %>%
  distinct(count)
print(distinct_counts)

# aha! at least one has 2688 instead of 336
# which sa2_codes?

# Find the sa2_code values with 2688 rows
sa2_code_with_2688 <- times_per_sa2 %>%
  filter(count == 2688)
print(sa2_code_with_2688)


# sa2_code count
# <dbl> <int>
# 1   126801  2688
# 2   179501  2688
# 3   341801  2688

# save values to investigate further

sa2_codes_to_check <- c(126801, 179501, 341801)

# look at raw data in case decimal gives clues
spark_duplicates <- spark %>%
  arrange(sa2, ts) %>%
  filter(sa2 %in% sa2_codes_to_check)

# So for each timestamp & sa2_code there are 8 values:
# (mostly) 4 identical values, and 4 identical smaller values

# look at counts.

distinct_counts <- spark_duplicates %>%
  group_by(cnt) %>%            # Group by the distinct values in the cnt column
  summarise(count = n())    # Count the number of occurrences of each distinct value

# Display the result
print(distinct_counts)

#-------------------------------------------------------------------------------
# Remove duplicates - Method 1: take maximum value only
# - note can't use filter(value == max(value)), as this would return all values
# if multiple (which there are here)

# this takes a while to run:
spark_tidy_max <- spark_tidy1 %>%
  group_by(sa2_code, time_stamp) %>%
  slice_max(order_by = spark_devices, n = 1, with_ties = FALSE) %>%
  ungroup()
print(nrow(spark_tidy_max))

dfSummary(spark_tidy_max)
# dim: 804720 x 3
# duplicates: 0
# vars;
# - time_stamp: 336 distinct values over 13 days 23 H (2 June - 16 June 2024)
# - sa2_code: 2395 distinct values as expected
# - spark devices: min < med < max: 0 < 944 < 13057 ; 14157 (1.8% missing).

#-------------------------------------------------------------------------------
# Plot histogram to check distribution
#-------------------------------------------------------------------------------

ggplot(spark_tidy_max, aes(x = spark_devices)) +
  geom_histogram(binwidth = 10, fill = "blue", colour = "darkred") +
  labs(title = "Histogram of Spark Devices",
       x = "Number of Spark Devices",
       y = "Frequency"
       ) +
  theme_minimal()


# show counts

# Counting distinct values in the spark_devices column
distinct_counts <- spark_tidy_max %>%
  count(spark_devices, sort = TRUE)
print(distinct_counts, n = 200)
# 0:   37160
# NA:  14157

# look at the end of the tail - do we think they are real or not?

spark_tail <- spark_tidy_max %>%
  arrange(desc(spark_devices)) %>%
  slice_head(n = 20)
print(spark_tail, n = 20)

# join with sa2_code:
source("00_load_clean_supplementary_data.R")

spark_tail_2 <- spark_tail %>%
  left_join(population_by_area %>% select(sa2_code, sa2_name, ta_name, ur_name, uri_name),
            by = "sa2_code")
print(spark_tail_2, n = 20)

# yes, i think they could be genuine - i.e. holiday makers? is this school holidays or some other event?
# markets, fairs, etc?


