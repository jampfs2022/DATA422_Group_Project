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

spark <-  read_csv("C:/rfiles/DATA422/Project/sp_data.csv.gz")
head(as.data.frame(spark), 20)
# [811,776 x 3]

spark_tidy1 <- spark %>%
  mutate(ts = ymd_hms(ts)) %>%
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

#-------------------------------------------------------------------------------
#------------------ re-written up to here. Below needs updating ----------------
#-------------------------------------------------------------------------------






dfSummary(spark_tidy_max)
# dim: 805755 x 3
# duplicates: 0
# vars;
# - time_stamp: 336 distinct values over 13 days (2 June - 16 June 2024)
# - sa2_code: 2395 distinct values
# - spark devices: min < med < max: 0 < 943 < 13057; 14157 (1.8% missing).

#-------------------------------------------------------------------------------
# Plot histogram to check distribution
#-------------------------------------------------------------------------------

ggplot(spark_clean, aes(x = spark_devices)) +
  geom_histogram(binwidth = 10, fill = "blue", colour = "darkred") +
  labs(title = "Histogram of Spark Devices",
       x = "Number of Spark Devices",
       y = "Frequency"
       ) +
  theme_minimal()


# show counts

# Counting distinct values in the spark_devices column
distinct_counts <- spark_clean %>%
  count(spark_devices, sort = TRUE)
print(distinct_counts)

# 0:   37148
# NA:  14157
# remove these values and plot again.

spark_cut <-  spark_clean %>%
  filter(spark_devices != 0 & !is.na(spark_devices))

# Counting distinct values in the spark_devices column
distinct_counts <- spark_cut %>%
  count(spark_devices, sort = TRUE)
print(distinct_counts)

# why are there decimal values? I thought this was a straight count of devices per hour?

# A tibble: 753,829 × 2
# spark_devices     n
# <dbl> <int>
#   1          10.5    28
# 2          21      28
# 3         218.     28
# 4         436.     28
# 5          11.5    14
# 6          13.0    14
# 7          13.7    14
# 8          15.4    14
# 9          16.9    14
# 10          17.2    14

ggplot(spark_cut, aes(x = spark_devices)) +
  geom_histogram(binwidth = 1, fill = "blue", colour = "darkred") +
  labs(title = "Histogram of Spark Devices",
       x = "Number of Spark Devices",
       y = "Frequency"
  ) +
  theme_minimal()

# check the largest values:
top_10_spark_devices <- spark_cut %>%
  arrange(desc(spark_devices)) %>%
  select(spark_devices) %>%
  head(30)
print(top_10_spark_devices, n = 30)

# so not a lone outlier.


