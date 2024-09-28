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

rows_to_display <- spark_tidy2 %>%
  slice(811500:811776)
print(rows_to_display, n = 200)

#-------------------------------------------------------------------------------
# Explore duplicates
#-------------------------------------------------------------------------------

dfSummary(spark_tidy1)
# dim: 811,776 x 3
# duplicates: 6021

# There are 336 hours in time period, and 2395 sa2 regions.
# therefore should only be 804,720 rows

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
#   1   126801  2688
# 2   179501  2688
# 3   341801  2688





#-------------------------------
# plot to view any pattern

p <- ggplot(spark_tidy1, aes(x = sa2_code, time_stamp))
interactive_plot <- ggplotly(p)
interactive_plot

#-------------------------------
# look at a specific set of 21 duplicates

specific_timestamp <- ymd_hms("2024-06-02 13:00:00")

# Filter the dataframe for that timestamp and view the duplicates
duplicates <- spark_tidy1 %>%
  filter(time_stamp == specific_timestamp)  %>%
  group_by(sa2_code) %>%
  filter(n() > 1)  # Keep only duplicates
print(duplicates, n = 30)

#-------------------------------------------------------------------------------
# Remove duplicates?

spark_tidy2 <- spark_tidy1 %>%
  distinct()

dfSummary(spark_tidy2)
# dim: 805755 x 3
# duplicates: 0
# vars;
# - time_stamp: 336 distinct values over 13 days (2 June - 16 June 2024)
# - sa2_code: 2395 distinct values
# - spark devices: min < med < max: 0 < 943 < 13057; 14157 (1.8% missing).

#-------------------------------------------------------------------------------
# check for time_stamp and sa2_code duplicates
#-------------------------------------------------------------------------------

duplicate_count <- spark_tidy2 %>%
  group_by(time_stamp, sa2_code) %>%
  filter(n() > 1) %>%
  nrow()
print(duplicate_count)
# 2043 rows have n > 1 duplicates in time_stamp and sa2_code, but not in devices.

total_duplicate_rows_value <- spark_tidy %>%
  group_by(time_stamp, sa2_code) %>%
  filter(n() > 1) %>%
  tally() %>%
  summarise(total = sum(n))

total_duplicate_rows_value <- total_duplicate_rows_value$total


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


