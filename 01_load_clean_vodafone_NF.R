#-------------------------------------------------------------------------------
# load packages
#-------------------------------------------------------------------------------

library(tidyverse)
library(arrow)
library(lubridate)
library(plotly)
library(summarytools)
#-------------------------------------------------------------------------------
# load data
#-------------------------------------------------------------------------------

vodafone <- read_parquet("C:/rfiles/DATA422/Project/vf_data.parquet")
head(vodafone)

dfSummary(vodafone)
# Dimensions: 811752 x 3
# Duplicates: 6048
# Variables:
# -- dt: 336 distinct values, as expected, no missing
# -- area: 2395 distinct values as expected.
# ---- 3 areas have duplicates (2688), all others have 336 rows as expected
# -----> 126801, 179501, 341801  - same 3 as in spark
# -- devices: min < med < max: 0 < 404.5 < 5595.9, missing: 14157  (1.7%)

vodafone_tidy <- vodafone  %>%
  rename(time_stamp = "dt", sa2_code = "area", vodafone_devices = "devices") %>%
  mutate(time_stamp = ymd_hms(time_stamp)) %>%
  mutate(sa2_code = as.numeric(sa2_code))
head(vodafone_tidy)
# time_stamp, sa2_code, vodafone_devices


#-------------------------------------------------------------------------------
# Remove duplicates
#-------------------------------------------------------------------------------

# should be this number of rows:
distinct_combinations <- vodafone_tidy %>%
  distinct(sa2_code, time_stamp) %>%
  nrow()
print(distinct_combinations)
# 804696
# this means we are missing 24 combinations.


# takes a while to run:
vodafone_tidy_max <- vodafone_tidy %>%
  group_by(sa2_code, time_stamp) %>%
  slice_max(order_by = vodafone_devices, n = 1, with_ties = FALSE) %>%
  ungroup()
print(nrow(vodafone_tidy_max))
# 804696 as expected

#-------------------------------------------------------------------------------
# Plot histogram to check distribution
#-------------------------------------------------------------------------------

ggplot(vodafone_tidy_max , aes(x = vodafone_devices)) +
  geom_histogram(binwidth = 10, fill = "blue", colour = "darkred") +
  labs(title = "Histogram of Spark Devices",
       x = "Number of Spark Devices",
       y = "Frequency"
  ) +
  theme_minimal()

# very similar to spark

# show counts

# Counting distinct values in the spark_devices column
distinct_counts <- vodafone_tidy_max %>%
  count(vodafone_devices, sort = TRUE)
print(distinct_counts)

# 0:   37145
# NA:  14112
# NaN: 45

# so need to impute NA and NaN
