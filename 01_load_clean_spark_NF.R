#-------------------------------------------------------------------------------
# load packages
#-------------------------------------------------------------------------------

library(tidyverse)

#-------------------------------------------------------------------------------
# load data
#-------------------------------------------------------------------------------

spark <-  read_csv("C:/rfiles/DATA422/Project/sp_data.csv.gz")
head(as.data.frame(spark), 20)
# [811,776 x 3]

spark_clean <- spark %>%
  rename(time_stamp = "ts", sa2_code = "sa2", spark_devices = "cnt")
head(spark_clean)
str(spark_clean)


#-------------------------------------------------------------------------------
# Check for duplicates
#-------------------------------------------------------------------------------

# Identify and view duplicate rows

duplicate_count <- spark_clean %>%
  filter(duplicated(.)) %>%
  nrow()
print(duplicate_count)
# 6021 duplicate rows


# Remove duplicates

spark_clean <- spark_clean %>%
  distinct()

str(spark_clean)
# [805,755 × 3]

#-------------------------------------------------------------------------------
# check for NA
#-------------------------------------------------------------------------------

# Table 1: Count of NA values
missing_count <- spark_clean %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# Table 2: Percentage of NA values
missing_percentage <- spark_clean %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100))

# Print the two tables
print(missing_count)
print(missing_percentage)

# 14157 NA values in "spark_devices" column, so will need a missing value strategy.
# This is 1.76% of the data

#-------------------------------------------------------------------------------
# Statistics
#-------------------------------------------------------------------------------

summary(spark_clean)

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


