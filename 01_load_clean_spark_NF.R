#-------------------------------------------------------------------------------
# load packages
#-------------------------------------------------------------------------------

library(tidyverse)

#-------------------------------------------------------------------------------
# load data
#-------------------------------------------------------------------------------

spark <-  read_csv("C:/rfiles/DATA422/Project/sp_data.csv.gz")
head(as.data.frame(spark), 20)

spark_clean <- spark %>%
  rename(timestamp = "ts", sa2_code = "sa2", spark_pings = "cnt")
head(spark_clean)

# [811,776 x 3]

#-------------------------------------------------------------------------------
# Check for duplicates
#-------------------------------------------------------------------------------

# Identify and view duplicate rows

duplicate_count <- spark_clean %>%
  filter(duplicated(.)) %>%
  nrow() # 6021 duplicate rows


# Remove duplicates

spark_clean <- spark_clean %>%
  distinct()

str(spark_clean)

# [805,755 × 3]

#-------------------------------------------------------------------------------
# check for NA
#-------------------------------------------------------------------------------

na_columns <- spark_clean %>%
  map_lgl(~ any(is.na(.))) %>%
  enframe(name = "column", value = "has_na") %>%
  filter(has_na)

print(na_columns)

# so yes, there are NA values. How many?

na_count <- spark_clean %>%
  summarise(
    na_count = sum(is.na(spark_pings)),
    total_count = n(),
    na_percentage = (na_count / total_count) * 100
    )
print(na_count)

# 14157 NA values, so will need a missing value strategy.
# This is 1.76% of the data


#-------------------------------------------------------------------------------
# Plot histogram to check distribution
#-------------------------------------------------------------------------------


