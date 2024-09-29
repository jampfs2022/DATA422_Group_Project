library(tidyverse)
library(summarytools)

#-------------------------------------------------------------------------------
# load data
#-------------------------------------------------------------------------------

df_loaded <- readRDS("df_final.rds")

#-------------------------------------------------------------------------------
# extract counts from midnight - 1am
#-------------------------------------------------------------------------------
# need to justify choice of time to use - plot to see patterns
# looking for the night time drop off, but not too late in night that inactive cell phones
# are dropped from tower

df_midnight <- df_loaded %>%
  filter(hour(time_stamp) == 0)


#-------------------------------------------------------------------------------
# Convert counts to population using algorithm
#-------------------------------------------------------------------------------
source("04_algorithm_NF.R")
midnight_population <- convert_to_population(df_midnight)

#-------------------------------------------------------------------------------
# Compare population estimates to actual population counts
#-------------------------------------------------------------------------------

# select columns of interest
head(midnight_population)
str(midnight_population)

compare_pops <- midnight_population %>%
  select(total_count, pop_estimate, vodafone_devices, spark_devices, third_provider, adult_count, child_count)

head(compare)
#-------------------------------------------------------------------------------
# save final datafile as agzipped csv
#-------------------------------------------------------------------------------

# required columns:

# Territorial Authority Code
# sa2_code
# datetime in NZST
# count of people


