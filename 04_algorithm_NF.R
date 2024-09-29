library(tidyverse)
library(summarytools)

#-------------------------------------------------------------------------------
# load data
#-------------------------------------------------------------------------------

df_loaded <- readRDS("df_final.rds")
str(df_loaded)

#-------------------------------------------------------------------------------
# extract counts from midnight - 1am
#-------------------------------------------------------------------------------
# need to justify choice of time to use - plot to see patterns
# looking for the night time drop off, but not too late in night that inactive cell phones
# are dropped from tower

df_midnight <- df_loaded %>%
  filter(hour(time_stamp) == 0) %>%
  mutate(total_devices = spark_devices + vodafone_devices) %>%
  mutate(adult_count = total_count - child_count) %>%
  mutate(population_weight = (1.2 * (adult_count / (adult_count + child_count)) +
                                0.1 * (child_count / (adult_count + child_count)))) %>%
  mutate(estimated_population = total_devices / population_weight) %>%
  select(total_devices, adult_count, child_count, total_count, estimated_population)

head(df_midnight)

#------------------------------------------------------------------------------


