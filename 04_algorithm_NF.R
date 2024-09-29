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

#------------------------------------------------------------------------------

  midnight_population <- df_midnight %>%
    mutate(other_devices = (spark_devices + vodafone_devices) / 2) %>%
    mutate(adult_count = (total_count - child_count)) %>%
    mutate(pop_estimate = (spark_devices + vodafone_devices + third_provider) *
              (1.2 * adult_count + 0.1 * child_count))

