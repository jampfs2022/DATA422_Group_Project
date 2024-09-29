#-------------------------------------------------------------------------------
# load supplementary data (population and areas)
#-------------------------------------------------------------------------------

source("00_load_clean_supplementary_data.R") # concordances [2815 x 9]


#-------------------------------------------------------------------------------
# load cell phone count data
#-------------------------------------------------------------------------------

source("01_load_clean_vodafone_NF.R") # vodafone_tidy_max [804696 x 3]
# 2024-06-03 to 2024-06-16 23:00:00 , range = 13d 23H 0M 0S

source("02_load_clean_spark_NF.R") # spark_tidy_max [804720 x 3]
# 2024-06-02 12:00:00 to 2024-06-16 11:00:00, range : 13d 23H 0M 0S

#-------------------------------------------------------------------------------
# Join all data
#-------------------------------------------------------------------------------

# join cellphone data
df <- vodafone_tidy_max %>%
  full_join(spark_tidy_max,  by = c("sa2_code", "time_stamp"))
# 833460 x 4 due to different time ranges

head(df)
dfSummary(df)

# select only the overlapping time range
df_intersection <- df %>%
  filter(time_stamp %in% intersect(vodafone_tidy_max$time_stamp, spark_tidy_max$time_stamp))
dfSummary(df_intersection)
# Dimensions: 775980 x 4
# time_stamp; 2024-06-03 to 2024-06-16 11:00:00 , range : 13d 11H 0M 0S, 324 distinct
# sa2_code

# join with population and area data
all_data <- df %>%
  left_join(population_by_area, by = "sa2_code")
head(all_data)

# reorder columns
df_final <- all_data %>%
  select(time_stamp, vodafone_devices, spark_devices, everything())
head(df_final)

# check final dataset
dfSummary(all_data)
# Dimensions: 833460 x 14
# Duplicates: 0

#-------------------------------------------------------------------------------
# still has NZ and NaNs, need to go back and impute missing data.

#-------------------------------------------------------------------------------
# save so don't need to re-run to generate everytime we want to use it

saveRDS(df_final, "df_merged.rds")

# check:
df_loaded <- readRDS("df_merged.rds")
