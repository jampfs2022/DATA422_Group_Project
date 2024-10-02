#-------------------------------------------------------------------------------
# Load packages
#-------------------------------------------------------------------------------

library(tidyverse)
library(summarytools)

#-------------------------------------------------------------------------------
# Load data
#-------------------------------------------------------------------------------

merged_data <- readRDS("merged_data.rds")

head(merged_data)

# label missing values

merged_data <- merged_data %>%
  mutate(vodafone_missing_flag = ifelse(is.na(vodafone_devices), 1, 0)) %>%
  mutate(spark_missing_flag = ifelse(is.na(spark_devices), 1, 0))
dfSummary(merged_data)


#-------------------------------------------------------------------------------
# Check pattern of missing values
#-------------------------------------------------------------------------------
# Plot missing values by sa2_code and time_stamp

ggplot(merged_data, aes(x = sa2_code, y = time_stamp)) +
  geom_point(aes(color = is.na(vodafone_devices) | is.na(spark_devices)), alpha = 0.5) +
  labs(title = "Missing Vodafone and Spark Devices by SA2 and Time",
       x = "SA2 Code", y = "Time Stamp", color = "Missing") +
  theme_minimal()

# clearly grouped missing values. Entire sa2_code(s) are missing, and small sections of others.

#---------------------------------
# List SA2 codes that have missing values (NA)

v_missing <- merged_data %>%
  filter(is.na(vodafone_devices) | is.nan(vodafone_devices)) %>%
  group_by(sa2_code, sa2_name) %>%
  summarize(count_missing = n()) %>%
  arrange(desc(count_missing))
print(v_missing, n = 88)

s_missing <- merged_data %>%
  filter(is.na(spark_devices) | is.nan(spark_devices)) %>%
  group_by(sa2_code, sa2_name) %>%
  summarize(count_missing = n()) %>%
  arrange(desc(count_missing))
print(s_missing, n = 88)

identical(v_missing, s_missing)
# FALSE

all_missing <- v_missing %>%
  full_join(s_missing, by = c("sa2_name", "sa2_code")) %>%
  rename(v_missing = count_missing.x, s_missing = count_missing.y)

print(all_missing, n = 88)

# In total, only 88 sa2_codes have missing values
# There are 42 consecutive codes between 197700 and 201800 that are completely missing (i.e. 324 ts)
# 1 x sa2_code has 24 values missing at the end of the time range ( 251400 Wellington Central)
# 45 other codes have 2 missing values each, and appear to all be oceanic/island regions

#-------------------------------------------------------------------------------
# First have a look at the small gaps
#-------------------------------------------------------------------------------

small_gaps <- all_missing %>%
  filter(v_missing == 1) %>%
  select(sa2_code, sa2_name)
print(small_gaps, n = 45)


# get all data from these regions

small_gap_locs <- merged_data %>%
  filter(sa2_code %in% small_gaps$sa2_code)
dfSummary(small_gap_locs)


# select one area
first_sgl <- small_gap_locs %>%
  filter(sa2_code == first(sa2_code))
dfSummary(small_gap_locs)


# all zeros
# note the missing value is at a different time_stamp for v & s

# so, check how many of these areas are all zero. easy-peasy to impute if so!

sum_sgl <- small_gap_locs %>%
  group_by(sa2_code) %>%
  summarize(v_total = sum(vodafone_devices, na.rm = TRUE),
            s_total = sum(spark_devices, na.rm = TRUE))
print(sum_sgl, n = 45)

# now plot one

ggplot(first_sgl, aes(x = time_stamp, y = vodafone_devices)) +
  geom_line() +
  theme_minimal()

#-------------------------------------------------------------------------------
# Saved different groups of Missing Data
#-------------------------------------------------------------------------------

single_gaps_not_zero = c(258900, 343000)

# (1)
completely_missing <- all_missing %>%
  filter(v_missing == 324) %>%
  select(sa2_code, sa2_name)
print(completely_missing, n = 42)

# (2)
single_gaps <- all_missing %>%
  filter(sa2_code %in% single_gaps_not_zero) %>%
  select(sa2_code, sa2_name)
print(single_gaps)

#(3)
single_gaps_zeros <- all_missing %>%
  filter(sa2_code %in% small_gaps$sa2_code) %>%
  filter(!(sa2_code %in% single_gaps_not_zero)) %>%
  select(sa2_code, sa2_name)
print(single_gaps_zeros, n = 43)

#(4)
wgtn_gap <- all_missing %>%
  filter(sa2_code == 251400) %>%
  select(sa2_code, sa2_name)
print(wgtn_gap)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Deal with missing values for each of the four sets
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#(1) Completely missing for both vodafone and spark for entire time series.
#-------------------------------------------------------------------------------
#-- cannot impute. no data to impute from. remove from dataset

imputed_data_step1 <- merged_data %>%
  filter(!(sa2_code %in% completely_missing$sa2_code))
dfSummary(imputed_data_step1)

#-------------------------------------------------------------------------------
#(2) Single gaps, all other time stamps = 0, so assign to 0
#-------------------------------------------------------------------------------

imputed_data_step2 <- imputed_data_step1 %>%
  mutate(
    vodafone_devices = ifelse(sa2_code %in% single_gaps_zeros$sa2_code &
                                is.na(vodafone_devices), 0, vodafone_devices),
    spark_devices = ifelse(sa2_code %in% single_gaps_zeros$sa2_code &
                             is.na(spark_devices), 0, spark_devices)
  )
dfSummary(imputed_data_step2)

#-------------------------------------------------------------------------------
#(3) Single gaps, non_zero dataset. Impute using mean of data from hour either side.
#-------------------------------------------------------------------------------

# 258900
# 343000

# function to impute value from rows (i.e. hours) either side:

impute_by_neighbor_mean <- function(data, sa2_codes, column_name) {
  data %>%
    arrange(time_stamp) %>%  # Ensure data is ordered by time_stamp
    group_by(sa2_code) %>%
    mutate(
      # Check if the sa2_code is in the list and the value is missing
      !!sym(column_name) := ifelse(
        sa2_code %in% sa2_codes & is.na(!!sym(column_name)),
        # Calculate the mean of the previous and next row
        (lag(!!sym(column_name), default = NA) + lead(!!sym(column_name), default = NA)) / 2,
        !!sym(column_name)
      )
    ) %>%
    ungroup()
}

imputed_data_step3 <- impute_by_neighbor_mean(imputed_data_step2, c(258900, 343000), "vodafone_devices")
imputed_data_step3 <- impute_by_neighbor_mean(imputed_data_step3, c(258900, 343000), "spark_devices")

#------------------------------
BOP_island <- imputed_data_step3 %>%
  filter(sa2_code == 258900)
dfSummary(BOP_island)

ggplot(BOP_island, aes(x = time_stamp)) +
  geom_line(aes(y = vodafone_devices), color = "blue") +
  geom_line(aes(y = spark_devices), color = "black") +
  geom_point(aes(y = vodafone_devices), color = "blue") +
  geom_point(aes(y = spark_devices), color = "black") +
  geom_point(data = BOP_island %>% filter(vodafone_missing_flag == 1),
             aes(y = vodafone_devices), color = "red", shape = 8, size = 3) +
  geom_point(data = BOP_island %>% filter(spark_missing_flag == 1),
             aes(y = spark_devices), color = "red", shape = 8, size = 3) +
  theme_minimal()


#------------------------------
Chathams <- imputed_data_step3 %>%
  filter(sa2_code == 343000)
dfSummary(BOP_island)

ggplot(Chathams, aes(x = time_stamp)) +
  geom_line(aes(y = vodafone_devices), color = "blue") +
  geom_line(aes(y = spark_devices), color = "black") +
  geom_point(aes(y = vodafone_devices), color = "blue") +
  geom_point(aes(y = spark_devices), color = "black") +
  geom_point(data = Chathams %>% filter(vodafone_missing_flag == 1),
             aes(y = vodafone_devices), color = "red", shape = 8, size = 3) +
  geom_point(data = Chathams %>% filter(spark_missing_flag == 1),
             aes(y = spark_devices), color = "red", shape = 8, size = 3) +
  theme_minimal()

#-------------------------------------------------------------------------------
#(4) Wgtn gap:
#-------------------------------------------------------------------------------

# consecutive values missing at end of dataset of interest. Use pattern in surrounding
# suburbs and rest of time period to impute.

# imputed_data <- imputed_data_step3 %>% ...






#-------------------------------------------------------------------------------
# Simple imputation using mode (absolutely not the final choice, using just to carry on...)
#-------------------------------------------------------------------------------

library(modeest)
v_mode <- getmode(merged_data$vodafone_devices)
print(getmode)

# mode
merged_data_imputed <- merged_data %>%
  mutate(
    vodafone_devices = ifelse(is.na(vodafone_devices) | is.nan(vodafone_devices),
                              mlv(vodafone_devices, method = "mf", na.rm = TRUE),
                              vodafone_devices),
    spark_devices = ifelse(is.na(spark_devices) | is.nan(spark_devices),
                           mlv(spark_devices, method = "mf", na.rm = TRUE),
                           spark_devices)
  )

#
# # or mean
# merged_data_imputed <- merged_data %>%
#   mutate(
#     vodafone_devices = ifelse(is.na(vodafone_devices) | is.nan(vodafone_devices),
#                               mean(vodafone_devices),
#                               vodafone_devices),
#     spark_devices = ifelse(is.na(spark_devices) | is.nan(spark_devices),
#                            mean(spark_devices),
#                            spark_devices)
#   )

# Check the result
head(merged_data_imputed)
dfSummary(merged_data_imputed)

# save temporary result
saveRDS(merged_data_imputed, "mode_imputed_data.rds")







