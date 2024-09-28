# 02_load_and_clean_spark_HH.R
#-------------------------------------------------------------------------------
# Load libraries
#-------------------------------------------------------------------------------
library(tidyverse)
library(arrow)
library(ggplot2)
library(scales)

#-------------------------------------------------------------------------------
# sp_data 
#-------------------------------------------------------------------------------

# Load data
sp_data <- read_csv('sp_data.csv.gz')

# Step 1: Clean Data (excluding group-by initially)
clean_sp_data <- sp_data %>%
  mutate(
    ts_char = as.character(ts),  # Convert `ts` to character
    date = substr(ts_char, 1, 10),  # Extract the date part (YYYY-MM-DD)
    hour = substr(ts_char, 12, 13),  # Extract the hour part (HH)
    hour = ifelse(is.na(hour) | hour == "", "00", hour),  # Replace NA or empty hour with "00"
    cnt = round(cnt),
    sa2 = as.character(sa2)
  ) %>%
  select(sa2, date, hour, cnt, ts_char)  # Keep only relevant columns

# Step 2: Impute Missing Values in 'cnt' Using the Mean
mean_cnt <- mean(clean_sp_data$cnt, na.rm = TRUE)
clean_sp_data <- clean_sp_data %>%
  mutate(cnt = ifelse(is.na(cnt), mean_cnt, cnt))

# Step 3: Rename and Group
clean_sp_data <- clean_sp_data %>%
  rename(sp_devices = cnt, date_time = ts_char) %>%
  group_by(sa2, date, hour) %>%
  summarise(total_sp = sum(sp_devices, na.rm = TRUE), .groups = "drop")

# view structure 
head(clean_sp_data)
summary(clean_sp_data)

# Bar plot of the average `total_vf` by hour
ggplot(clean_sp_data, aes(x = hour, y = total_sp)) +
  stat_summary(fun = sum, geom = "bar", fill = "skyblue", color = "black") +
  labs(title = "Total SP Distribution by Hour",
       x = "Hour",
       y = "SP Devices Number") +
  scale_y_continuous(labels = comma) +
  theme_minimal()


#-------------------------------------------------------------------------------
# vf_data
#-------------------------------------------------------------------------------

#Load data
vf_data <- read_parquet("vf_data.parquet")

# Step 1: Clean Data 
clean_vf_data <- vf_data %>%
  mutate(
    dt_char = as.character(dt),  # Convert `dt` to character
    date = substr(dt_char, 1, 10),  # Extract the date part (YYYY-MM-DD)
    hour = substr(dt_char, 12, 13),  # Extract the hour part (HH)
    hour = ifelse(is.na(hour) | hour == "", "00", hour),  # Replace NA or empty hour with "00"
    devices = round(devices),
    sa2 = as.character(area)  # Rename `area` to `sa2`
  ) %>%
  select(sa2, date, hour, devices, dt_char)  # Keep only relevant columns

# Step 2: Impute Missing Values in 'cnt' Using the Mean
mean_devices <- mean(clean_vf_data$devices, na.rm = TRUE)
clean_vf_data <- clean_vf_data %>% 
  mutate(devices = ifelse(is.na(devices), mean_devices, devices))

# Step 3: Rename and Group
clean_vf_data <- clean_vf_data %>% 
  rename(vf_devices = devices, date_time = dt_char) %>% 
  group_by(sa2, date, hour) %>% 
  summarise(total_vf = sum(vf_devices, na.rm = TRUE), .groups = "drop")


# view structure 
head(clean_vf_data)
summary(clean_vf_data)


# Bar plot of the average `total_vf` by hour
ggplot(clean_vf_data, aes(x = hour, y = total_vf)) +
  stat_summary(fun = sum, geom = "bar", fill = "skyblue", color = "black") +
  labs(title = "Total VF Distribution by Hour",
       x = "Hour",
       y = "VF Devices Number") +
  scale_y_continuous(labels = comma) +
  theme_minimal()



#-------------------------------------------------------------------------------
# join vf and sp data 
#-------------------------------------------------------------------------------

# join sp_data and vf_data
joined_cellphone <- full_join(clean_vf_data, clean_sp_data, by = c("sa2", "date", "hour")) %>% 
  mutate(
    total_vf = ifelse(is.na(total_vf), 0, total_vf),
    total_sp = ifelse(is.na(total_sp), 0, total_sp)
  )

# View structure of joined data
head(joined_cellphone)
summary(joined_cellphone)









