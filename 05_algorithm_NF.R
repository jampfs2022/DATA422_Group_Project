library(tidyverse)
library(summarytools)

#-------------------------------------------------------------------------------
# load data
#-------------------------------------------------------------------------------

df <- readRDS("mean_imputed_data.rds")
str(df)


#-------------------------------------------------------------------------------
# select timestamp to use as overnight e.g. resident population
#-------------------------------------------------------------------------------

df_tidy <- df %>%
  select(time_stamp, vodafone_devices, spark_devices, sa2_code, child_count, total_count) %>%
  mutate(total_devices = spark_devices + vodafone_devices,
         adult_count = total_count - child_count)
str(df_tidy)


#-------------------------------------------------------------------------------
# Determine mean child to adult ratio
#-------------------------------------------------------------------------------

mean_ratio <- df_tidy %>%
  filter(adult_count > 0) %>%
  mutate(child_to_adult_ratio = child_count / adult_count) %>%
  summarise(mean_child_to_adult_ratio = mean(child_to_adult_ratio, na.rm = TRUE))
print(mean_ratio)
# 0.222

------------------------------------------------------------------------------
  # plot histogram of number of devices for each hour

  df_hourly_summary <- df_tidy %>%
  mutate(hour = hour(time_stamp)) %>%
  group_by(hour) %>%
  summarize(total_devices_sum = sum(total_devices, na.rm = TRUE))
print(df_hourly_summary, n = 24)

ggplot(df_hourly_summary, aes(x = hour, y = total_devices_sum)) +
  geom_col(fill = "skyblue", colour = "black") +  # Use geom_col() for pre-summarized data
  labs(title = "Total Devices by Hour",
       x = "Hour of the Day",
       y = "Total Devices") +
  theme_minimal()

# but this doesn't actually tell us muc, as it includes all regions, residential and CBDs.
# Instead, look at a known residential suburb
# e.g. 161100 Wattle Downs. Residential area only, 2 primary schools

wd_hourly <- df_tidy %>%
  filter(sa2_code == 161100) %>%
  mutate(hour = hour(time_stamp)) %>%
  group_by(hour) %>%
  summarize(total_devices_sum = sum(total_devices, na.rm = TRUE))
print(wd_hourly, n = 24)

ggplot(wd_hourly, aes(x = hour, y = total_devices_sum)) +
  geom_col(fill = "skyblue", colour = "black") +  # Use geom_col() for pre-summarized data
  labs(title = "Total Devices by Hour",
       x = "Hour of the Day",
       y = "Total Devices") +
  theme_minimal()

# can see drop-off during day as everyone leaves for work.school, increases again in the evening
# peak is at 5am, so take that value?

#-------------------------------------------------------------------------------
# extract counts at midnight
#-------------------------------------------------------------------------------

df_midnight <- df_tidy %>%
  select(time_stamp, total_devices, sa2_code,
         child_count, adult_count, total_count) %>%
  filter(hour(time_stamp) == 0) %>%
  select(-time_stamp)

head(df_midnight)

# plot this to see variability
ggplot(df_midnight, aes(x = "", y = total_devices)) +
  geom_boxplot(fill = "lightblue", color = "darkred") +
  labs(title = "Variation of Total Devices at 12 AM",
       x = "12 AM",
       y = "Total Devices") +
  theme_minimal()

#-------------------------------------------------------------------------------
# 5am device count will vary every day, but only 1 fixed resident population (for such
# a short time period), so take the mean per sa2_code
#-------------------------------------------------------------------------------


df_residential <- df_midnight %>%
  group_by(sa2_code) %>%
  summarize(
    adult_count = first(adult_count),                  # Take the first entry for adult_count
    child_count = first(child_count),                  # Take the first entry for child_count
    total_count = first(total_count),                  # Take the first entry for total_count
    mean_total_devices = mean(total_devices, na.rm = TRUE) # Calculate mean for vodafone_devices
  )
head(df_residential)

#-------------------------------------------------------------------------------
# First try: just punch in some numbers
#-------------------------------------------------------------------------------

first_estimate <- df_residential  %>%
  mutate(estimated_devices = (1.0 * (adult_count) + 0.1 * (child_count))) %>%
  select(mean_total_devices, estimated_devices, adult_count, child_count, total_count)

head(first_estimate)

#------------------------------------------------------------------------------
# Now want to fit a model to find alpha and beta to minimise the difference between
# the estimated population and the actual population.

# Fit the linear model (want a weight, so don't include intercept)
model <- lm(mean_total_devices ~ 0 + adult_count + child_count, data = df_residential)

# Summary of the model to see the coefficients
summary(model)

coef(model)
# adult_count child_count
# 0.84056847 -0.07901675

# Display the coefficients
cat("Beta for adult_count:", coef(model)[1], "\n")
cat("Beta for child_count:", coef(model)[2], "\n")

# predictions <- predict(model, newdata = df_residential)

#-------------------------------------------------------------------------------
# but don't want -ve values for the coefficients

# install.packages("nnls")

library(nnls)

# Prepare the independent variables (as a matrix) and the dependent variable
X <- as.matrix(df_residential[, c("adult_count", "child_count")])
y <- df_residential$mean_total_devices

# Fit the model with non-negative constraints
nnls_model <- nnls(X, y)

# Get the coefficients
coefs = coef(nnls_model)

# Display the coefficients
cat("Beta for adult_count:", coefs[1], "\n")
cat("Beta for child_count:", coefs[2], "\n")


#-------------------------------------------------------------------------------
# predictions:

predictions <- coefs[1] * df_residential$adult_count +
  coefs[2] * df_residential$child_count

# Add predictions to your dataframe
df_residential <- df_residential %>%
  mutate(predicted_devices = predictions)

head(df_residential)

#-------------------------------------------------------------------------------
# plot residuals

df_residential <- df_residential %>%
  mutate(difference = mean_total_devices - predicted_devices)

# Step 2: Plot the difference
ggplot(df_residential, aes(x = sa2_code, y = difference)) +
  geom_bar(stat = "identity", fill = "blue", colour = "black") +
  labs(title = "Difference Between Mean Total Devices and Predicted Devices",
       x = "SA2 Code",
       y = "Difference (mean_total_devices - predicted_devices)") +
  theme_minimal()

#-------------------------------------------------------------------------------

# so relationship is: Devices = 0.8 * Adults, and kids are 0.222 x Adults
# so weighting is: Total Population / Devices
# w = (A + k) / (0.823A + 0k)
# w = (A + 0.222A) / 0.823A
# w = 1.222 / 0.823
# w = 1.485

w = (1 + mean_ratio) / coefs[1]
w = w[["mean_child_to_adult_ratio"]]
print(w)
# 1.486091

#-------------------------------------------------------------------------------
# Have a look at this:

df_residential <- df_residential %>%
  mutate(predicted_count = mean_total_devices * w)

head(df_residential)


#-------------------------------------------------------------------------------
# Compare to simple weight taking from total population count and total device count at midnight
#-------------------------------------------------------------------------------

# population
census_pop <- df_tidy %>%
  filter(time_stamp == first(time_stamp)) %>%
  summarize(NZ_people_count = sum(total_count))
print(census_pop)
# 5222750

# total_devices in NZat midnight

device_total <- df_tidy %>%
  filter(time_stamp == nth(time_stamp,1)) %>%
  summarize(tot = sum(total_devices, na.rm = TRUE))
print(device_total)
# 3,528,664

# so a simple conversion would just be:
w2 = census_pop[[1]] / device_total[[1]]
print(w2)

# 1.48
# i.e. very similar, for a lot less work!




