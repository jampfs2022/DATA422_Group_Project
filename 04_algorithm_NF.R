library(tidyverse)
library(summarytools)

#-------------------------------------------------------------------------------
# load data
#-------------------------------------------------------------------------------

df_loaded <- readRDS("df_final.rds")
str(df_loaded)

#-------------------------------------------------------------------------------
# extract counts from midnight
#-------------------------------------------------------------------------------
# need to justify choice of time to use - plot to see patterns
# looking for the night time drop off, but not too late in night that inactive cell phones
# are dropped from tower

df_midnight <- df_loaded %>%
  filter(hour(time_stamp) == 0) %>%
  mutate(total_devices = spark_devices + vodafone_devices) %>%
  mutate(adult_count = total_count - child_count) %>%
  mutate(population_weight = (1 * (adult_count / (adult_count + child_count)) +
                                0.1 * (child_count / (adult_count + child_count)))) %>%
  mutate(estimated_population = total_devices / population_weight) %>%
  select(total_devices, adult_count, child_count, total_count, estimated_population, ta_code)

head(df_midnight)

#------------------------------------------------------------------------------
# Now want to fit a model to find alpha and beta to minimise the difference between
# the estimated population and the actual population.

df_for_model <- df_midnight %>%
  mutate(adult_count = total_count - child_count) %>%
  mutate(adult_devices = (total_devices) * (adult_count / (adult_count + child_count)),
         child_devices = (total_devices) * (child_count / (adult_count + child_count)))

head(df_for_model)

# Step 2: Fit the linear model
model <- lm(total_count ~ adult_devices + child_devices, data = df_for_model)

# Step 3: Extract the coefficients (alpha and beta)
alpha <- coef(model)["adult_devices"]
beta <- coef(model)["child_devices"]

# Display the results
cat("Estimated alpha (Adults' device ownership rate):", alpha, "\n")
cat("Estimated beta (Children's device ownership rate):", beta, "\n")


# df_estimates <- df_for_model %>%
#   mutate(estimated_population = (total_devices) /
#            (alpha * (adult_count / (adult_count + child_count)) +
#               beta * (child_count / (adult_count + child_count)))) %>%
#  select(total_devices, adult_count, child_count, total_count, estimated_population, ta_code)
#
# head(df_estimates)

#-------------------------------------------------------------------------------
# make predictions

# Step 1: Make predictions using the linear model
df_predictions <- df_for_model %>%
  mutate(predicted_population = predict(model, newdata = df_for_model))

# Step 2: Calculate residuals (differences between actual and predicted population)
df_predictions  <- df_predictions  %>%
  mutate(residuals = total_count - predicted_population)

# Step 3: Calculate RMSE
rmse <- sqrt(mean(df_predictions $residuals^2))
print(rmse)
# need to impute NA values first

# -----------------------------------------------------------------------------
# maybe overfitting, need to relax?

# temporararily, remove missing values (go back and impute later on)
df_filtered <- df_for_model %>%
  filter(!is.na(total_count) & !is.na(adult_devices) & !is.na(child_devices))

library(glmnet)
x <- model.matrix(total_count ~ adult_devices + child_devices, df_filtered)
y <- df_filtered$total_count
model <- cv.glmnet(x, y, alpha = 1)


# Extract coefficients for the min lambda
coefficients_min <- coef(model, s = "lambda.min")

# Extracting specific coefficients
alpha <- coefficients_min["adult_devices", 1]  # Replace with the correct index if needed
beta <- coefficients_min["child_devices", 1]    # Replace with the correct index if needed

# Print the coefficients
cat("Alpha (adult_devices):", alpha, "\n")
cat("Beta (child_devices):", beta, "\n")

# ----------------------------------------------------------------------------
# save data.

head(df_filtered)

saveRDS(df_filtered, "df_final.rds")
