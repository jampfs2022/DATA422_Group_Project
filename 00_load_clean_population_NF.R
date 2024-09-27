
library(tidyverse)

#-------------------------------------------------------------------------------
# Sub-national Population Estimates
#-------------------------------------------------------------------------------

pop <- read_csv("C:/rfiles/DATA422/Project/subnational_pop_ests.csv") %>%
  rename(year = `Year at 30 June`)
head(pop)

#-------------------------------------------------------------------------------
# Remove single-valued columns

constant_columns <- pop %>%
  map_lgl(~ n_distinct(.) == 1) %>%
  enframe(name = "column", value = "is_constant") %>%
  filter(is_constant)

print(constant_columns)

# Identify and remove constant columns
pop_clean <- pop %>%
  select(where(~ n_distinct(.) > 1))
head(pop_clean)



#-------------------------------------------------------------------------------
# check for NA values

na_columns <- pop_clean %>%
  map_lgl(~ any(is.na(.))) %>%
  enframe(name = "column", value = "has_na") %>%
  filter(has_na)

print(na_columns)

#-------------------------------------------------------------------------------
# What age values are there?

distinct_entries <- pop %>%
  distinct(AGE_POPES_SUB_006)
print(distinct_entries)

# AGE0014
# TOTALALLAGES
# and they always correspond, so can remove one.

pop_clean <- pop_clean %>%
  select(-Age)
head(pop_clean)

# Now rename columns

pop_clean <- pop_clean %>%
  rename(age = "AGE_POPES_SUB_006", sa2_code = "AREA_POPES_SUB_006", count = "OBS_VALUE")
head(pop_clean)


# only two age values in file, and both have total at bottom. Need to remove those lines

# Remove total lines:

rows_to_remove <- pop_clean %>%
  filter(sa2_code == "NZTA")
print(rows_to_remove)

# AGE0014      NZTA   968300
# TOTALALLAGES NZTA  5223100
# ?? so need to subtract total to get adults???

pop_clean <- pop_clean %>%
  filter(!(sa2_code == "NZTA"))
summary(pop_clean)


# check that totals correspond - no they don't so not totals? check data source for more info.

age_summary <- pop_clean %>%
  group_by(age) %>%
  summarise(total_count = sum(count))

# Print the summary
print(age_summary)

# AGE0014          2269030
# TOTALALLAGES    12185400

# Reshape into TIDY format:

#-------------------------------------------------------------------------------
# Reshaping the dataframe - two different fields in the "age" column
pop_clean <- pop_clean %>%
  pivot_wider(names_from = age, values_from = count)
head(pop_clean)

