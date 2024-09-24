
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
  rename(age = "AGE_POPES_SUB_006", area = "AREA_POPES_SUB_006", count = "OBS_VALUE")
head(pop_clean)


# only two age values in file, and both have total at bottom. Need to remove those lines

# Remove total lines:

rows_to_remove <- pop_clean %>%
  filter(area == "NZTA")
print(rows_to_remove)

# AGE0014      NZTA   968300
# TOTALALLAGES NZTA  5223100

pop_clean <- pop_clean %>%
  filter(!(area == "NZTA"))
summary(pop_clean)


# check that totals correspond

age_summary <- pop_clean %>%
  group_by(age) %>%
  summarise(total_count = sum(count))

# Print the summary
print(age_summary)

# AGE0014          2269030
# TOTALALLAGES    12185400
