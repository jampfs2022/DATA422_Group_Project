
source("00_load_clean_concordances_NF.R")
source("00_load_clean_population_NF.R")

# not shouldn't use inner join. change to left or outer and check lengths.
concordances <- sa2_to_ta %>%
  full_join(sa2_to_ur %>% select(sa2_code, ur_code, ur_descriptor, Mapping), by = "sa2_code") %>%
  full_join(ur_to_uri  %>% select(ur_code, uri_code, uri_descriptor), by = "ur_code")

summary(concordances)


# check for duplicate rows

duplicates <- concordances %>%
  group_by(across(everything())) %>%  # Group by all columns
  filter(n() > 1) %>%                 # Keep only rows that appear more than once
  ungroup()                           # Un-group after filtering
print(duplicates)
# none, all good

# check for NA values





# convert area from numeric to character, to match population

concordances <- concordances %>%
  mutate(sa2_code = as.character(sa2_code))
head(concordances)

#-------------------------------------------------------------------------------
# now join POPULATION (sa2)

population_by_areas <- concordances %>%
  left_join(pop_clean, by = "sa2_code")
head(population_by_areas)
str(population_by_areas)

# Find Auckland CBD

# uri codes
distinct_uri_values <- population_by_areas %>%
  select(uri_descriptor, uri_code) %>%    # Select the relevant columns
  distinct()                              # Get distinct combinations
print(distinct_uri_values)


auckland_areas <- population_by_areas %>%
  filter(ur_descriptor == "Auckland" & uri_code %in% c(14, 12, 11, 13))
head(auckland_areas)
