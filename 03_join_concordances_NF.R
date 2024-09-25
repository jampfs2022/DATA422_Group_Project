
source("00_load_clean_concordances_NF.R")


concordances <- sa2_to_ta %>%
  inner_join(sa2_to_ur %>% select(sa2_code, ur_code, ur_descriptor, Mapping), by = "sa2_code") %>%
  inner_join(ur_to_uri  %>% select(ur_code, uri_code, uri_descriptor), by = "ur_code")

summary(concordances)


# check for duplicate rows

duplicates <- concordances %>%
  group_by(across(everything())) %>%  # Group by all columns
  filter(n() > 1) %>%                 # Keep only rows that appear more than once
  ungroup()                           # Ungroup after filtering

# none, all good