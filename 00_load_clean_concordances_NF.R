
library(tidyverse)
getwd()
list.files("C:/rfiles/DATA422/Project")

#-------------------------------------------------------------------------------
# LOAD IN FILES
#-------------------------------------------------------------------------------
# Classification Report: Statistical Area 2 2023
sa2 <- read_csv("C:/rfiles/DATA422/Project/sa2_2023.csv",
                skip = 7,
                col_names = c("sa2_code", "sa2_descriptor")
                )
head(sa2)

#-------------------------------------------------------------------------------
# Concordance Report: Statistical Area 2 2023 to Territorial Authority 2023

sa2_to_ta <- read_csv("C:/rfiles/DATA422/Project/sa2_ta_concord_2023.csv",
                      skip = 7,
                      col_names = c("sa2_code", "sa2_descriptor", "Mapping", "ta_code","ta_descriptor"))
head(sa2_to_ta)

#-------------------------------------------------------------------------------
# Concordance report: Statistical Area 2 2023 to Urban Rural 2023

sa2_to_ur <- read_csv("C:/rfiles/DATA422/Project/urban_rural_to_sa2_concord_2023.csv",
                      skip = 7,
                      col_names = c("sa2_code", "sa2_descriptor", "Mapping", "ur_code","ur_descriptor"))
head(sa2_to_ur)

#-------------------------------------------------------------------------------
# Concordance Report: Urban Rural 2023 to Urban Rural Indicator

ur_to_uri <- read_csv("C:/rfiles/DATA422/Project/urban_rural_to_indicator_2023.csv",
                      skip = 7,
                      col_names = c("ur_code", "ur_descriptor", "Mapping", "uri_code","uri_descriptor"))
head(ur_to_uri)



#-------------------------------------------------------------------------------
# CLEAN DATA
#-------------------------------------------------------------------------------

# SA2

# check for NA
sa2 %>% filter(is.na(sa2_code))
sa2 %>% filter(is.na(sa2_descriptor))

# check for duplicates
summary(sa2)
sa2 %>% count(sa2_code) # 2395 unique values, so no duplicates
sa2 %>% count(sa2_descriptor) # 2395 unique values, so no duplicates

# convert code from character to integer
sa2$sa2_code <- as.numeric(sa2$sa2_code)
head(sa2)

summary(sa2)

sa2[1,]
sa2[2395,]

#-------------------------------------------------------------------------------
# sa2_ta_concord_2023
# 2395

summary(sa2_to_ta)

str(sa2_to_ta)

# check for NA
sa2_to_ta %>% filter(is.na(sa2_code))
sa2_to_ta %>% filter(is.na(sa2_descriptor))
sa2_to_ta %>% filter(is.na(Mapping))
sa2_to_ta %>% filter(is.na(ta_code))
sa2_to_ta %>% filter(is.na(ta_descriptor))

# check for duplicates
sa2_to_ta %>% count(sa2_code) # 2395 unique values, so no duplicates
sa2_to_ta %>% count(sa2_descriptor) # 2395 unique values
sa2_to_ta %>% count(Mapping) # only 1 unique calue: column all the same
sa2_to_ta %>% count(ta_code) # 68 unique values
sa2_to_ta %>% count(ta_descriptor) #68 unique values

# remove redundant column
sa2_to_ta <- sa2_to_ta %>% select(-Mapping)
colnames(sa2_to_ta)

#-------------------------------------------------------------------------------
# sa2_to_ur
# 2815

summary(sa2_to_ur)
str(sa2_to_ur)

# check for NA
sa2_to_ur %>% filter(is.na(sa2_code))
sa2_to_ur %>% filter(is.na(sa2_descriptor))
sa2_to_ur %>% filter(is.na(Mapping))
sa2_to_ur %>% filter(is.na(ur_code))
sa2_to_ur %>% filter(is.na(ur_descriptor))

# check for duplicates
sa2_to_ur %>% count(sa2_code) # 2395 unique values, so there are 420 duplicates
sa2_to_ur %>% count(sa2_descriptor) # 2395 unique values, so 420 duplicates
sa2_to_ur %>% count(Mapping) # 4 values
sa2_to_ur %>% count(ur_code) # 745 unique values
sa2_to_ur %>% count(ur_descriptor) # 745 unique values

# find the duplicates

sa2_to_ur %>%
  group_by(across(everything())) %>%
  filter(n() >1) %>%
  ungroup()
# no complete rows are duplicates

sa2_to_ur %>%
  group_by(sa2_code) %>%
  filter(n() > 1) %>%
  distinct(sa2_code)
# but 270 sa2_code values are duplicated

duplicates <- sa2_to_ur %>%
  group_by(sa2_code) %>%
  filter(n() > 1) %>%
  ungroup()
# with  690, so 270 values have 420 additional duplicates.
# (270 + 420 = 690)
print(duplicates)




#-------------------------------------------------------------------------------
# ur_to_uri
# 745

summary(ur_to_uri)
str(ur_to_uri)

# check for NA
ur_to_uri %>% filter(is.na(ur_code))
ur_to_uri %>% filter(is.na(ur_descriptor))
ur_to_uri %>% filter(is.na(Mapping))
ur_to_uri %>% filter(is.na(uri_code))
ur_to_uri %>% filter(is.na(uri_descriptor))

# check for duplicates
ur_to_uri %>% count(ur_code) # 745 unique values, so no duplicates
ur_to_uri %>% count(ur_descriptor) # 745 unique values, so no duplicates
ur_to_uri %>% count(Mapping) # 1 values
ur_to_uri %>% count(uri_code) # 9 unique values
ur_to_uri %>% count(uri_descriptor) # 9 unique values

# remove redundant column
ur_to_uri <- ur_to_uri %>% select(-Mapping)
colnames(ur_to_uri)


#-------------------------------------------------------------------------------
# Compare columns in different dataframes
#-------------------------------------------------------------------------------

# sa2 & sa2_to_ta:

# Compare columns pairwise and create a column for each comparison
comparison_df <- sa2 %>%
  mutate(same_col1 = sa2$sa2_code == sa2_to_ta$sa2_code,
         same_col2 = sa2$sa2_descriptor == sa2_to_ta$sa2_descriptor)

# View rows where there is a mismatch
comparison_df %>% filter(!same_col1 | !same_col2)


