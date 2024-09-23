
library(tidyverse)
getwd()
list.files("C:/rfiles/DATA422/Project")

#-------------------------------------------------------------------------------
## LOAD IN FILES
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
# Concordance Report: Urban Rural 2023 to Urban Rural Indicator

ur_to_uri <- read_csv("C:/rfiles/DATA422/Project/urban_rural_to_indicator_2023.csv",
                      skip = 7,
                      col_names = c("ur_code", "ur_descriptor", "Mapping", "uri_code","uri_descriptor"))
head(ur_to_uri)

#-------------------------------------------------------------------------------
# Concordance report: Statistical Area 2 2023 to Urban Rural 2023

sa2_to_ur <- read_csv("C:/rfiles/DATA422/Project/urban_rural_to_sa2_concord_2023.csv",
                      skip = 7,
                      col_names = c("sa2_code", "sa2_descriptor", "Mapping", "ur_code","ur_descriptor"))
head(sa2_to_ur)

#-------------------------------------------------------------------------------
## CHECK DATA
#-------------------------------------------------------------------------------

summary(sa2)
sa2 %>% count(sa2_code) # 2396 unique values, so no duplicates

na_rows <- sa2 %>%
  filter(is.na(sa2_code))
na_rows

sa2$sa2_code <- as.integer(sa2$sa2_code)
head(sa2)
