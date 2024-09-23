
library(tidyverse)
getwd()
list.files("C:/rfiles/DATA422/Project")


#-------------------------------------------------------------------------------
# Classification Report: Statistical Area 2 2023
sa2 <- read_csv("C:/rfiles/DATA422/Project/sa2_2023.csv",
                skip = 6)
head(sa2)

#-------------------------------------------------------------------------------
# Concordance Report: Statistical Area 2 2023 to Territorial Authority 2023

sa2_tO_ta <- read_csv("C:/rfiles/DATA422/Project/sa2_ta_concord_2023.csv",
                      skip = 7,
                      col_names = c("sa2_code", "sa2_descriptor", "Mapping", "ta_code","ta_descriptor"))
head(sa2_tO_ta)

#-------------------------------------------------------------------------------
# Sub-national Population Estimates

pop <- read_csv("C:/rfiles/DATA422/Project/subnational_pop_ests.csv") %>%
  rename(year = `Year at 30 June`)
head(pop)


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