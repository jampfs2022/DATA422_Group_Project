
library(tidyverse)

#-------------------------------------------------------------------------------
# Sub-national Population Estimates

pop <- read_csv("C:/rfiles/DATA422/Project/subnational_pop_ests.csv") %>%
  rename(year = `Year at 30 June`)
head(pop)
