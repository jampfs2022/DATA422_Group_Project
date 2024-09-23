# Load libraries

library(tidyverse)
library(nanoparquet)

# Load Spark Data

sp_data <- read.csv("E:/UC/DATA422/Assignments/Group Project/Group_Project_Github/sp_data.csv.gz")

# Load Vodafone Data

vf_data <-read_parquet("E:/UC/DATA422/Assignments/Group Project/Group_Project_Github/vf_data.parquet")

# Load SA2 Code and Names file
sa2_2023 <- read.csv("E:/UC/DATA422/Assignments/Group Project/Group_Project_Github/sa2_2023.csv", skip = 6)

# Load SA2 Concordance to TA supporting dataset

sa2_to_ta_col_names <- c("sa22023","sa22023_name", "relationship", "ta2023", "ta2023_name", "unknown")

sa2_to_ta <- read.csv("E:/UC/DATA422/Assignments/Group Project/Group_Project_Github/sa2_ta_concord_2023.csv", 
                      skip = 7 ,
                      col.names = sa2_to_ta_col_names) %>%
  select(-unknown)

# Load Population supporting dataset
pop_est <- read.csv("E:/UC/DATA422/Assignments/Group Project/Group_Project_Github/subnational_pop_ests.csv")

# Load Urban/Rural to Indicator supporting dataset

urban_to_ind_col_names <- c("ur2023", "ur2023_name", "relationship", "ur_ind", "ur_type", "unknown")

urban_to_ind <- pop_est <- read.csv("E:/UC/DATA422/Assignments/Group Project/Group_Project_Github/urban_rural_to_indicator_2023.csv",
                                    skip = 7,
                                    col.names = urban_to_ind_col_names)%>%
  select(-unknown)

# Load Urban/Rural to SA2 Concordance supporting dataset

urban_to_sa2_col_names <- c("sa22023", "sa22023_name", "relationship", "ur2023", "ur2023_name", "unknown")

urban_to_sa2 <-  pop_est <- read.csv("E:/UC/DATA422/Assignments/Group Project/Group_Project_Github/urban_rural_to_sa2_concord_2023.csv",
                                     skip = 7,
                                     col.names = urban_to_sa2_col_names) %>%
  select(-unknown)


# View the datasets
sp_data
vf_data

# View supporting datasets
sa2_2023
sa2_to_ta
pop_est
urban_to_ind
urban_to_sa2
  