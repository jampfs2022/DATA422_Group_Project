
library(tidyverse)
library(arrow)

#-------------------------------------------------------------------------------
# load

vd_data <- read_parquet("C:/rfiles/DATA422/Project/vf_data.parquet")
head(vd_data)
