# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/lake/datacomm.rds") %>% 
  filter(UL_NO_CODE != "")

# Create clean data -------------------------------------------------------
Clean <- data.frame(
  UL_NO_CODE = Data$UL_NO_CODE
)



