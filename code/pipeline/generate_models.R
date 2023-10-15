# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/marts/data_for_models.rds")


# 1. keep it simple -------------------------------------------------------

#### Remove data points when respondent has already given

