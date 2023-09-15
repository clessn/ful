# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/lake/datacomm.rds") %>% 
  filter(UL_NO_CODE != "")

# Create clean data -------------------------------------------------------
Clean <- data.frame(
  UL_NO_CODE = Data$UL_NO_CODE
)

# Méthode -----------------------------------------------------------------
table(Data$Méthode)
Clean$Methode <- NA
Clean$Methode[Data$Méthode == "E"] <- "email"
Clean$Methode[Data$Méthode == "M"] <- "tel"
Clean$Methode[Data$Méthode == "L"] <- "poste"
Clean$Methode[Data$Méthode == "H"] <- "tel"
table(Clean$Methode)

table(Data$Activité)
unique(Data$Activité)
unique(Data$Groupe.cible)
