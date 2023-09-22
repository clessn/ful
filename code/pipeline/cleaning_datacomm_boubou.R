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


# Motif -------------------------------------------------------------------
table(Data$Motif)
Clean$OutcomeEvenementAssist <- NA
Clean$OutcomeEvenementAssist[Data$Motif == "E-Assisté"] <- 1
Clean$OutcomeEvenementAssist[Data$Motif == "E-PasAssi."] <- 0
table(Clean$OutcomeEvenementAssist)

Clean$OutcomeEvenementRSVP <- NA
Clean$OutcomeEvenementRSVP[Data$Motif == "E-RSVP oui"] <- 1
Clean$OutcomeEvenementRSVP[Data$Motif == "E-RSVP non"] <- 0
table(Clean$OutcomeEvenementRSVP)

Clean$OutcomeSollicitationReussie <- NA
Clean$OutcomeSollicitationReussie[Data$Motif == "S-Accepté"] <- 1 # Positif
Clean$OutcomeSollicitationReussie[Data$Motif == "S-À venir"] <- 1
Clean$OutcomeSollicitationReussie[Data$Motif == "S-Don lig."] <- 1
Clean$OutcomeSollicitationReussie[Data$Motif == "S-Prom.Cd."] <- 1
Clean$OutcomeSollicitationReussie[Data$Motif == "S-Réfléchi"] <- 0.5 # Maybe
Clean$OutcomeSollicitationReussie[Data$Motif == "S-Courriel"] <- 0.5
Clean$OutcomeSollicitationReussie[Data$Motif == "S-Info."] <- 0.5
Clean$OutcomeSollicitationReussie[Data$Motif == "S-Non rej."] <- 0.25 # Non rejoint
Clean$OutcomeSollicitationReussie[Data$Motif == "S-Refusé"] <- 0 # Negatif
table(Clean$OutcomeSollicitationReussie)

saveRDS(Clean, "_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/datacommBoubou.rds")
