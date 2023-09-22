# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/lake/datacomm.rds") %>% 
  filter(UL_NO_CODE != "")

# Create clean data -------------------------------------------------------
Clean <- data.frame(
  UL_NO_CODE = Data$UL_NO_CODE,
  date_comm = as.Date(Data$Date)
)


# Lettre (type de comm) ------------------------------------------------------------------

Clean$comm_type <- NA

#### Sollicitation ####
Clean$comm_type[Data$Lettre %in% c("Lettre Fiche de sollicitation",
                                   "Fiche promesse conditionnelle",
                                   "Fiche désire réfléchir",
                                   "1er Relance PC en PP",
                                   "2e Relance PC en PP",
                                   "Fiche de sollicitation",
                                   "1er Relance Sol. en TM",
                                   "Sollicitation (Conversion)",
                                   "2e Relance Sol. en PP",
                                   "Courriel de sollicitation",
                                   "2e Relance Sol. par Courriel",
                                   "Confirmation pour PC",
                                   "1er Relance Sol. par Courriel",
                                   "2e Relance Sol. en TM",
                                   "Demande d'autorisation de PD",
                                   "1er Relance Sol. en PP",
                                   "3e Relance Sol. en Courriel",
                                   "3e Relance Sol. en TM",
                                   "Courriel de désire réfléchir",
                                   "1er Relance PC en Courriel",
                                   "2e Relance PC en Courriel",
                                   "3e Relance PC en TM",
                                   "Majoration don mensuel en PP",
                                   "3e Relance Sol. en PP",
                                   "Fiche de cotisation",
                                   "Confirmation pour CC"
)] <- "sollicitation"


#### Évènement ####
Clean$comm_type[Data$Lettre %in% c("Courriel d'invitation",
                                   "Lettre retrouvailles 5 ans",
                                   "Lettre d'invitation Dévelop.",
                                   "Lettre cocktail facultaire",
                                   "Lettre golf Québec",
                                   "Lettre golf Montréal",
                                   "Lettre d'invitation",
                                   "Retrouv. facultaires 5 ans",
                                   "Courriel de retrouvailles",
                                   "Lettre d'invitation facultaire",
                                   "Carte d'invitation facultaire",
                                   "Carte d'invitation Dévelop.",
                                   "Courriel d'invitation facult.",
                                   "Courriel golf Québec",
                                   "Courriel coktail facultaire",
                                   "Courriel de rappel",
                                   "Téléphone de rappel",
                                   "Lettre d'information",
                                   "Lettre de rappel",
                                   "Courriel d'information supp.",
                                   "Lettre information supplémenta"
)] <- "evenement"


#### Fidélisation ####
Clean$comm_type[Data$Lettre %in% c("Fiche de renouvell. d'adhésion",
                                   "Acquisition Carte Partenaire",
                                   "Courriel acquisition Carte P",
                                   "Lettre d'invitation Fidel.",
                                   "Carte d'invitation Fidel.",
                                   "Envoi de l'épinglette"
)] <- "fidelisation"


#### Remerciement ####
Clean$comm_type[Data$Lettre %in% c("Lettre remerciements",
                                   "Courriel de remerciement",
                                   "Courriel remerciement"
)] <- "remerciement"


#### Information ####
Clean$comm_type[Data$Lettre %in% c("Courriel sondage")] <- "sondage"


## Factorize ####
Clean$comm_type <- factor(Clean$comm_type, ordered = FALSE)
table(Clean$comm_type)


# Activite et groupe cible ----------------------------------------------------------------

Clean$activite <- Data$Activité
Clean$groupe_cible <- Data$Groupe.cible



# Save it -----------------------------------------------------------------
saveRDS(Clean, "_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/datacomm.rds")

