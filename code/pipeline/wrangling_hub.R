# Packages ----------------------------------------------------------------
library(tidyverse)
library(data.table)
source("code/pipeline/functions.R", encoding = "UTF-8")

# Data --------------------------------------------------------------------
DataComm <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/datacomm.rds") %>% 
  select(-activite, -groupe_cible, -Methode)
DataOutcome <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/datacomm_outcome.rds")
DataRep <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/CleanData.rds") %>% 
  distinct(UL_NO_CODE, .keep_all = TRUE)

# Has donated as a given date ---------------------------------------------
dates_first_dons <- DataRep$historic_dateFirstDonation
names(dates_first_dons) <- DataRep$UL_NO_CODE

DataComm$date_first_donation <- dates_first_dons[DataComm$UL_NO_CODE]

DataComm$has_donated_asof_date <- don_first(DataComm$date_comm, DataComm$date_first_donation)

# Stable respondents variables --------------------------------------------

## 1. wrangle, select ------------------------------------------------------
sports <- c("NATATI", "ATHLET", "BASKET", "VOLLEY", "FOOTBA",
            "SOCCER", "R&OGEN", "SKIRO", "HOCKEY", "XCOUNT",
            "BADMIN", "RUGBY", "HANDBA", "TENNIS", "GOLFRO")

RepSubset <- DataRep %>% 
  select(UL_NO_CODE, ses_female, ses_origin_qc,
         ses_origin_roc, ses_origin_other, Programme,
         education_yearFirstGraduation, education_yearMIGraduation,
         starts_with("ro_sport"), ro_athlete) %>% 
  mutate(ses_origin = case_when(
    ses_origin_roc == 1 ~ "roc",
    ses_origin_qc == 1 ~ "qc",
    ses_origin_other == 1 ~ "other"
  ),
  ses_origin = factor(ses_origin),
  Programme = factor(Programme)) %>% 
  select(-c(ses_origin_roc, ses_origin_qc, ses_origin_other))
# Créer une colonne dummy pour chaque sport
for (sport in sports) {
  RepSubset <- RepSubset %>%
    mutate(!!paste0("rosport_", sport) := apply(select(., starts_with("ro_sport")) == sport, 1, any))
}

RepSubset2 <- RepSubset %>%
  select(-all_of(paste0("ro_sport_", 1:4))) %>% 
  mutate(across(starts_with("rosport_"), as.integer),
         across(starts_with("rosport_"), ~replace_na(.x, 0)))

CommWithStable <- left_join(DataComm, RepSubset2, by = "UL_NO_CODE")

# Add rolling comm variables ----------------------------------------------

CommWithStable$rolling_n_sollicitation <- get_n_comm("sollicitation")
CommWithStable$rolling_n_fidelisation <- get_n_comm("fidelisation")
CommWithStable$rolling_n_evenement <- get_n_comm("evenement")
CommWithStable$rolling_n_remerciement <- get_n_comm("remerciement")
CommWithStable$rolling_n_sondage <- get_n_comm("sondage")


