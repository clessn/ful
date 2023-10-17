# Packages ----------------------------------------------------------------
library(tidyverse)
source("code/pipeline/functions.R", encoding = "UTF-8")

# Data --------------------------------------------------------------------
DataOutcome <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/datacomm_outcome.rds") %>%
  filter(comm_type == "sollicitation") %>% 
  drop_na(OutcomeSollicitationReussie) %>% 
  mutate(outcome = ifelse(OutcomeSollicitationReussie > 0.5, 1, 0))
DataRep <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/CleanData.rds") %>% 
  distinct(UL_NO_CODE, .keep_all = TRUE)
All <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/datacomm.rds") %>% 
  select(-activite, -groupe_cible, -Methode) %>% 
  filter(comm_type == "sollicitation") %>% 
  arrange(date_comm) %>% 
  group_by(UL_NO_CODE) %>% 
  mutate(i = 1,
         rolling_n_sollicitation = cumsum(i)) %>% 
  distinct(UL_NO_CODE, date_comm, .keep_all = TRUE) %>% 
  select(UL_NO_CODE, date_comm, rolling_n_sollicitation)

# Has donated as a given date ---------------------------------------------
dates_first_dons <- DataRep$historic_dateFirstDonation
names(dates_first_dons) <- DataRep$UL_NO_CODE

DataOutcome$date_first_donation <- dates_first_dons[DataOutcome$UL_NO_CODE]

DataOutcome$has_donated_asof_date <- don_first(DataOutcome$date_comm, DataOutcome$date_first_donation)

Main <- DataOutcome

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

RepSubset <- RepSubset %>%
  select(-all_of(paste0("ro_sport_", 1:4))) %>% 
  mutate(across(starts_with("rosport_"), as.integer),
         across(starts_with("rosport_"), ~replace_na(.x, 0)))


Main <- left_join(Main, RepSubset, by = "UL_NO_CODE") %>% 
  left_join(., All, by = c("UL_NO_CODE", "date_comm")) %>% 
  select(-c(OutcomeEvenementAssist, OutcomeEvenementRSVP,
            OutcomeSollicitationReussie,
            activite, groupe_cible, date_first_donation))

#### MODELING
mdata <- Main %>% 
  select(-UL_NO_CODE, -date_comm, -comm_type) %>% 
  mutate(Methode = factor(Methode),
         rolling_n_sollicitation = ifelse(rolling_n_sollicitation > 15, 16, rolling_n_sollicitation),
         rolling_n_sollicitation = factor(rolling_n_sollicitation)) %>% 
  drop_na()

model <- glm(outcome ~ .,
             data = mdata,
             family = binomial(link = "logit"))

saveRDS(model, "_SharedFolder_fondation-ulaval/Data/pipeline/marts/models/modele_predict_outcome.rds")
