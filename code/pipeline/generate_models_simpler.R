# Packages ----------------------------------------------------------------
library(tidyverse)
library(caret)
library(randomForest)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/marts/data_for_models.rds")

set.seed(198)
Mdata <- Data %>% 
  filter(has_donated_asof_date == 0) %>% 
  select(-c(UL_NO_CODE, date_comm, date_first_donation,
            has_donated_asof_date, rolling_n_sondage)) %>% 
  mutate(ses_origin = relevel(ses_origin, ref = "qc"),
         rolling_n_sollicitation = ifelse(rolling_n_sollicitation > 15, 16, rolling_n_sollicitation),
         rolling_n_fidelisation = ifelse(rolling_n_fidelisation > 5, 6, rolling_n_fidelisation),
         rolling_n_evenement = ifelse(rolling_n_evenement > 15, 16, rolling_n_evenement),
         rolling_n_sollicitation = factor(rolling_n_sollicitation),
         rolling_n_fidelisation = factor(rolling_n_fidelisation),
         rolling_n_evenement = factor(rolling_n_evenement),
         rolling_n_remerciement = factor(rolling_n_remerciement)) %>% 
  drop_na()
  ### get a random sample of 100 000 communications
  #%>% slice_sample(n = 100000)

# 1. One model by program -------------------------------------------------------

set.seed(145)
for (i in 1:length(unique(Mdata$Programme))){
  prog <- unique(Mdata$Programme)[i]
  d <- Mdata %>%
    filter(Programme == prog)
  if (nrow(d) > 50000){
    d <- d %>% 
      slice_sample(n = 50000)
  }
  y <- d$delta_first_don
  X <- d %>% select(-delta_first_don)
  # optimize parameters
  message("optimisation starts")
  best_mtry <- tuneRF(X, as.factor(y), plot=FALSE, ntreeTry=50,
                      stepFactor = 1.5, verbose = TRUE)[[1]]
  # Fit random forest model using the best mtry
  message("fit starts")
  modeli <- randomForest(X, y, mtry=best_mtry,
                         importance=FALSE, verbose = TRUE)
  saveRDS(modeli, paste0("_SharedFolder_fondation-ulaval/Data/pipeline/marts/models/", prog, ".rds"))
  message(paste0(i, " - ", prog))
}


set.seed(145)
for (i in 1:length(unique(Mdata$Programme))){
  prog <- unique(Mdata$Programme)[i]
  d <- Mdata %>%
    filter(Programme == prog) %>% 
    select(-Programme)
  pos <- which(d$delta_first_don == 1)
  neg <- which(d$delta_first_don == 0)
  neg_sample <- sample(neg, size = (length(pos)*100/5)-length(pos))
  d <- d[c(pos, neg_sample),]
  modeli <- glm(delta_first_don ~ .,
                data = d,
                family = binomial(link = "probit"))
  saveRDS(modeli, paste0("_SharedFolder_fondation-ulaval/Data/pipeline/marts/models/glms/", prog, ".rds"))
  message(paste0(i, " - ", prog))
}

d <- Mdata
pos <- which(d$delta_first_don == 1)
neg <- which(d$delta_first_don == 0)
neg_sample <- sample(neg, size = (length(pos)*100/5)-length(pos))
d <- d[c(pos, neg_sample),]
modeli <- glm(delta_first_don ~ .,
              data = d,
              family = binomial(link = "probit"))
saveRDS(modeli, paste0("_SharedFolder_fondation-ulaval/Data/pipeline/marts/models/glms/all_progs.rds"))



