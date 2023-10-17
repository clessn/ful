# Packages ----------------------------------------------------------------
library(tidyverse)
library(caret)
library(randomForest)
library(doParallel)
library(foreach)
library(glmnet)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/marts/data_for_models.rds")

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

# 1. Simplest possible -------------------------------------------------------

# Number of chunks you want
n_chunks <- 8

chunks <- Mdata %>%
  group_by(across(where(is.factor))) %>%
  mutate(chunk_id = (row_number() - 1) %% n_chunks + 1) %>%
  ungroup() %>%
  split(.$chunk_id)

# Remove the chunk_id column from each chunk
chunks <- lapply(chunks, function(chunk) {
  select(chunk, -chunk_id)
})


data_splits <- split(Mdata, rep(1:8, each = nrow(Mdata) / 8))

fit_glm <- function(data_chunk) {
  glm(delta_first_don ~ . +
        rolling_n_sollicitation*Programme, data = data_chunk, family = binomial())
}

fit_rf <- function(data_chunk) {
  # Extract response and predictors
  y <- data_chunk$delta_first_don
  X <- data_chunk %>% select(-delta_first_don)
  # Find best mtry using tuneRF
  best_mtry <- tuneRF(X, as.factor(y), plot=FALSE, ntreeTry=100)[[1]]
  # Fit random forest model using the best mtry
  rf_model <- randomForest(X, y, mtry=best_mtry, importance=FALSE)
  return(rf_model)
}


cl <- makePSOCKcluster(8)

registerDoParallel(cl)


### GLM binomial
# Appliquer la fonction glm en parallèle sur les morceaux de données
results <- foreach(chunk = chunks, .combine = c,
                   .verbose = TRUE) %dopar% {
  fit_glm(chunk)
}

### RandomForest
#start.time <- proc.time()
## Appliquer la fonction glm en parallèle sur les morceaux de données
#results <- foreach(chunk = chunks, .combine = c,
#                   .packages = c("tidyverse", "randomForest"),
#                   .verbose = TRUE) %dopar% {
#  fit_rf(chunk)
#}
#stop.time <- proc.time()

saveRDS(results, "_SharedFolder_fondation-ulaval/Data/pipeline/marts/premodel2.rds")

stopCluster(cl) 
