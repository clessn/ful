library(tidyverse)
library(data.table)
library(pbapply)

DataComm <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/datacomm.rds")
DataOutcome <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/datacomm_outcome.rds")
DataRep <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/CleanData.rds") %>% 
  distinct(UL_NO_CODE, .keep_all = TRUE)


ul_id <- "00E00B068ZU68.L23"
datecomm <- as.Date("2015-04-12")

datedon <- DataRep$historic_dateFirstDonation[DataRep$UL_NO_CODE == ul_id]

### nb of comm by commtype ###

DataComm$n_sollicitation <- get_n_comm(commtype = "sollicitation")
DataComm$n_evenement <- get_n_comm(commtype = "evenement")
DataComm$n_fidelisation <- get_n_comm(commtype = "fidelisation")
DataComm$n_remerciement <- get_n_comm(commtype = "remerciement")
DataComm$n_sondage <- get_n_comm(commtype = "sondage")


### vecteur ###

vec_datecomm <- as.Date(c(DataComm$date_comm))
vec_ul_id <- c(DataRep$UL_NO_CODE)
vec_datedon <- as.Date(c(DataRep$historic_dateFirstDonation[DataRep$UL_NO_CODE == ul_id]))



### fonction ###

don_first <- function(ul_id, datecomm, datedon, data = DataRep){
  if (is.na(datedon)) {
    return(0)
  } else if (datecomm >= datedon) {
    return(1)
  } else {
    return(0)
  }
}

result <- don_first(ul_id, datecomm, datedon)
print(result)



resultats <- numeric(length(vec_ul_id))

for (i in 1:length(vec_ul_id)) {
  id <- vec_ul_id[i]
  datedon <- DataRep$historic_dateFirstDonation[DataRep$UL_NO_CODE == id]
  resultats[i] <- don_first(id, datecomm, datedon)
  if (i %% 100 == 0)  {
    print(i)
  }
}


print(resultats)



prop == à chaque date, voir (cumsum nb outcome positif)/(cumsum nb outcome total)



prop_outcome <- function(commtype, data = DataOutcome){
  output <- data %>% 
    mutate(is_commtype = ifelse(comm_type == commtype, 1, 0),
                   initorder = row_number()) %>%
    arrange(date_comm) %>% 
    group_by(UL_NO_CODE) %>% 
    mutate(n = cumsum(is_commtype),
           n = ifelse(n == 0, 0, n-1),
           ) %>% 
    ungroup() %>% 
    arrange(initorder) %>% 
    pull(., n)
    
}




cumsum(DataOutcome$)

prop_outcome <- function(commtype, data = DataOutcome) {
  
  output <- data %>% 
    mutate(is_commtype = ifelse(comm_type == commtype, 1, 0),
           initorder = row_number(),
           OutcomeSollicitationReussie = ifelse(OutcomeSollicitationReussie %in% c(0, 0.25, 0.5), 0, 1),
           OutcomeSollicitationReussie = ifelse(comm_type != "sollicitation", 0, OutcomeSollicitationReussie)) %>%
    arrange(date_comm) %>% 
    group_by(UL_NO_CODE) %>% 
    mutate(n_commtype = cumsum(is_commtype),
           n_commtype = ifelse(n_commtype == 0, 0, n_commtype-1),
           cumsum_positive = cumsum(OutcomeSollicitationReussie),
           cumsum_positive = ifelse(cumsum_positive == 0, 0, cumsum_positive-1)) %>% 
    ungroup() %>% 
    arrange(initorder) %>% 
    mutate(proportion = cumsum_positive / n_commtype) %>%
    select(date_comm, proportion)
  
  return(output)
}













