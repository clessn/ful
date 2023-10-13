library(tidyverse)
library(data.table)

DataComm <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/datacomm.rds")
DataOutcome <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/datacomm_outcome.rds")
DataRep <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/CleanData.rds") %>% 
  distinct(UL_NO_CODE, .keep_all = TRUE)


ids <- sample(DataComm$UL_NO_CODE, 100)


ul_ids <- c(sample(DataRep$UL_NO_CODE, 3), "00E00B004ZU38.L23")
datecomms <- as.Date(c(sample(DataComm$date_comm, 3), "2015-06-10"))
datedons <- as.Date(c(NA, NA, "2021-08-15", "1987-11-30"))



### fonction ###

don_first <- function(datecomms, datedons, data = DataRep){
  # S'assure que les dates sont au format Date
  datecomms <- as.Date(datecomms)
  datedons <- as.Date(datedons)
  
  # Utilise ifelse pour gérer les vecteurs
  output <- ifelse(is.na(datedons), 
                   0, 
                   ifelse(datecomms >= datedons, 1, 0))
  return(output)
}

result <- don_first(datecomms, datedons)
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























