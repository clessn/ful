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























