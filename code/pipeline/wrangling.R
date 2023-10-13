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

#don_first <- function(ul_id, datecomm, datedon, data = DataRep){
#}

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












