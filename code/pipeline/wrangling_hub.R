library(tidyverse)
library(data.table)
source("code/pipeline/functions.R", encoding = "UTF-8")

DataComm <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/datacomm.rds")
DataOutcome <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/datacomm_outcome.rds")
DataRep <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/CleanData.rds") %>% 
  distinct(UL_NO_CODE, .keep_all = TRUE)


# Has donated as a given date ---------------------------------------------
dates_first_dons <- DataRep$historic_dateFirstDonation
names(dates_first_dons) <- DataRep$UL_NO_CODE

DataComm$date_first_donation <- dates_first_dons[DataComm$UL_NO_CODE]

DataComm$has_donated_asof_date <- don_first(DataComm$date_comm, DataComm$date_first_donation)

