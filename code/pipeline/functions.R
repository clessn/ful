library(tidyverse)
library(data.table)
DataComm <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/warehouse/datacomm.rds")

# Convert DataComm to a data.table
setDT(DataComm)

commtype <- "sollicitation"
date <- as.Date("2015-04-12")
ul_id <- "00E00B068ZU68.L23"

# function

get_n_comm <- function(ul_id, commtype, date, data = DataComm){
  date <- as.Date(date)
  n <- nrow(data[UL_NO_CODE == ul_id &
                       comm_type == commtype &
                       date_comm < date,
                     .(date_comm, comm_type, UL_NO_CODE)])
  return(n)
}


get_n_comm(ul_id = "00E12B214ZU40.L23", date = "2020-10-9", commtype = "evenement")
















