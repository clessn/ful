# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
files <- list.files("_SharedFolder_fondation-ulaval/Data/DataComm")

for (i in 1:length(files)){
  d <- read.csv(file = paste0("_SharedFolder_fondation-ulaval/Data/DataComm/", files[i]),
                fileEncoding = "ISO-8859-1")
  if (i == 1){
    df <- d
  } else {
    df <- rbind(df, d)
  }
  print(i)
}

saveRDS(df, "_SharedFolder_fondation-ulaval/Data/pipeline/lake/datacomm.rds")
