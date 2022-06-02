# library 
library(tidyverse)


# loading data and initial cleanning
Data <- read.csv("/Users/alexandrecote/Dropbox/_CLESSN/ful/_SharedFolder_fondation-ulaval/data/1 - diplômés.CSV")


Encoding(Data) <- "UTF-8"

Freq06 <- data.frame(mots = vec06)


Freq06$mots <- gsub("`|\\'", "", iconv(Freq06$mots, to="ASCII//TRANSLIT"))
Freq06$mots <- str_replace_all(Freq06$mots, "[^[:alnum:]]", " ")