
library(tidyverse)
library(lubridate)


Data <- readRDS("_SharedFolder_fondation-ulaval/Data/CleanData.rds") %>% 
  mutate(donated = ifelse(historic_lastDonation_amount > 0, 1, 0)) %>% 
  filter((education_yearMIGraduation >= 1960) & (education_FirstDegree_Medecine == 1 |
     education_FirstDegree_Admin == 1 |
     education_FirstDegree_AgroNutri == 1 |
     education_FirstDegree_Music == 1 |
     education_FirstDegree_Pharm == 1)) 


Data$Programme <- NA
Data$Programme[Data$education_FirstDegree_Medecine == 1] <- "Médecine"
Data$Programme[Data$education_FirstDegree_Admin == 1] <- "Administration"
Data$Programme[Data$education_FirstDegree_AgroNutri == 1] <- "Agronomie"
Data$Programme[Data$education_FirstDegree_Music == 1] <- "Musique"
Data$Programme[Data$education_FirstDegree_Pharm == 1] <- "Pharmacie"
table(Data$Programme)

Data$Programme <- factor(Data$Programme, ordered = F)
table(Data$Programme)

Data$education_yearFirstGraduation <- factor(Data$education_yearFirstGraduation, ordered = T)
unique(Data$education_yearFirstGraduation)

Data$education_yearMIGraduation <- factor(Data$education_yearMIGraduation, ordered = T)

hist(Data$education_yearFirstGraduation)




model <- glm(donated ~ education_yearFirstGraduation*Programme + ses_female, 
             data = Data, family = binomial())
summary(model)

year()

 "historic_dateFirstDonation" - "education_yearMIGraduation" 


Data$donationGap <- NA
Data$donationGap[] 


expand.grid()
predict(type = "response")
