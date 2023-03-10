
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

Data$yearSinceMIGraduation <- NA
Data$yearSinceMIGraduation <- factor(2023 - Data$education_yearMIGraduation)
table(Data$yearSinceMIGraduation)

model <- glm(donated ~ yearSinceMIGraduation + Programme + ses_female, 
             data = Data, family = binomial())
summary(model)


df <- expand.grid(yearSinceMIGraduation = factor(1:63),
                  Programme = unique(Data$Programme),
                  ses_female = c(0, 1))
view(df)

df$prob <- predict(model, newdata = df,
  type = "response")

GraphData <- df %>% 
  group_by(yearSinceMIGraduation, Programme) %>% 
  summarise(prob = mean(prob))

ggplot(data = GraphData, aes(y = prob, x = yearSinceMIGraduation)) +
  #geom_point(aes(color = Programme)) +
  geom_line(aes(color = Programme, group = Programme))





