library(tidyverse)


Data <- readRDS("_SharedFolder_fondation-ulaval/Data/CleanData.rds") %>% 
  mutate(donated = ifelse(historic_lastDonation_amount > 0, 1, 0))

table(Data$donated)
names(Data)
table(Data$historic_donationLevel)
table(Data$historic_donationLevel_gouverneur)

Data$historic_donationLevelVD <- 0
Data$historic_donationLevelVD[Data$historic_donationLevel == "0.125"] <- 0.33
Data$historic_donationLevelVD[Data$historic_donationLevel == "0.25"] <- 0.66
Data$historic_donationLevelVD[Data$historic_donationLevel >= "0.375"] <- 1
table(Data$historic_donationLevelVD)


table(Data$emploi_teacherUlaval)

options(digits=4)

table(Data$contact_declineCommunications_number)
hist(Data$contact_declineCommunications_number)
Data$refusContact <- 0
Data$refusContact <- log(Data$contact_declineCommunications_number)
table(Data$refusContact)

minmaxNormalization <- function(x) {
  return((x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)))
}

Data$refusContact <- 0
Data$refusContact[Data$contact_declineCommunications_number == 1] <- 1
Data$refusContact[Data$contact_declineCommunications_number == 2] <- 2
Data$refusContact[Data$contact_declineCommunications_number == 3] <- 3
Data$refusContact[Data$contact_declineCommunications_number == 4] <- 4
Data$refusContact[Data$contact_declineCommunications_number == 5] <- 5
Data$refusContact[Data$contact_declineCommunications_number == 6] <- 6
Data$refusContact[Data$contact_declineCommunications_number == 7] <- 7
Data$refusContact[Data$contact_declineCommunications_number == 8] <- 8
Data$refusContact[Data$contact_declineCommunications_number == 9] <- 9
Data$refusContact[Data$contact_declineCommunications_number == 10] <- 10
Data$refusContact[Data$contact_declineCommunications_number == 11] <- 11
Data$refusContact[Data$contact_declineCommunications_number == 12] <- 12
Data$refusContact[Data$contact_declineCommunications_number >= 13] <- 13
table(Data$refusContact)

Data$normRefusContact <- 0
Data$normRefusContact <- minmaxNormalization(Data$refusContact)
table(Data$normRefusContact)

table(Data$emploi_technicianUlaval)

#Teacher
DataTeacher <- Data %>% 
  filter(emploi_teacherUlaval == 1)

model1 <- glm(donated ~ ses_female + normRefusContact, data = DataTeacher, family = binomial(link = "logit"))
summary(model1)

model2 <- glm(donated ~ ses_female + emploi_teacherUlaval + normRefusContact, data = Data, family = binomial())
summary(model2)

#Executive
DataExecutive <- Data %>% 
  filter(emploi_executiveUlaval == 1)

model3 <-glm(donated ~ ses_female + emploi_executiveUlaval + normRefusContact, data = Data, family = binomial())
summary(model3)

model4 <- glm(donated ~ ses_female + normRefusContact, data = DataExecutive, family = binomial())
summary(model4)

#Retired
DataRetired <- Data %>% 
  filter(emploi_retiredUlaval == 1)

model <-glm(donated ~ ses_female + emploi_retiredUlaval + normRefusContact, data = Data, family = binomial())
summary(model)

model <- glm(donated ~ ses_female + normRefusContact, data = DataRetired, family = binomial())
summary(model)

#Professional
table(Data$emploi_professionalUlaval)

DataProfessional <- Data %>% 
  filter(emploi_professionalUlaval == 1)

model <-glm(donated ~ ses_female + emploi_professionalUlaval + normRefusContact, data = Data, family = binomial())
summary(model)

model <- glm(donated ~ ses_female + normRefusContact, data = DataProfessional, family = binomial())
summary(model)


#Lecturer
table(Data$emploi_lecturerUlaval)

DataLecturer <- Data %>% 
  filter(emploi_lecturerUlaval == 1)

model <-glm(donated ~ ses_female + emploi_lecturerUlaval + normRefusContact, data = Data, family = binomial())
summary(model)

model <- glm(donated ~ ses_female + normRefusContact, data = DataLecturer, family = binomial())
summary(model)



### Predicted probabilities #####

# Based on year of first graduation
DataDegrees <- Data %>% 
  filter(education_FirstDegree_Medecine == 1 |
           education_FirstDegree_Admin ==  1 |
           education_FirstDegree_Music == 1 |
           education_FirstDegree_AgroNutri == 1 |
           education_FirstDegree_Pharm == 1)

table(DataDegrees$education_FirstDegree_Admin)
table(DataDegrees$education_FirstDegree_Music)
table(DataDegrees$education_FirstDegree_Medecine)
table(DataDegrees$education_FirstDegree_AgroNutri)
table(DataDegrees$education_FirstDegree_Pharm)

model <- glm(donated ~ ses_female + education_yearFirstGraduation +
               ses_origin_qc + ses_origin_roc + education_FirstDegree_Medecine +
               education_FirstDegree_Music + education_FirstDegree_Pharm +
               education_FirstDegree_AgroNutri,
             family = binomial(),
             data = DataDegrees)
summary(model)

#### 2. Odds
odds <- exp(model$coefficients) 
probability <- odds / (1 + odds)

subset <- DataDegrees[1:5,]


fake <- expand.grid(c(0,1), c(0,1), c(0,1), 1975:2022) %>% 
  filter(!(Var2==1&Var3==1))

names(fake) <- c('ses_female', 'ses_origin_qc', 'ses_origin_roc',
                 'education_yearFirstGraduation')

fake$education_FirstDegree_Medecine <- 0
fake$education_FirstDegree_Music <- 0
fake$education_FirstDegree_AgroNutri <- 0
fake$education_FirstDegree_Pharm <- 0

medecins <- fake %>% mutate(education_FirstDegree_Medecine = 1)
music <- fake %>% mutate(education_FirstDegree_Music = 1)
agro <- fake %>% mutate(education_FirstDegree_AgroNutri = 1)
pharm <- fake %>% mutate(education_FirstDegree_Pharm = 1)

All <- rbind(fake, medecins, music, agro, pharm) %>% 
  mutate(pred = predict(object = model, newdata = ., type = "response")*100,
         degree = ifelse(education_FirstDegree_Medecine == 1, "Médecine", "Administration"),
         degree = ifelse(education_FirstDegree_Music == 1, "Musique", degree),
         degree = ifelse(education_FirstDegree_AgroNutri == 1, "Agronomie", degree),
         degree = ifelse(education_FirstDegree_Pharm == 1, "Pharmacie", degree),
         )


ggplot(All, aes(x = education_yearFirstGraduation, y = pred, group = degree, color = degree)) +
  #geom_point(alpha = 0.2, shape = 2) +
  #geom_line(alpha = 0.2) +
  geom_smooth(se = F , span = 0.5,
              linewidth = 2.5) +
  xlab("") +
  ylab("\nProbabilité de donner (%)\n") +
  clessnverse::theme_clean_light(base_size = 12) +
  scale_y_continuous(limits = c(0,75)) +
  scale_x_continuous(breaks = seq(1975, 2020, by = 10)) +
  scale_color_manual(values = c("Médecine" = "#e30513",
                                "Musique" = "#ffc103",
                                "Agronomie" = "#7f7f7f",
                                "Pharmacie" = "#000000",
                                "Administration" = "blue" )) +
  theme(legend.position = "right",
        axis.line.x = element_blank(),
        legend.text = element_text(size = 25),
        axis.title.x = element_text(size = 60),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 30, hjust = 0.5),
        axis.text.y = element_text(size = 30))

ggsave("_SharedFolder_fondation-ulaval/pres_2022-12-06/graphs/3.probs_year_degree.png",
       width = 12, height = 10)


# Based on number of decline of communication


###Exploration 

modeltest <- glm(prospectif_plannedDonation  ~ ses_age + education_FirstDegree_Medecine +
                   education_FirstDegree_Music + education_FirstDegree_Pharm +
                   education_FirstDegree_AgroNutri,
                 family = binomial(),
                 data = DataD)

summary(modeltest)

hist(Data$prospectif_openPledges_totalAmount)

