library(tidyverse)
library(readr)

###Get data###

graduateCampagne04 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2004.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne05 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2005.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne06 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2006.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne07 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2007.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne08 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2008.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne09 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2009.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne10 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2010.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne11 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2011.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne12 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2012.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne13 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2013.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne14 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2014.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne15 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2015.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne16 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2016.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne17 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2017.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne18 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2018.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne19 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2019.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne20 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2020.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne21 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2021.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne22 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2022.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateCampagne23 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - campagne 2023.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

graduateEvenement04 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2004.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement05 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2005.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement06 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2006.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement07 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2007.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement08 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2008.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement09 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2009.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement10 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2010.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement11 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2011.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement12 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2012.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement13 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2013.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement14 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2014.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement15 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2015.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement16 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2016.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement17 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2017.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement18 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2018.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement19 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2019.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement20 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2020.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement21 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2021.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement22 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2022.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduateEvenement23 <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/1 - diplômés - événement 2023.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

studiedCampagne <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/2 - a déjà étudié UL - Campagnes.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
studiedEvenement <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/2 - a déjà étudié UL - Événements.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

studentCampagne <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/3 - Étudiant - Campagnes.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
studentEvenement <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/3 - Étudiant - Événements.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

friendCampagne <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/5 - Ami - Campagnes.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
friendEvenement <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/5 - Ami - Événements.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

teaching_staffCampagne <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/6 - Personnel enseignant - Campagnes.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
teaching_staffEvenement <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/6 - Personnel enseignant - Événements.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

non_teaching_staffCampagne <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/7 - Personnel non-enseignant - Campagnes.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
non_teaching_staffEvenement <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/7 - Personnel non-enseignant - Événements.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

otherCampagne <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/9 - Autres - Campagnes.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
otherEvenement <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/9 - Autres - Événements.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

inactive_employeeCampagne <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/50 - Employé inactif - Campagnes.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
inactive_employeeEvenement <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/50 - Employé inactif - Événements.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

retiredCampagne <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/51 - Retraité UL - Campagnes.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
retiredEvenement <- read.csv("/home/alexab/Dropbox/CLESSN/ful/_SharedFolder_fondation-ulaval/Data/DataComm/51 - Retraité UL - Événements.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

###Add column class, rbind selon class and select variables###

GraduateCampagne <- rbind(graduateCampagne04, graduateCampagne05, graduateCampagne06,
                          graduateCampagne07, graduateCampagne08, graduateCampagne09,
                          graduateCampagne10, graduateCampagne11, graduateCampagne12,
                          graduateCampagne13, graduateCampagne14, graduateCampagne15,
                          graduateCampagne16, graduateCampagne17, graduateCampagne18,
                          graduateCampagne19, graduateCampagne20, graduateCampagne21,
                          graduateCampagne22, graduateCampagne23)
names(GraduateCampagne) <- make.unique(as.character(GraduateCampagne[1,]))
GraduateCampagne <- GraduateCampagne[-1,] %>% 
  mutate(class = "graduateCampagne") 

GraduateEvenement <- rbind(graduateEvenement04, graduateEvenement05, graduateEvenement06,
                          graduateEvenement07, graduateEvenement08, graduateEvenement09,
                          graduateEvenement10, graduateEvenement11, graduateEvenement12,
                          graduateEvenement13, graduateEvenement14, graduateEvenement15,
                          graduateEvenement16, graduateEvenement17, graduateEvenement18,
                          graduateEvenement19, graduateEvenement20, graduateEvenement21,
                          graduateEvenement22, graduateEvenement23)
names(GraduateEvenement) <- make.unique(as.character(GraduateEvenement[1,]))
GraduateEvenement <- GraduateEvenement[-1,] %>% 
  mutate(class = "graduateEvenement") 

names(studiedCampagne) <- make.unique(as.character(studiedCampagne[1,]))
studiedCampagne <- studiedCampagne[-1,] %>% 
  mutate(class = "studiedCampagne") 
names(studiedEvenement) <- make.unique(as.character(studiedEvenement[1,])) 
studiedEvenement <- studiedEvenement[-1,] %>% 
  mutate(class = "studiedEvenement") 

names(studentCampagne) <- make.unique(as.character(studentCampagne[1,]))
studentCampagne <- studentCampagne[-1,] %>%
  mutate(class = "studentCampagne") 
names(studentEvenement) <- make.unique(as.character(studentEvenement[1,]))
studentEvenement <- studentEvenement[-1,] %>%
  mutate(class = "studentEvenement") 

names(friendCampagne) <- make.unique(as.character(friendCampagne[1,]))
friendCampagne <- friendCampagne[-1,] %>%
  mutate(class = "friendCampagne") 
names(friendEvenement) <- make.unique(as.character(friendEvenement[1,]))
friendEvenement <- friendEvenement[-1,] %>%
  mutate(class = "friendEvenement") 

names(teaching_staffCampagne) <- make.unique(as.character(teaching_staffCampagne[1,]))
teaching_staffCampagne <- teaching_staffCampagne[-1,] %>%
  mutate(class = "teaching staffCampagne") 
names(teaching_staffEvenement) <- make.unique(as.character(teaching_staffEvenement[1,]))
teaching_staffEvenement <- teaching_staffEvenement[-1,] %>%
  mutate(class = "teaching staffEvenement") 

names(non_teaching_staffCampagne) <- make.unique(as.character(non_teaching_staffCampagne[1,]))
non_teaching_staffCampagne <- non_teaching_staffCampagne[-1,] %>%
  mutate(class = "non-teaching staffCampagne")
names(non_teaching_staffEvenement) <- make.unique(as.character(non_teaching_staffEvenement[1,]))
non_teaching_staffEvenement <- non_teaching_staffEvenement[-1,] %>%
  mutate(class = "non-teaching staffEvenement")

names(otherCampagne) <- make.unique(as.character(otherCampagne[1,]))
otherCampagne <- otherCampagne[-1,] %>%
  mutate(class = "otherCampagne") 
names(otherEvenement) <- make.unique(as.character(otherEvenement[1,]))
otherEvenement <- otherEvenement[-1,] %>%
  mutate(class = "otherEvenement") 

names(inactive_employeeCampagne) <- make.unique(as.character(inactive_employeeCampagne[1,]))
inactive_employeeCampagne <- inactive_employeeCampagne[-1,] %>%
  mutate(class = "inactive employeeCampagne") 
names(inactive_employeeEvenement) <- make.unique(as.character(inactive_employeeEvenement[1,]))
inactive_employeeEvenement <- inactive_employeeEvenement[-1,] %>%
  mutate(class = "inactive employeeEvenement") 

names(retiredCampagne) <- make.unique(as.character(retiredCampagne[1,]))
retiredCampagne <- retiredCampagne[-1,] %>%
  mutate(class = "retiredCampagne")
names(retiredEvenement) <- make.unique(as.character(retiredEvenement[1,]))
retiredEvenement <- retiredEvenement[-1,] %>%
  mutate(class = "retiredEvenement")

###Rbind le tout###

DataComm <- rbind(GraduateCampagne, GraduateEvenement, studiedCampagne, studiedEvenement,
                  studentCampagne, studentEvenement, friendCampagne, friendEvenement,
                  teaching_staffCampagne, teaching_staffEvenement, non_teaching_staffCampagne,
                  non_teaching_staffEvenement, otherCampagne, otherEvenement,
                  inactive_employeeCampagne, inactive_employeeEvenement, retiredCampagne,
                  retiredEvenement)

saveRDS(DataComm)





