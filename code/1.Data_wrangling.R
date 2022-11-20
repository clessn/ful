
library(tidyverse)

###Get data###

graduate1 <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/1 - diplômés - région 0000.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduate2 <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/1 - diplômés - région 0100.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
graduate3 <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/1 - diplômés - région plus grand 0100.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

studied1 <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/2 - a déjà étudié UL - 6100 et plus.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
studied2 <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/2 - a déjà étudié UL - région 0000.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
studied3 <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/2 - a déjà étudié UL - région 0100 - F.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
studied4 <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/2 - a déjà étudié UL - région 0100 - PAS F.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
studied5 <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/2 - a déjà étudié UL - région 0110 à 0200.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
studied6 <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/2 - a déjà étudié UL - région 0210 à 0500.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
studied7 <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/2 - a déjà étudié UL - région 0600 à 2110.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
studied8 <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/2 - a déjà étudié UL - région 2030 à 5051.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
studied9 <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/2 - a déjà étudié UL - région 5052 à 6090.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

student <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/3 - Étudiant.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

friend <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/5 - Ami.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

teaching_staff <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/6 - Personnel enseignant.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

non_teaching_staff <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/7 - Personnel non-enseignant.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

other <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/9 - Autre.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

inactive_employee <- read.csv("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/50 - Employé inactif.CSV", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

retired <- readxl::read_xlsx("C:/Users/alexa/Dropbox/CLESSN/FUL/_SharedFolder_fondation-ulaval/Data/NewData/51 - Retraité UL.xlsx")


###Add column class, rbind selon class and select variables###

Graduate <- rbind(graduate1, graduate2, graduate3)
names(Graduate) <- make.unique(as.character(Graduate[1,]))
Graduate <- Graduate[-1,] %>% 
  mutate(class = "graduate") 

Studied <- rbind(studied1, studied2, studied3, studied4, studied5, studied6, studied7, studied8, studied9) 
names(Studied) <- make.unique(as.character(Studied[1,]))
Studied <- Studied[-1,] %>% 
  mutate(class = "studied") 

names(student) <- make.unique(as.character(student[1,]))
student <- student[-1,] %>%
  mutate(class = "student") 

names(friend) <- make.unique(as.character(friend[1,]))
friend <- friend[-1,] %>%
  mutate(class = "friend") 


names(teaching_staff) <- make.unique(as.character(teaching_staff[1,]))
teaching_staff <- teaching_staff[-1,] %>%
  mutate(class = "teaching staff") 

names(non_teaching_staff) <- make.unique(as.character(non_teaching_staff[1,]))
non_teaching_staff <- non_teaching_staff[-1,] %>%
  mutate(class = "non-teaching staff")

names(other) <- make.unique(as.character(other[1,]))
other <- other[-1,] %>%
  mutate(class = "other") 

names(inactive_employee) <- make.unique(as.character(inactive_employee[1,]))
inactive_employee <- inactive_employee[-1,] %>%
  mutate(class = "inactive employee") 

retired <- retired %>% mutate(class = "retired")
names(retired) <- names(Graduate)

###Rbind le tout###

Data <- rbind(Graduate, Studied, student, friend, teaching_staff, non_teaching_staff, other, inactive_employee, retired)


saveRDS(Data)
