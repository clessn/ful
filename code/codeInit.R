# library 
library(tidyverse)


# loading data and initial cleanning
# Diplome
DataDiplome <- read_csv("/Users/jeremygilbert//Dropbox/Travail/CLESSN/ful/_SharedFolder_fondation-ulaval/data/1 - diplômés.CSV")

# Data Etude 1 group
DataEtude1Group <- read_csv("/Users/jeremygilbert//Dropbox/Travail/CLESSN/ful/_SharedFolder_fondation-ulaval/data/2 - dejaEtudeUL1erGroupe.CSV")

# Data Etude 2 group
DataEtude2Group <- read_csv("/Users/jeremygilbert//Dropbox/Travail/CLESSN/ful/_SharedFolder_fondation-ulaval/data/2 - dejaEtudeUL2eGroupe.CSV")

# Data Etudiant
DataEtudiant <- read_csv("/Users/jeremygilbert//Dropbox/Travail/CLESSN/ful/_SharedFolder_fondation-ulaval/data/3 - Étudiant.CSV")

# Data Ami
DataAmi <- read_csv("/Users/jeremygilbert//Dropbox/Travail/CLESSN/ful/_SharedFolder_fondation-ulaval/data/5 - Ami.CSV")

# Data Enseignant
DataEnseignant <- read_csv("/Users/jeremygilbert//Dropbox/Travail/CLESSN/ful/_SharedFolder_fondation-ulaval/data/6 - PersonnelEnseignant.CSV")

# Data Non-Enseignant
DataNonEnseignant <- read.csv("/Users/jeremygilbert//Dropbox/Travail/CLESSN/ful/_SharedFolder_fondation-ulaval/data/7 - PersonnelNon-enseignant.CSV")

# Data Autre
DataAutre <- read.csv("/Users/jeremygilbert//Dropbox/Travail/CLESSN/ful/_SharedFolder_fondation-ulaval/data/9 - Autre.CSV")

# Data  employe inactif
# DataInact <- read_csv("/Users/jeremygilbert//Dropbox/Travail/CLESSN/ful/_SharedFolder_fondation-ulaval/data/50 - EmployéInactif.CSV")

# Data retraite 
DataRetraite <- read_csv("/Users/jeremygilbert//Dropbox/Travail/CLESSN/ful/_SharedFolder_fondation-ulaval/data/51 - Retraité UL.CSV")