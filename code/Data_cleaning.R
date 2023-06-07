
##***********###
# Packages ####
##***********###
library(DBI)
library(RSQLite)
library(tidyverse)
##***********###
# Get data from SQLite ####
##***********###
mydb <- dbConnect(RSQLite::SQLite(), "_SharedFolder_fondation-ulaval/Data/ful_bd.db")

Data <- dbGetQuery(mydb, "SELECT
                          UL_NO_CODE,
                          SEX,
                          BIRTHDATE,
                          DT_OF_DEATH,
                          AGE,
                          UL_GENERATION,
                          LANG_CD,
                          UL_ADR_P_H_CITY,
                          UL_ADR_P_H_STATE,
                          UL_ADR_P_H_PAYS,
                          UL_ADR_P_H_CP,
                          UL_ACAD_ORG_01,
                          UL_MAJOR1_CD_01,
                          UL_AV_CLASS_YR_01,
                          UL_EDUCATIO_LVL_PI,
                          UL_ACAD_ORG_PI,
                          UL_AV_CLASS_YR_PI,
                          EMPLOYMENT_DESCR,
                          TITLE_LONG,
                          UL_GRP_EMPL_FUL,
                          UL_GR_EMPLOI_RH,
                          START_DT,
                          END_DT,
                          UL_TYPE_EMPLOI,
                          UL_SRVC_IND_CD_FUL,
                          UL_SRVC_IND_CD_PSO,
                          UL_SRVC_IND_CD_RAI,
                          UL_SRVC_IND_NEG,
                          UL_TYP_COL_FUL_CD,
                          INSTITUTION,
                          UL_DT_DER_DON_1,
                          UL_AMT_DER_DON_1,
                          UL_TYP_DER_DON_1,
                          UL_DT_DER_DON_2,
                          UL_AMT_DER_DON_2,
                          UL_TYP_DER_DON_2,
                          UL_DT_DER_DON_3,
                          UL_AMT_DER_DON_3,
                          UL_TYP_DER_DON_3,
                          UL_DON_ANN_YR_1
                          UL_DON_ANN_AMT_1,
                          UL_DON_ANN_TYP_1,
                          UL_DON_ANN_YR_2,
                          UL_DON_ANN_AMT_2,
                          UL_DON_ANN_TYP_2,
                          UL_DON_ANN_YR_3,
                          UL_DON_ANN_AMT_3,
                          UL_DON_ANN_TYP_3,
                          UL_ANN_DER_DON,
                          UL_MNT_VERSE_VIE,
                          TTL_YR_GIVING
                          UL_CNS_YR_GIVING,
                          UL_DON_PLUS_IMP,
                          UL_PROM_PLUS_IMP,
                          UL_PPI_DT_START,
                          UL_PPI_DT_END,
                          UL_ENG_VIE,
                          UL_ENG_NBR_VIE,
                          UL_ENG_AN_PASSE,
                          UL_ENC_AN_PASSE,
                          UL_ENG_VIE_LIMIT,
                          UL_MNT_PD_OUV,
                          UL_MT_VERSE_PD_OUV,
                          UL_SOLDE_PD_OUV,
                          UL_DT_DEBUT_PD_OUV,
                          UL_DT_FIN_PD_OUV,
                          UL_MNT_V01_PD_OUV,
                          UL_MNT_V02_PD_OUV,
                          UL_MNT_V03_PD_OUV,
                          UL_MNT_V04_PD_OUV,
                          UL_MNT_V05_PD_OUV,
                          UL_MNT_V06_PD_OUV,
                          UL_NBR_PD_OUV,
                          UL_UNLIMIT_FLG,
                          UL_ENG_GC_LVL,
                          UL_DT_PREM_DON,
                          UL_DON_ASS_VIE,
                          UL_DON_PLANIF,
                          UL_RSLT_D_A_SOL,
                          UL_NBR_REFUS,
                          COMPLETED_DT
                            FROM data_all")
#                            ORDER BY RANDOM() LIMIT 50000")

dbDisconnect(mydb)


## CSV of r&o
ro <- read.csv("_SharedFolder_fondation-ulaval/Data/__previous_data/Rouge et Or.CSV") %>% 
  select(UL_NO_CODE, ro_sport = INVLV_CD, ro_startDate = TO_CHAR.B.START_DT..YYYY.MM.DD)


##***********###
# Generate empty CleanData ####
##***********###

CleanData <- data.frame(id = 1:nrow(Data),
                        UL_NO_CODE = Data$UL_NO_CODE)


##***********###
# Function to clean raw FUL numeric vars ####
##***********###

clean_raw_num <- function(raw_data_vector){
  output <- gsub(",", ".", raw_data_vector)
  output[which(substr(output, 1, 1) == ".")] <- paste0("0", output[which(substr(output, 1, 1) == ".")])
  output <- as.numeric(output)
  return(output)
}

minmaxNormalization <- function(x) {
  return((x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)))
}


##***********###
# Cleaning ####
##***********###

# SES ####

#Sexe
table(Data$SEX)
CleanData$ses_female <- NA
CleanData$ses_female[Data$SEX == "F"] <- 1
CleanData$ses_female[Data$SEX == "M"] <- 0
table(CleanData$ses_female)

#Date de décès
#table(Data$DT_OF_DEATH)[1:100]
#CleanData$ses_dead <- NA
#CleanData$ses_dead[Data$DT_OF_DEATH == "NA"] <- 0
#CleanData$ses_dead[Data$DT_OF_DEATH == ] <- 1

#Age
CleanData$ses_age <- clean_raw_num(Data$AGE)
hist(CleanData$ses_age)

table(Data$AGE)[1:45]
CleanData$ses_age34m <- 0
CleanData$ses_age34m[CleanData$ses_age >= 18 & CleanData$ses_age < 35] <- 1
table(CleanData$ses_age34m)

CleanData$ses_age35_54 <- 0
CleanData$ses_age35_54[CleanData$ses_age >= 35 & CleanData$ses_age < 55] <- 1
table(CleanData$ses_age35_54)

CleanData$ses_age55p <- 0
CleanData$ses_age55p[CleanData$ses_age >= 55] <- 1
table(CleanData$ses_age55p)

#langue
table(Data$LANG_CD)[1:10]

#Ville de domicile
#table(Data$UL_ADR_P_H_CITY)[1:100]
#CleanData$ses_ gsub("^.{0,6}", "", df$text_col)



#État/province
table(Data$UL_ADR_P_H_STATE)[1:100]
CleanData$ses_origin_qc <- 0
CleanData$ses_origin_qc[Data$UL_ADR_P_H_STATE == "QC"] <- 1
table(CleanData$ses_origin_qc)

CleanData$ses_origin_roc <- 0 #autres provinces 
CleanData$ses_origin_roc[Data$UL_ADR_P_H_STATE == "AB"] <- 1
CleanData$ses_origin_roc[Data$UL_ADR_P_H_STATE == "MB"] <- 1
CleanData$ses_origin_roc[Data$UL_ADR_P_H_STATE == "ON"] <- 1
CleanData$ses_origin_roc[Data$UL_ADR_P_H_STATE == "BC"] <- 1
CleanData$ses_origin_roc[Data$UL_ADR_P_H_STATE == "PE"] <- 1
CleanData$ses_origin_roc[Data$UL_ADR_P_H_STATE == "NB"] <- 1
CleanData$ses_origin_roc[Data$UL_ADR_P_H_STATE == "NS"] <- 1
CleanData$ses_origin_roc[Data$UL_ADR_P_H_STATE == "NU"] <- 1
CleanData$ses_origin_roc[Data$UL_ADR_P_H_STATE == "SK"] <- 1
CleanData$ses_origin_roc[Data$UL_ADR_P_H_STATE == "NL"] <- 1
CleanData$ses_origin_roc[Data$UL_ADR_P_H_STATE == "NT"] <- 1
CleanData$ses_origin_roc[Data$UL_ADR_P_H_STATE == "YT"] <- 1
table(CleanData$ses_origin_roc)

table(Data$UL_ADR_P_H_PAYS)
CleanData$ses_origin_other <- 0
CleanData$ses_origin_other[Data$UL_ADR_P_H_PAYS != "Canada"] <- 1 #tous les autres pays
table(CleanData$ses_origin_other)

#code postal
table(Data$UL_ADR_P_H_CP)[1:100]
CleanData$ses_postalcode <- NA
CleanData$ses_postalcode <- gsub(" ", "", Data$UL_ADR_P_H_CP)
CleanData$ses_postalcode <- tolower(CleanData$ses_postalcode)
table(CleanData$ses_postalcode)[1:100]



# EDUCATION ####

#Faculté du premier diplome
table(Data$UL_ACAD_ORG_01)
CleanData$education_FirstDegree_Medecine <- 0
CleanData$education_FirstDegree_Medecine[Data$UL_ACAD_ORG_01 == "MED"] <- 1
table(CleanData$education_FirstDegree_Medecine)

CleanData$education_FirstDegree_Admin <- 0
CleanData$education_FirstDegree_Admin[Data$UL_ACAD_ORG_01 == "ADM"] <- 1
table(CleanData$education_FirstDegree_Admin)

CleanData$education_FirstDegree_Music <- 0
CleanData$education_FirstDegree_Music[Data$UL_ACAD_ORG_01 == "MUS"] <- 1
table(CleanData$education_FirstDegree_Music)

CleanData$education_FirstDegree_AgroNutri <- 0
CleanData$education_FirstDegree_AgroNutri[Data$UL_ACAD_ORG_01 == "AGA"] <- 1
table(CleanData$education_FirstDegree_AgroNutri)

CleanData$education_FirstDegree_Pharm <- 0
CleanData$education_FirstDegree_Pharm[Data$UL_ACAD_ORG_01 == "PHA"] <- 1
table(CleanData$education_FirstDegree_Pharm)

CleanData$education_FirstDegree_ArchiArtDesign <- 0
CleanData$education_FirstDegree_ArchiArtDesign[Data$UL_ACAD_ORG_01 == "ARC"] <- 1
table(CleanData$education_FirstDegree_ArchiArtDesign)

CleanData$education_FirstDegree_Droit <- 0
CleanData$education_FirstDegree_Droit[Data$UL_ACAD_ORG_01 == "DRT"] <- 1
table(CleanData$education_FirstDegree_Droit)

CleanData$education_FirstDegree_EtudeSup <- 0
CleanData$education_FirstDegree_EtudeSup[Data$UL_ACAD_ORG_01 == "EGR"] <- 1
table(CleanData$education_FirstDegree_EtudeSup)

CleanData$education_FirstDegree_Education <- 0
CleanData$education_FirstDegree_Education[Data$UL_ACAD_ORG_01 == "EDC"] <- 1
table(CleanData$education_FirstDegree_Education)

CleanData$education_FirstDegree_ForestGeo <- 0
CleanData$education_FirstDegree_ForestGeo[Data$UL_ACAD_ORG_01 == "FFG"] <- 1
table(CleanData$education_FirstDegree_ForestGeo)

CleanData$education_FirstDegree_HautesEtudesInt <- 0
CleanData$education_FirstDegree_HautesEtudesInt[Data$UL_ACAD_ORG_01 == "IQHEI-Inst"] <- 1
table(CleanData$education_FirstDegree_HautesEtudesInt) #aucun résultat

CleanData$education_FirstDegree_LettresScienceHum <- 0
CleanData$education_FirstDegree_LettresScienceHum[Data$UL_ACAD_ORG_01 == "LET"] <- 1
table(CleanData$education_FirstDegree_LettresScienceHum)

CleanData$education_FirstDegree_MedDentaire <- 0
CleanData$education_FirstDegree_MedDentaire[Data$UL_ACAD_ORG_01 == "MDD"] <- 1
table(CleanData$education_FirstDegree_MedDentaire)

CleanData$education_FirstDegree_Philo <- 0
CleanData$education_FirstDegree_Philo[Data$UL_ACAD_ORG_01 == "PHI"] <- 1
table(CleanData$education_FirstDegree_Philo)

CleanData$education_FirstDegree_ScienceGenie <- 0
CleanData$education_FirstDegree_ScienceGenie[Data$UL_ACAD_ORG_01 == "SCI"] <- 1
table(CleanData$education_FirstDegree_ScienceGenie)

CleanData$education_FirstDegree_ScienceInfirmiere <- 0
CleanData$education_FirstDegree_ScienceInfirmiere[Data$UL_ACAD_ORG_01 == "SIN"] <- 1
table(CleanData$education_FirstDegree_ScienceInfirmiere)

CleanData$education_FirstDegree_ScienceSociale <- 0
CleanData$education_FirstDegree_ScienceSociale[Data$UL_ACAD_ORG_01 == "SSC"] <- 1
table(CleanData$education_FirstDegree_ScienceSociale)

CleanData$education_FirstDegree_Theologie <- 0
CleanData$education_FirstDegree_Theologie[Data$UL_ACAD_ORG_01 == "THL"] <- 1
table(CleanData$education_FirstDegree_Theologie)

CleanData$education_FirstDegree_MultidispCollegeUni <- 0
CleanData$education_FirstDegree_MultidispCollegeUni[Data$UL_ACAD_ORG_01 == "UNV"] <- 1
table(CleanData$education_FirstDegree_MultidispCollegeUni)


#Programme du premier diplome
table(Data$UL_MAJOR1_CD_01)




#Année de promotion du premier diplome
table(Data$UL_AV_CLASS_YR_01)
CleanData$education_yearFirstGraduation <- NA
CleanData$education_yearFirstGraduation <- clean_raw_num(Data$UL_AV_CLASS_YR_01)
CleanData$education_yearFirstGraduation[CleanData$education_yearFirstGraduation == 0] <- NA
hist(CleanData$education_yearFirstGraduation)

#Niveau d'étude du diplome le plus important
table(Data$UL_EDUCATIO_LVL_PI)

#Faculté du diplome le plus important
table(Data$UL_ACAD_ORG_PI)

#Année de promotion du diplome le plus important
table(Data$UL_AV_CLASS_YR_PI)
CleanData$education_yearMIGraduation <- NA
CleanData$education_yearMIGraduation <- clean_raw_num(Data$UL_AV_CLASS_YR_PI)
CleanData$education_yearMIGraduation[CleanData$education_yearMIGraduation == 0] <- NA
hist(CleanData$education_yearMIGraduation)

#établissement
table(Data$INSTITUTION)
CleanData$education_ulaval <- 0
CleanData$education_ulaval[Data$INSTITUTION == "LAVAL"] <- 1
table(CleanData$education_ulaval)


# EMPLOI ####
#Description emloyeur
#table(Data$EMPLOYMENT_DESCR)[1:5]
#CleanData$emploi_employedUlaval <- NA
#CleanData$emploi_employedUlaval[Data$EMPLOYMENT_DESCR == "Université Laval - Employé"] <- 1
#table(CleanData$emploi_employedUlaval)

CleanData$emploi_retiredUlaval <- 0
CleanData$emploi_retiredUlaval[Data$EMPLOYMENT_DESCR == "Université Laval - Retraité"] <- 1
table(CleanData$emploi_retiredUlaval)

##Dernier poste occupé à l'ULaval
#professeur
table(Data$TITLE_LONG)
CleanData$emploi_teacherUlaval <- 0
CleanData$emploi_teacherUlaval[which(grepl("^Professeur", Data$TITLE_LONG))] <- 1
CleanData$emploi_teacherUlaval[which(grepl("^Médecin clinicien", Data$TITLE_LONG))] <- 1
CleanData$emploi_teacherUlaval[which(grepl("^Enseignant", Data$TITLE_LONG))] <- 1
CleanData$emploi_teacherUlaval[which(grepl("^Dentiste clinicien", Data$TITLE_LONG))] <- 1
table(CleanData$emploi_teacherUlaval)

#technicien
CleanData$emploi_technicianUlaval <- 0
CleanData$emploi_technicianUlaval[which(grepl("^Technicien", Data$TITLE_LONG))] <- 1
CleanData$emploi_technicianUlaval[which(grepl("^Personnel technique", Data$TITLE_LONG))] <- 1
table(CleanData$emploi_technicianUlaval)

#professionnel
CleanData$emploi_professionalUlaval <- 0
CleanData$emploi_professionalUlaval[which(grepl("^Professionnel", Data$TITLE_LONG))] <- 1
CleanData$emploi_professionalUlaval[which(grepl("^Personnel professionnel", Data$TITLE_LONG))] <- 1
table(CleanData$emploi_professionalUlaval)

#chargé de cours
CleanData$emploi_lecturerUlaval <- 0
CleanData$emploi_lecturerUlaval[which(grepl("^Chargé de cours", Data$TITLE_LONG))] <- 1
CleanData$emploi_lecturerUlaval[which(grepl("^Chargée de cours", Data$TITLE_LONG))] <- 1
CleanData$emploi_lecturerUlaval[which(grepl("^Chargé d'enseignement", Data$TITLE_LONG))] <- 1
CleanData$emploi_lecturerUlaval[which(grepl("^Chargée d'enseignement", Data$TITLE_LONG))] <- 1
CleanData$emploi_lecturerUlaval[which(grepl("^Chargé de sessions", Data$TITLE_LONG))] <- 1
CleanData$emploi_lecturerUlaval[which(grepl("^Chargée de sessions", Data$TITLE_LONG))] <- 1
CleanData$emploi_lecturerUlaval[which(grepl("^Chargé/e cours - forfaitaire", Data$TITLE_LONG))] <- 1
table(CleanData$emploi_lecturerUlaval)

#cadres
CleanData$emploi_executiveUlaval <- 0
CleanData$emploi_executiveUlaval[which(grepl("^Chef", Data$TITLE_LONG))] <- 1
CleanData$emploi_executiveUlaval[which(grepl("^Conseill", Data$TITLE_LONG))] <- 1
CleanData$emploi_executiveUlaval[which(grepl("^Consultant", Data$TITLE_LONG))] <- 1
CleanData$emploi_executiveUlaval[which(grepl("^Coordonnat", Data$TITLE_LONG))] <- 1
CleanData$emploi_executiveUlaval[which(grepl("^Cadre", Data$TITLE_LONG))] <- 1
CleanData$emploi_executiveUlaval[which(grepl("^Administrat", Data$TITLE_LONG))] <- 1
CleanData$emploi_executiveUlaval[which(grepl("^Chargé de responsabilités administratives", Data$TITLE_LONG))] <- 1
CleanData$emploi_executiveUlaval[which(grepl("^Chargée de responsabilités administratives", Data$TITLE_LONG))] <- 1
CleanData$emploi_executiveUlaval[which(grepl("^Direct", Data$TITLE_LONG))] <- 1
CleanData$emploi_executiveUlaval[which(grepl("^vice-doyen", Data$TITLE_LONG))] <- 1
CleanData$emploi_executiveUlaval[which(grepl("^Doyen", Data$TITLE_LONG))] <- 1
CleanData$emploi_executiveUlaval[which(grepl("^Adjointe au vice-recteur/trice exécutif/tive", Data$TITLE_LONG))] <- 1
CleanData$emploi_executiveUlaval[which(grepl("^Adjoint au vice-recteur/trice exécutif/tive", Data$TITLE_LONG))] <- 1
CleanData$emploi_executiveUlaval[which(grepl("^Rect", Data$TITLE_LONG))] <- 1
CleanData$emploi_executiveUlaval[which(grepl("^Responsable", Data$TITLE_LONG))] <- 1
CleanData$emploi_executiveUlaval[which(grepl("^Secrétaire générale", Data$TITLE_LONG))] <- 1
CleanData$emploi_executiveUlaval[which(grepl("^Secrétaire de gestion ", Data$TITLE_LONG))] <- 1
table(CleanData$emploi_executiveUlaval)

#Auxiliaire
CleanData$emploi_assistantUlaval <- 0
CleanData$emploi_assistantUlaval[which(grepl("^Auxiliaire d", Data$TITLE_LONG))] <- 1
table(CleanData$emploi_assistantUlaval)

#personnel administratif
CleanData$emploi_adminStaffUlaval <- 0
CleanData$emploi_adminStaffUlaval[which(grepl("^Auxiliaire administratif", Data$TITLE_LONG))] <- 1
CleanData$emploi_adminStaffUlaval[which(grepl("^Préposée à l'impression", Data$TITLE_LONG))] <- 1
CleanData$emploi_adminStaffUlaval[which(grepl("^Préposé à l'impression", Data$TITLE_LONG))] <- 1
CleanData$emploi_adminStaffUlaval[which(grepl("^Personnel de bureau", Data$TITLE_LONG))] <- 1
CleanData$emploi_adminStaffUlaval[which(grepl("^Commis", Data$TITLE_LONG))] <- 1
CleanData$emploi_adminStaffUlaval[which(grepl("^Apparit", Data$TITLE_LONG))] <- 1
CleanData$emploi_adminStaffUlaval[which(grepl("^Contractuel administratif", Data$TITLE_LONG))] <- 1
table(CleanData$emploi_adminStaffUlaval)

#sport
CleanData$emploi_sportUlaval <- 0
CleanData$emploi_sportUlaval[which(grepl("^AC /", Data$TITLE_LONG))] <- 1
CleanData$emploi_sportUlaval[which(grepl("^Surveillant-sauveteur", Data$TITLE_LONG))] <- 1
CleanData$emploi_sportUlaval[which(grepl("^Monit", Data$TITLE_LONG))] <- 1
table(CleanData$emploi_sportUlaval)

#personnel autre
CleanData$emploi_otherStaffUlaval <- 0
CleanData$emploi_otherStaffUlaval[which(grepl("^Préposée au prêt", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Préposé au prêt", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Préposé à l'entretien", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Préposée à l'entretien", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Préposé aux installations", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Préposée aux installations", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Personnel métiers", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Personnel de soutien", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Personne-ressource", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Magasin", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Jardin", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Format", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Installat", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Chargée de communication", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Chargé de communication", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Bibliothécaire", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Assistante-dentaire", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Assistant-dentaire", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Aide-technique", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Agent", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("Stagiaire", Data$TITLE_LONG))] <- 1
CleanData$emploi_otherStaffUlaval[which(grepl("^Accompagnat", Data$TITLE_LONG))] <- 1
table(CleanData$emploi_otherStaffUlaval)


#Superviseur de stage
CleanData$emploi_internshipSupervisorUlaval <- 0
CleanData$emploi_internshipSupervisorUlaval[which(grepl("^Superviseur de stage", Data$TITLE_LONG))] <- 1
CleanData$emploi_internshipSupervisorUlaval[which(grepl("^Superviseure de stage", Data$TITLE_LONG))] <- 1
table(CleanData$emploi_internshipSupervisorUlaval)

#recherche
CleanData$emploi_researchUlaval <- 0
CleanData$emploi_researchUlaval[which(grepl("Stagiaire postdoctoral (recherche)", Data$TITLE_LONG))] <- 1
CleanData$emploi_researchUlaval[which(grepl("^Développeu", Data$TITLE_LONG))] <- 1
CleanData$emploi_researchUlaval[which(grepl("^Analyste", Data$TITLE_LONG))] <- 1
table(CleanData$emploi_researchUlaval)


#Groupe d'emploi FUL
table(Data$UL_GRP_EMPL_FUL)

#Groupe d'emploi RH
table(Data$UL_GR_EMPLOI_RH)

#Date début expérience travail Ulaval
table(Data$START_DT)[1:100]
CleanData$emploi_startDateWorkUlaval <- NA
CleanData$emploi_startDateWorkUlaval <- as.Date(Data$START_DT)
hist(CleanData$emploi_startDateWorkUlaval,
     breaks = "year")

#Date fin expérience de travail Ulaval
#table(Data$END_DT)[1:10]
#CleanData$emploi_endDateWorkUlaval <- NA
#CleanData$emploi_endDateWorkUlaval <- as.Date(Data$END_DT)
#hist(CleanData$emploi_endDateWorkUlaval,
#     breaks = "month")

#Type d'emploi Ulaval
table(Data$UL_TYPE_EMPLOI)[1:100]


# CONTACT ####

# Nombre de refus pour les communications (SOLMAS, SOLCNV, SUISOL et SOLPER)
unique(Data$UL_NBR_REFUS)[1:10]
table(Data$UL_NBR_REFUS)
CleanData$contact_declineCommunications_number <- clean_raw_num(Data$UL_NBR_REFUS)
hist(CleanData$contact_declineCommunications_number)
table(CleanData$contact_declineCommunications_number)

# Date de la dernière communication
Data$COMPLETED_DT[1:15] 
sum(is.na(Data$COMPLETED_DT))
unique(Data$COMPLETED_DT)[1:10]
table(Data$COMPLETED_DT)[1:100]
CleanData$contact_lastCommunicationDate <- as.Date(Data$COMPLETED_DT)
table(CleanData$contact_lastCommunicationDate)[1:50]
hist(CleanData$contact_lastCommunicationDate,
     breaks = "year")

#Inactivité totale contact
table(Data$UL_SRVC_IND_CD_FUL) 
CleanData$contact_totalInactivity <- 0
CleanData$contact_totalInactivity[Data$UL_SRVC_IND_CD_FUL == "FUL"] <- 1
table(CleanData$contact_totalInactivity) 


#Ne pas solicité temporaire
table(Data$UL_SRVC_IND_CD_PSO) 
CleanData$contact_temporaryNoSolicit <- 0
CleanData$contact_temporaryNoSolicit[Data$UL_SRVC_IND_CD_PSO == "PSO"] <- 1
table(CleanData$contact_temporaryNoSolicit) 

#Ne pas solicité permanent 
table(Data$UL_SRVC_IND_CD_RAI) 
CleanData$contact_permanentNoSolicit <- 0
CleanData$contact_permanentNoSolicit[Data$UL_SRVC_IND_CD_RAI == "RAI"] <- 1
table(CleanData$contact_permanentNoSolicit)

#Service négatif
table(Data$UL_SRVC_IND_NEG) 
CleanData$contact_negativeService <- 0
CleanData$contact_negativeService[Data$UL_SRVC_IND_NEG == "O"] <- 1
table(CleanData$contact_negativeService)

#Type de collaborateur FUL
table(Data$UL_TYP_COL_FUL_CD) 




# HISTORIQUE ####

#Date du dernier don
Data$UL_DT_DER_DON_1[1:15] 
sum(is.na(Data$UL_DT_DER_DON_1))
unique(Data$UL_DT_DER_DON_1)[1:10]
table(Data$UL_DT_DER_DON_1)[1:100]
CleanData$historic_lastDonation_date <- as.Date(Data$UL_DT_DER_DON_1)
table(CleanData$historic_lastDonation_date)[1:50]

#Date de l'avant-dernier don
unique(Data$UL_DT_DER_DON_2)[1:10]
table(Data$UL_DT_DER_DON_2)[1:100]
CleanData$historic_penulDonation_date <- as.Date(Data$UL_DT_DER_DON_2)
table(CleanData$historic_penulDonation_date)[1:50]

# Montant totale de l'encaissement de l'an passé
unique(Data$UL_ENC_AN_PASSE)[1:10]
table(Data$UL_ENC_AN_PASSE)[1:100]
CleanData$historic_CollectedLastYear_totalAmount <- clean_raw_num(Data$UL_ENC_AN_PASSE)
table(CleanData$historic_CollectedLastYear_totalAmount)[1:50]
hist(CleanData$historic_CollectedLastYear_totalAmount)

# Montant du dernier don
unique(Data$UL_AMT_DER_DON_1)[1:10]
table(Data$UL_AMT_DER_DON_1)[1:100]
CleanData$historic_lastDonation_amount <- clean_raw_num(Data$UL_AMT_DER_DON_1)
table(CleanData$historic_lastDonation_amount)[1:50]
hist(CleanData$historic_lastDonation_amount)


# Niveau cercle donateur FUL
## https://www.ulaval.ca/fondation/decouvrir/reconnaissance/paliers-de-dons/
Data$UL_ENG_GC_LVL[1:15] 
sum(is.na(Data$UL_ENG_GC_LVL))
unique(Data$UL_ENG_GC_LVL)[1:10]
table(Data$UL_ENG_GC_LVL)

#### Gouverneur
table(Data$UL_ENG_GC_LVL)
CleanData$historic_donationLevel_gouverneur <- 0
CleanData$historic_donationLevel_gouverneur[Data$UL_ENG_GC_LVL == "GOUVERNEUR"] <- 1
table(CleanData$historic_donationLevel_gouverneur)

#### Commandeur
table(Data$UL_ENG_GC_LVL)
CleanData$historic_donationLevel_commandeur <- 0
CleanData$historic_donationLevel_commandeur[Data$UL_ENG_GC_LVL == "COMMANDEUR"] <- 1
table(CleanData$historic_donationLevel_commandeur)

#### Cercle rectrice - Membre
table(Data$UL_ENG_GC_LVL)
CleanData$historic_donationLevel_recMember <- 0
CleanData$historic_donationLevel_recMember[Data$UL_ENG_GC_LVL == "RECMEMBRE"] <- 1
table(CleanData$historic_donationLevel_recMember)

#### Cercle rectrice - Chevalier
table(Data$UL_ENG_GC_LVL)
CleanData$historic_donationLevel_recChevalier <- 0
CleanData$historic_donationLevel_recChevalier[Data$UL_ENG_GC_LVL == "RECCHEVAL"] <- 1
table(CleanData$historic_donationLevel_recChevalier)

#### Cercle rectrice - Grand Chevalier
table(Data$UL_ENG_GC_LVL)
CleanData$historic_donationLevel_recGrandChevalier <- 0
CleanData$historic_donationLevel_recGrandChevalier[Data$UL_ENG_GC_LVL == "RECGRCHEV"] <- 1
table(CleanData$historic_donationLevel_recGrandChevalier)

#### Cercle rectrice - Officier
table(Data$UL_ENG_GC_LVL)
CleanData$historic_donationLevel_recOfficier <- 0
CleanData$historic_donationLevel_recOfficier[Data$UL_ENG_GC_LVL == "RECOFFICI"] <- 1
table(CleanData$historic_donationLevel_recOfficier)

#### Cercle rectrice - Grand Officier
table(Data$UL_ENG_GC_LVL)
CleanData$historic_donationLevel_recGrandOfficier <- 0
CleanData$historic_donationLevel_recGrandOfficier[Data$UL_ENG_GC_LVL == "RECGROFFI"] <- 1
table(CleanData$historic_donationLevel_recGrandOfficier)

#### Membre du Cercle de Monseigneur de Laval
table(Data$UL_ENG_GC_LVL)
CleanData$historic_donationLevel_msgnrLaval <- 0
CleanData$historic_donationLevel_msgnrLaval[Data$UL_ENG_GC_LVL == "MSGNRLAVAL"] <- 1
table(CleanData$historic_donationLevel_msgnrLaval)

#### Variable ordinale
table(Data$UL_ENG_GC_LVL)
CleanData$historic_donationLevel <- 0 # No donation level: 0
CleanData$historic_donationLevel[CleanData$historic_donationLevel_gouverneur == 1] <- 0.125
CleanData$historic_donationLevel[CleanData$historic_donationLevel_commandeur == 1] <- 0.25
CleanData$historic_donationLevel[CleanData$historic_donationLevel_recMember == 1] <- 0.375
CleanData$historic_donationLevel[CleanData$historic_donationLevel_recMember == 1] <- 0.375
CleanData$historic_donationLevel[CleanData$historic_donationLevel_recChevalier == 1] <- 0.5
CleanData$historic_donationLevel[CleanData$historic_donationLevel_recGrandChevalier == 1] <- 0.625
CleanData$historic_donationLevel[CleanData$historic_donationLevel_recOfficier == 1] <- 0.75
CleanData$historic_donationLevel[CleanData$historic_donationLevel_recGrandOfficier == 1] <- 0.875
CleanData$historic_donationLevel[CleanData$historic_donationLevel_msgnrLaval == 1] <- 1
table(CleanData$historic_donationLevel)

# Date du premier don
Data$UL_DT_PREM_DON[1:15] 
sum(is.na(Data$UL_DT_PREM_DON))
unique(Data$UL_DT_PREM_DON)[1:10]
table(Data$UL_DT_PREM_DON)[1:100]
CleanData$historic_dateFirstDonation <- as.Date(Data$UL_DT_PREM_DON)
table(CleanData$historic_dateFirstDonation)[1:50]
hist(CleanData$historic_dateFirstDonation,
     breaks = "year")

# Don par assurance vie (Y/N)
Data$UL_DON_ASS_VIE[1:15] 
sum(is.na(Data$UL_DON_ASS_VIE))
unique(Data$UL_DON_ASS_VIE)[1:10]
table(Data$UL_DON_ASS_VIE)
CleanData$historic_lifeInsuranceDonation <- NA
CleanData$historic_lifeInsuranceDonation[Data$UL_DON_ASS_VIE==""] <- 0
CleanData$historic_lifeInsuranceDonation[Data$UL_DON_ASS_VIE=="OUI"] <- 1
table(CleanData$historic_lifeInsuranceDonation)
sum(is.na(CleanData$historic_lifeInsuranceDonation))

# Montant des dons annuels (dernière année)
unique(Data$UL_DON_ANN_AMT_1)[1:10]
table(Data$UL_DON_ANN_AMT_1)[1:100]
CleanData$historic_donationsLastYear_totalAmount <- clean_raw_num(Data$UL_DON_ANN_AMT_1)
table(CleanData$historic_donationsLastYear_totalAmount)[1:50]

#Type de paiement des dons annuels (dernière année)
table(Data$UL_DON_ANN_TYP_1)

#Type Action
table(Data$UL_DON_ANN_TYP_1)
CleanData$historic_donationsLastYear_paymentType_action <- 0
CleanData$historic_donationsLastYear_paymentType_action[Data$UL_DON_ANN_TYP_1 == "ACTION"] <- 1
table(CleanData$historic_donationsLastYear_paymentType_action)

#Type Bien
table(Data$UL_DON_ANN_TYP_1)
CleanData$historic_donationsLastYear_paymentType_bien <- 0
CleanData$historic_donationsLastYear_paymentType_bien[Data$UL_DON_ANN_TYP_1 == "BIEN"] <- 1
table(CleanData$historic_donationsLastYear_paymentType_bien)

#Type CC
table(Data$UL_DON_ANN_TYP_1)
CleanData$historic_donationsLastYear_paymentType_CC <- 0
CleanData$historic_donationsLastYear_paymentType_CC[Data$UL_DON_ANN_TYP_1 == "CC"] <- 1
table(CleanData$historic_donationsLastYear_paymentType_CC)

#Type CCFAC
table(Data$UL_DON_ANN_TYP_1)
CleanData$historic_donationsLastYear_paymentType_CCFAC <- 0
CleanData$historic_donationsLastYear_paymentType_CCFAC[Data$UL_DON_ANN_TYP_1 == "CCFAC"] <- 1
table(CleanData$historic_donationsLastYear_paymentType_CCFAC)

#Type CCLO
table(Data$UL_DON_ANN_TYP_1)
CleanData$historic_donationsLastYear_paymentType_CCLO <- 0
CleanData$historic_donationsLastYear_paymentType_CCLO[Data$UL_DON_ANN_TYP_1 == "CCLO"] <- 1
table(CleanData$historic_donationsLastYear_paymentType_CCLO)

#Type CH
table(Data$UL_DON_ANN_TYP_1)
CleanData$historic_donationsLastYear_paymentType_CH <- 0
CleanData$historic_donationsLastYear_paymentType_CH[Data$UL_DON_ANN_TYP_1 == "CH"] <- 1
table(CleanData$historic_donationsLastYear_paymentType_CH)

#Type cheque
table(Data$UL_DON_ANN_TYP_1)
CleanData$historic_donationsLastYear_paymentType_cheque <- 0
CleanData$historic_donationsLastYear_paymentType_cheque[Data$UL_DON_ANN_TYP_1 == "CHEQUE"] <- 1
table(CleanData$historic_donationsLastYear_paymentType_cheque)

#Type CO
table(Data$UL_DON_ANN_TYP_1)
CleanData$historic_donationsLastYear_paymentType_CO <- 0
CleanData$historic_donationsLastYear_paymentType_CO[Data$UL_DON_ANN_TYP_1 == "CO"] <- 1
table(CleanData$historic_donationsLastYear_paymentType_CO)

#Type mposte
table(Data$UL_DON_ANN_TYP_1)
CleanData$historic_donationsLastYear_paymentType_mposte <- 0
CleanData$historic_donationsLastYear_paymentType_mposte[Data$UL_DON_ANN_TYP_1 == "MPOSTE"] <- 1
table(CleanData$historic_donationsLastYear_paymentType_mposte)

#Type PB
table(Data$UL_DON_ANN_TYP_1)
CleanData$historic_donationsLastYear_paymentType_PB <- 0
CleanData$historic_donationsLastYear_paymentType_PB[Data$UL_DON_ANN_TYP_1 == "PB"] <- 1
table(CleanData$historic_donationsLastYear_paymentType_PB)

#Type RR
table(Data$UL_DON_ANN_TYP_1)
CleanData$historic_donationsLastYear_paymentType_RR <- 0
CleanData$historic_donationsLastYear_paymentType_RR[Data$UL_DON_ANN_TYP_1 == "RR"] <- 1
table(CleanData$historic_donationsLastYear_paymentType_RR)

#Type RS
table(Data$UL_DON_ANN_TYP_1)
CleanData$historic_donationsLastYear_paymentType_RS <- 0
CleanData$historic_donationsLastYear_paymentType_RS[Data$UL_DON_ANN_TYP_1 == "RS"] <- 1
table(CleanData$historic_donationsLastYear_paymentType_RS)

#Type RU
table(Data$UL_DON_ANN_TYP_1)
CleanData$historic_donationsLastYear_paymentType_RU <- 0
CleanData$historic_donationsLastYear_paymentType_RU[Data$UL_DON_ANN_TYP_1 == "RU"] <- 1
table(CleanData$historic_donationsLastYear_paymentType_RU)

#Type VB
table(Data$UL_DON_ANN_TYP_1)
CleanData$historic_donationsLastYear_paymentType_VB <- 0
CleanData$historic_donationsLastYear_paymentType_VB[Data$UL_DON_ANN_TYP_1 == "VB"] <- 1
table(CleanData$historic_donationsLastYear_paymentType_VB)

#Type TRBANC
table(Data$UL_TYP_DER_DON_3)
CleanData$historic_second2LastDonation_type_trbanc <- 0
CleanData$historic_second2LastDonation_type_trbanc[Data$UL_TYP_DER_DON_3 == "TRBANC"] <- 1
table(CleanData$historic_second2LastDonation_type_trbanc)

# Avant-dernière année de dons annuels
unique(Data$UL_DON_ANN_YR_2)[1:10]
table(Data$UL_DON_ANN_YR_2)[1:100]
CleanData$historic_donationsPenulYear <- clean_raw_num(Data$UL_DON_ANN_YR_2)
table(CleanData$historic_donationsPenulYear)[1:50]

#Montant des dons annuels (avant-dernière année)
unique(Data$UL_DON_ANN_AMT_2)[1:10]
table(Data$UL_DON_ANN_AMT_2)[1:100]
CleanData$historic_donationsPenulYear_totalAmount <- clean_raw_num(Data$UL_DON_ANN_AMT_2)
table(CleanData$historic_donationsPenulYear_totalAmount)[1:50]

#Type de paiement des dons annuels (avant-dernière année)
table(Data$UL_DON_ANN_TYP_2)

#Type Action
table(Data$UL_DON_ANN_TYP_2)
CleanData$historic_donationsPenulYear_paymentType_action <- 0
CleanData$historic_donationsPenulYear_paymentType_action[Data$UL_DON_ANN_TYP_2 == "ACTION"] <- 1
table(CleanData$historic_donationsPenulYear_paymentType_action)

#Type Bien
table(Data$UL_DON_ANN_TYP_2)
CleanData$historic_donationsPenulYear_paymentType_bien <- 0
CleanData$historic_donationsPenulYear_paymentType_bien[Data$UL_DON_ANN_TYP_2 == "BIEN"] <- 1
table(CleanData$historic_donationsPenulYear_paymentType_bien)

#Type CC
table(Data$UL_DON_ANN_TYP_2)
CleanData$historic_donationsPenulYear_paymentType_CC <- 0
CleanData$historic_donationsPenulYear_paymentType_CC[Data$UL_DON_ANN_TYP_2 == "CC"] <- 1
table(CleanData$historic_donationsPenulYear_paymentType_CC)

#Type CCLO
table(Data$UL_DON_ANN_TYP_2)
CleanData$historic_donationsPenulYear_paymentType_CCLO <- 0
CleanData$historic_donationsPenulYear_paymentType_CCLO[Data$UL_DON_ANN_TYP_2 == "CCLO"] <- 1
table(CleanData$historic_donationsPenulYear_paymentType_CCLO)

#Type CH
table(Data$UL_DON_ANN_TYP_2)
CleanData$historic_donationsPenulYear_paymentType_CH <- 0
CleanData$historic_donationsPenulYear_paymentType_CH[Data$UL_DON_ANN_TYP_2 == "CH"] <- 1
table(CleanData$historic_donationsPenulYear_paymentType_CH)

#Type CO
table(Data$UL_DON_ANN_TYP_2)
CleanData$historic_donationsPenulYear_paymentType_CO <- 0
CleanData$historic_donationsPenulYear_paymentType_CO[Data$UL_DON_ANN_TYP_2 == "CO"] <- 1
table(CleanData$historic_donationsPenulYear_paymentType_CO)

#Type mposte
table(Data$UL_DON_ANN_TYP_2)
CleanData$historic_donationsPenulYear_paymentType_mposte <- 0
CleanData$historic_donationsPenulYear_paymentType_mposte[Data$UL_DON_ANN_TYP_2 == "MPOSTE"] <- 1
table(CleanData$historic_donationsPenulYear_paymentType_mposte)

#Type PB
table(Data$UL_DON_ANN_TYP_2)
CleanData$historic_donationsPenulYear_paymentType_PB <- 0
CleanData$historic_donationsPenulYear_paymentType_PB[Data$UL_DON_ANN_TYP_2 == "PB"] <- 1
table(CleanData$historic_donationsPenulYear_paymentType_PB)

#Type RR
table(Data$UL_DON_ANN_TYP_2)
CleanData$historic_donationsPenulYear_paymentType_RR <- 0
CleanData$historic_donationsPenulYear_paymentType_RR[Data$UL_DON_ANN_TYP_2 == "RR"] <- 1
table(CleanData$historic_donationsPenulYear_paymentType_RR)

#Type RS
table(Data$UL_DON_ANN_TYP_2)
CleanData$historic_donationsPenulYear_paymentType_RS <- 0
CleanData$historic_donationsPenulYear_paymentType_RS[Data$UL_DON_ANN_TYP_2 == "RS"] <- 1
table(CleanData$historic_donationsPenulYear_paymentType_RS)

#Type RU
table(Data$UL_DON_ANN_TYP_2)
CleanData$historic_donationsPenulYear_paymentType_RU <- 0
CleanData$historic_donationsPenulYear_paymentType_RU[Data$UL_DON_ANN_TYP_2 == "RU"] <- 1
table(CleanData$historic_donationsPenulYear_paymentType_RU)

#Type VB
table(Data$UL_DON_ANN_TYP_2)
CleanData$historic_donationsPenulYear_paymentType_VB <- 0
CleanData$historic_donationsPenulYear_paymentType_VB[Data$UL_DON_ANN_TYP_2 == "VB"] <- 1
table(CleanData$historic_donationsPenulYear_paymentType_VB)

#Avant-avant-dernière année de dons annuels
unique(Data$UL_DON_ANN_YR_3)[1:10]
table(Data$UL_DON_ANN_YR_3)[1:100]
CleanData$historic_donationsSecond2LastYear <- clean_raw_num(Data$UL_DON_ANN_YR_3)
table(CleanData$historic_donationsSecond2LastYear)[1:50]

#Montant des dons annuels (avant-avant-dernière année)
unique(Data$UL_DON_ANN_AMT_3)[1:10]
table(Data$UL_DON_ANN_AMT_3)[1:100]
CleanData$historic_donationsSecond2LastYear_totalAmount <- clean_raw_num(Data$UL_DON_ANN_AMT_3)
table(CleanData$historic_donationsSecond2LastYear_totalAmount)[1:50]

#Type de paiement des dons annuels (avant-avant-dernière année)
table(Data$UL_DON_ANN_TYP_3)

#Type Action
table(Data$UL_DON_ANN_TYP_3)
CleanData$historic_donationsSecond2LastYear_paymentType_action <- 0
CleanData$historic_donationsSecond2LastYear_paymentType_action[Data$UL_DON_ANN_TYP_3 == "ACTION"] <- 1
table(CleanData$historic_donationsSecond2LastYear_paymentType_action)

#Type Bien
table(Data$UL_DON_ANN_TYP_3)
CleanData$historic_donationsSecond2LastYear_paymentType_bien <- 0
CleanData$historic_donationsSecond2LastYear_paymentType_bien[Data$UL_DON_ANN_TYP_3 == "BIEN"] <- 1
table(CleanData$historic_donationsSecond2LastYear_paymentType_bien)

#Type CC
table(Data$UL_DON_ANN_TYP_3)
CleanData$historic_donationsSecond2LastYear_paymentType_CC <- 0
CleanData$historic_donationsSecond2LastYear_paymentType_CC[Data$UL_DON_ANN_TYP_3 == "CC"] <- 1
table(CleanData$historic_donationsSecond2LastYear_paymentType_CC)

#Type CCLO
table(Data$UL_DON_ANN_TYP_3)
CleanData$historic_donationsSecond2LastYear_paymentType_CCLO <- 0
CleanData$historic_donationsSecond2LastYear_paymentType_CCLO[Data$UL_DON_ANN_TYP_3 == "CCLO"] <- 1
table(CleanData$historic_donationsSecond2LastYear_paymentType_CCLO)

#Type CH
table(Data$UL_DON_ANN_TYP_3)
CleanData$historic_donationsSecond2LastYear_paymentType_CH <- 0
CleanData$historic_donationsSecond2LastYear_paymentType_CH[Data$UL_DON_ANN_TYP_3 == "CH"] <- 1
table(CleanData$historic_donationsSecond2LastYear_paymentType_CH)

#Type CO
table(Data$UL_DON_ANN_TYP_3)
CleanData$historic_donationsSecond2LastYear_paymentType_CO <- 0
CleanData$historic_donationsSecond2LastYear_paymentType_CO[Data$UL_DON_ANN_TYP_3 == "CO"] <- 1
table(CleanData$historic_donationsSecond2LastYear_paymentType_CO)

#Type mposte
table(Data$UL_DON_ANN_TYP_3)
CleanData$historic_donationsSecond2LastYear_paymentType_mposte <- 0
CleanData$historic_donationsSecond2LastYear_paymentType_mposte[Data$UL_DON_ANN_TYP_3 == "MPOSTE"] <- 1
table(CleanData$historic_donationsSecond2LastYear_paymentType_mposte)

#Type TO
table(Data$UL_DON_ANN_TYP_3)
CleanData$historic_donationsSecond2LastYear_paymentType_TO <- 0
CleanData$historic_donationsSecond2LastYear_paymentType_TO[Data$UL_DON_ANN_TYP_3 == "TO"] <- 1
table(CleanData$historic_donationsSecond2LastYear_paymentType_TO)

#Type RR
table(Data$UL_DON_ANN_TYP_3)
CleanData$historic_donationsSecond2LastYear_paymentType_RR <- 0
CleanData$historic_donationsSecond2LastYear_paymentType_RR[Data$UL_DON_ANN_TYP_3 == "RR"] <- 1
table(CleanData$historic_donationsSecond2LastYear_paymentType_RR)

#Type RS
table(Data$UL_DON_ANN_TYP_3)
CleanData$historic_donationsSecond2LastYear_paymentType_RS <- 0
CleanData$historic_donationsSecond2LastYear_paymentType_RS[Data$UL_DON_ANN_TYP_3 == "RS"] <- 1
table(CleanData$historic_donationsSecond2LastYear_paymentType_RS)

#Type RU
table(Data$UL_DON_ANN_TYP_3)
CleanData$historic_donationsSecond2LastYear_paymentType_RU <- 0
CleanData$historic_donationsSecond2LastYear_paymentType_RU[Data$UL_DON_ANN_TYP_3 == "RU"] <- 1
table(CleanData$historic_donationsSecond2LastYear_paymentType_RU)

#Type VB
table(Data$UL_DON_ANN_TYP_3)
CleanData$historic_donationsSecond2LastYear_paymentType_VB <- 0
CleanData$historic_donationsSecond2LastYear_paymentType_VB[Data$UL_DON_ANN_TYP_3 == "VB"] <- 1
table(CleanData$historic_donationsSecond2LastYear_paymentType_VB)

#Nombre d'années où des dons ont été effectués
unique(Data$TTL_YR_GIVING)[1:10]
table(Data$TTL_YR_GIVING)[1:100]
CleanData$historic_donations_numberYears <- clean_raw_num(Data$TTL_YR_GIVING)
table(CleanData$historic_donations_numberYears)

#historic_pledgesLife_highestAmount_startDate
Data$UL_PPI_DT_START[1:15] 
sum(is.na(Data$UL_PPI_DT_START))
unique(Data$UL_PPI_DT_START)[1:10]
table(Data$UL_PPI_DT_START)[1:100]
CleanData$historic_pledgesLife_highestAmount_startDate <- as.Date(Data$UL_PPI_DT_START)
table(CleanData$historic_pledgesLife_highestAmount_startDate)[1:50]

#historic_pledgesLife_highestAmount_endDate
Data$UL_PPI_DT_END[1:15] 
sum(is.na(Data$UL_PPI_DT_END))
unique(Data$UL_PPI_DT_END)[1:10]
table(Data$UL_PPI_DT_END)[1:100]
CleanData$historic_pledgesLife_highestAmount_startEND <- as.Date(Data$UL_PPI_DT_END)
table(CleanData$historic_pledgesLife_highestAmount_startEND)[1:50]

#Année du dernier don
table(Data$UL_ANN_DER_DON)
CleanData$historic_YearLastDonation <- clean_raw_num(Data$UL_ANN_DER_DON)
table(CleanData$historic_YearLastDonation)

#Montant des dons/paiements versés à vie
table(Data$UL_MNT_VERSE_VIE)[1:50]
CleanData$historic_donationsLife_totalAmount <- clean_raw_num(Data$UL_MNT_VERSE_VIE)
table(CleanData$historic_donationsLife_totalAmount)

#Nombre d'années consécutives où des dons ont été effectués à partir de l'année courante.
unique(Data$UL_CNS_YR_GIVING)[1:10]
table(Data$UL_CNS_YR_GIVING)[1:100]
CleanData$historic_donationsConsecutiveYears <- clean_raw_num(Data$UL_CNS_YR_GIVING)
table(CleanData$historic_donationsConsecutiveYears)

#Montant du don le plus important
unique(Data$UL_DON_PLUS_IMP)[1:10]
table(Data$UL_DON_PLUS_IMP)[1:100]
CleanData$historic_donationsLife_highestAmount <- clean_raw_num(Data$UL_DON_PLUS_IMP)
table(CleanData$historic_donationsLife_highestAmount)

#Montant de la promesse de don la plus importante
unique(Data$UL_PROM_PLUS_IMP)[1:10]
table(Data$UL_PROM_PLUS_IMP)[1:100]
CleanData$historic_pledgesLife_highestAmount <- clean_raw_num(Data$UL_PROM_PLUS_IMP)
table(CleanData$historic_pledgesLife_highestAmount)

#Montant total des engagements à vie
unique(Data$UL_ENG_VIE)[1:10]
table(Data$UL_ENG_VIE)[1:100]
CleanData$historic_commitmentLife_totalAmount <- clean_raw_num(Data$UL_ENG_VIE)
table(CleanData$historic_commitmentLife_totalAmount)

#Le nombre d'engagements à vie
unique(Data$UL_ENG_NBR_VIE)[1:10]
table(Data$UL_ENG_NBR_VIE)[1:100]
CleanData$historic_commitmentLife_number <- clean_raw_num(Data$UL_ENG_NBR_VIE)
table(CleanData$historic_commitmentLife_number)

#Montant totale de l'engagement de l'an passé
unique(Data$UL_ENG_AN_PASSE)[1:10]
table(Data$UL_ENG_AN_PASSE)[1:100]
CleanData$historic_commitmentLastYear_totalAmount <- clean_raw_num(Data$UL_ENG_AN_PASSE)
table(CleanData$historic_commitmentLastYear_totalAmount)

# PROSPECTIF ####

# Montant total des engagements à vie limité
Data$UL_ENG_VIE_LIMIT[1:15] 
sum(is.na(Data$UL_ENG_VIE_LIMIT))
unique(Data$UL_ENG_VIE_LIMIT)[1:10]
table(Data$UL_ENG_VIE_LIMIT)[1:100]
CleanData$prospectif_CommittedLifeLimited_totalAmount <- clean_raw_num(Data$UL_ENG_VIE_LIMIT)
table(CleanData$prospectif_CommittedLifeLimited_totalAmount)[1:50]
hist(CleanData$prospectif_CommittedLifeLimited_totalAmount)

# Montant total des promesses de don ouvertes
Data$UL_MNT_PD_OUV[1:15] 
sum(is.na(Data$UL_MNT_PD_OUV))
unique(Data$UL_MNT_PD_OUV)[1:10]
table(Data$UL_MNT_PD_OUV)
CleanData$prospectif_openPledges_totalAmount <- clean_raw_num(Data$UL_MNT_PD_OUV)
table(CleanData$prospectif_openPledges_totalAmount)[1:50]
hist(CleanData$prospectif_openPledges_totalAmount)

# Montant versé des promesses de don ouvertes
Data$UL_MT_VERSE_PD_OUV[1:15] 
sum(is.na(Data$UL_MT_VERSE_PD_OUV))
unique(Data$UL_MT_VERSE_PD_OUV)[1:10]
table(Data$UL_MT_VERSE_PD_OUV)
CleanData$prospectif_openPledges_paid <- clean_raw_num(Data$UL_MT_VERSE_PD_OUV)
table(CleanData$prospectif_openPledges_paid)[1:50]
hist(CleanData$prospectif_openPledges_paid)

# Solde des promesses de don ouvertes
Data$UL_SOLDE_PD_OUV[1:15] 
sum(is.na(Data$UL_SOLDE_PD_OUV))
unique(Data$UL_SOLDE_PD_OUV)[1:10]
table(Data$UL_SOLDE_PD_OUV)[1:100]
CleanData$prospectif_openPledges_balance <- clean_raw_num(Data$UL_SOLDE_PD_OUV)
table(CleanData$prospectif_openPledges_balance)[1:50]
hist(CleanData$prospectif_openPledges_balance)

# Date de début des promesses de don ouvertes
Data$UL_DT_DEBUT_PD_OUV[1:15] 
sum(is.na(Data$UL_DT_DEBUT_PD_OUV))
unique(Data$UL_DT_DEBUT_PD_OUV)[1:10]
table(Data$UL_DT_DEBUT_PD_OUV)[1:100]
CleanData$prospectif_openPledges_startDate <- as.Date(Data$UL_DT_DEBUT_PD_OUV)
table(CleanData$prospectif_openPledges_startDate)[1:50]
hist(CleanData$prospectif_openPledges_startDate,
     breaks = "month")

# Date de fin des promesses de don ouvertes
Data$UL_DT_FIN_PD_OUV[1:15] 
sum(is.na(Data$UL_DT_FIN_PD_OUV))
unique(Data$UL_DT_FIN_PD_OUV)[1:10]
table(Data$UL_DT_FIN_PD_OUV)[1:100]
CleanData$prospectif_openPledges_endDate <- as.Date(Data$UL_DT_FIN_PD_OUV)
table(CleanData$prospectif_openPledges_endDate)[1:50]
hist(CleanData$prospectif_openPledges_endDate,
     breaks = "month")

# Montant attendu pour l'année courante des pormesse ouvertes
Data$UL_MNT_V01_PD_OUV[1:15] 
sum(is.na(Data$UL_MNT_V01_PD_OUV))
unique(Data$UL_MNT_V01_PD_OUV)[1:10]
table(Data$UL_MNT_V01_PD_OUV)[1:100]
CleanData$prospectif_openPledges_xAmount2022 <- clean_raw_num(Data$UL_MNT_V01_PD_OUV)
table(CleanData$prospectif_openPledges_xAmount2022)[1:50]
hist(CleanData$prospectif_openPledges_xAmount2022)

# Montant attendu pour l'année courante + 1 des pormesse ouvertes
Data$UL_MNT_V02_PD_OUV[1:15] 
sum(is.na(Data$UL_MNT_V02_PD_OUV))
unique(Data$UL_MNT_V02_PD_OUV)[1:10]
table(Data$UL_MNT_V02_PD_OUV)[1:100]
CleanData$prospectif_openPledges_xAmount2023 <- clean_raw_num(Data$UL_MNT_V02_PD_OUV)
table(CleanData$prospectif_openPledges_xAmount2023)[1:50]
hist(CleanData$prospectif_openPledges_xAmount2023)

# Montant attendu pour l'année courante + 2 des pormesse ouvertes
Data$UL_MNT_V03_PD_OUV[1:15] 
sum(is.na(Data$UL_MNT_V03_PD_OUV))
unique(Data$UL_MNT_V03_PD_OUV)[1:10]
table(Data$UL_MNT_V03_PD_OUV)[1:100]
CleanData$prospectif_openPledges_xAmount2024 <- clean_raw_num(Data$UL_MNT_V03_PD_OUV)
table(CleanData$prospectif_openPledges_xAmount2024)[1:50]
hist(CleanData$prospectif_openPledges_xAmount2024)

# Montant attendu pour l'année courante + 3 des pormesse ouvertes
Data$UL_MNT_V04_PD_OUV[1:15] 
sum(is.na(Data$UL_MNT_V04_PD_OUV))
unique(Data$UL_MNT_V04_PD_OUV)[1:10]
table(Data$UL_MNT_V04_PD_OUV)[1:100]
CleanData$prospectif_openPledges_xAmount2025 <- clean_raw_num(Data$UL_MNT_V04_PD_OUV)
table(CleanData$prospectif_openPledges_xAmount2025)[1:50]
hist(CleanData$prospectif_openPledges_xAmount2025)

# Montant attendu pour l'année courante + 4 des pormesse ouvertes
Data$UL_MNT_V05_PD_OUV[1:15] 
sum(is.na(Data$UL_MNT_V05_PD_OUV))
unique(Data$UL_MNT_V05_PD_OUV)[1:10]
table(Data$UL_MNT_V05_PD_OUV)[1:100]
CleanData$prospectif_openPledges_xAmount2026 <- clean_raw_num(Data$UL_MNT_V05_PD_OUV)
table(CleanData$prospectif_openPledges_xAmount2026)[1:50]
sum(is.na(CleanData$prospectif_openPledges_xAmount2026))
hist(CleanData$prospectif_openPledges_xAmount2026)

# Montant attendu pour l'année courante + 5 des pormesse ouvertes
Data$UL_MNT_V06_PD_OUV[1:15] 
sum(is.na(Data$UL_MNT_V06_PD_OUV))
unique(Data$UL_MNT_V06_PD_OUV)[1:10]
table(Data$UL_MNT_V06_PD_OUV)[1:100]
CleanData$prospectif_openPledges_xAmount2027 <- clean_raw_num(Data$UL_MNT_V06_PD_OUV)
table(CleanData$prospectif_openPledges_xAmount2027)[1:50]
sum(is.na(CleanData$prospectif_openPledges_xAmount2027))
hist(CleanData$prospectif_openPledges_xAmount2027)

# Nombre de promesses de don ouvertes
Data$UL_NBR_PD_OUV[1:15] 
sum(is.na(Data$UL_NBR_PD_OUV))
unique(Data$UL_NBR_PD_OUV)[1:10]
table(Data$UL_NBR_PD_OUV)
CleanData$prospectif_openPledges_number <- clean_raw_num(Data$UL_NBR_PD_OUV)
table(CleanData$prospectif_openPledges_number)[1:50]
sum(is.na(CleanData$prospectif_openPledges_number))

# Promesse illimitée
Data$UL_UNLIMIT_FLG[1:15] 
sum(is.na(Data$UL_UNLIMIT_FLG))
unique(Data$UL_UNLIMIT_FLG)[1:10]
table(Data$UL_UNLIMIT_FLG)
CleanData$prospectif_unlimitedPledge <- NA
CleanData$prospectif_unlimitedPledge[Data$UL_UNLIMIT_FLG==""] <- 0
CleanData$prospectif_unlimitedPledge[Data$UL_UNLIMIT_FLG=="Y"] <- 1
table(CleanData$prospectif_unlimitedPledge)
sum(is.na(CleanData$prospectif_unlimitedPledge))

# Don planifié (Y/N)
Data$UL_DON_PLANIF[1:15] 
sum(is.na(Data$UL_DON_PLANIF))
unique(Data$UL_DON_PLANIF)[1:10]
table(Data$UL_DON_PLANIF)
CleanData$prospectif_plannedDonation <- NA
CleanData$prospectif_plannedDonation[Data$UL_DON_PLANIF==""] <- 0
CleanData$prospectif_plannedDonation[Data$UL_DON_PLANIF=="OUI"] <- 1
table(CleanData$prospectif_plannedDonation)
sum(is.na(CleanData$prospectif_plannedDonation))

# Programme
CleanData$Programme <- NA
CleanData$Programme[CleanData$education_FirstDegree_Medecine == 1] <- "Médecine"
CleanData$Programme[CleanData$education_FirstDegree_Admin == 1] <- "Administration"
CleanData$Programme[CleanData$education_FirstDegree_AgroNutri == 1] <- "Agronomie"
CleanData$Programme[CleanData$education_FirstDegree_Music == 1] <- "Musique"
CleanData$Programme[CleanData$education_FirstDegree_Pharm == 1] <- "Pharmacie"
CleanData$Programme[CleanData$education_FirstDegree_ArchiArtDesign == 1] <- "Aménagement, architecture, art et design"
CleanData$Programme[CleanData$education_FirstDegree_Droit == 1] <- "Droit"
CleanData$Programme[CleanData$education_FirstDegree_EtudeSup == 1] <- "Études supérieures"
CleanData$Programme[CleanData$education_FirstDegree_Education == 1] <- "Éducation"
CleanData$Programme[CleanData$education_FirstDegree_ForestGeo == 1] <- "Foresterie, géographie et géomatique"
CleanData$Programme[CleanData$education_FirstDegree_LettresScienceHum == 1] <- "Lettres et sciences humaines"
CleanData$Programme[CleanData$education_FirstDegree_MedDentaire == 1] <- "Médecine dentaire"
CleanData$Programme[CleanData$education_FirstDegree_Philo == 1] <- "Philosophie"
CleanData$Programme[CleanData$education_FirstDegree_ScienceGenie == 1] <- "Sciences et génie"
CleanData$Programme[CleanData$education_FirstDegree_ScienceInfirmiere == 1] <- "Sciences infirmières"
CleanData$Programme[CleanData$education_FirstDegree_ScienceSociale == 1] <- "Sciences sociales"
CleanData$Programme[CleanData$education_FirstDegree_Theologie == 1] <- "Théologie et sciences religieuses"
CleanData$Programme[CleanData$education_FirstDegree_MultidispCollegeUni == 1] <- "Diplôme multidisciplinaire ou collège universitaire"

#yearSinceMIGraduation
CleanData$yearSinceMIGraduation <- NA
CleanData$yearSinceMIGraduation <- 2023 - CleanData$education_yearMIGraduation

# ROUGE ET OR ####

ro2 <- ro %>% 
  arrange(UL_NO_CODE) %>% 
  group_by(UL_NO_CODE) %>% 
  mutate(n = 1:n()) %>% 
  pivot_wider(id_cols = UL_NO_CODE,
              names_from = n,
              values_from = c(ro_sport, ro_startDate)) %>% 
  mutate(ro_athlete = 1)

CleanData2 <- left_join(CleanData, ro2, by = "UL_NO_CODE") %>% 
  replace_na(list(ro_athlete = 0))

# Save it ####
saveRDS(CleanData2, "_SharedFolder_fondation-ulaval/Data/CleanData.rds")
