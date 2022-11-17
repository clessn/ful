
##***********###
# Packages ####
##***********###
library(DBI)
library(RSQLite)

##***********###
# Get data from SQLite ####
##***********###
mydb <- dbConnect(RSQLite::SQLite(), "_SharedFolder_fondation-ulaval/Data/ful_bd.db")

Data <- dbGetQuery(mydb, "SELECT
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
                            FROM data_all
                            ORDER BY RANDOM() LIMIT 50000")

dbDisconnect(mydb)

##***********###
# Generate empty CleanData ####
##***********###

CleanData <- data.frame(id = 1:nrow(Data))


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

# EDUCATION ####

# EMPLOI ####

# CONTACT ####

# Nombre de refus pour les communications (SOLMAS, SOLCNV, SUISOL et SOLPER)
unique(Data$UL_NBR_REFUS)[1:10]
table(Data$UL_NBR_REFUS)
CleanData$contact_declineCommunications_number <- minmaxNormalization(clean_raw_num(Data$UL_NBR_REFUS))
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


# HISTORIQUE ####

# Montant totale de l'encaissement de l'an passé
unique(Data$UL_ENC_AN_PASSE)[1:10]
table(Data$UL_ENC_AN_PASSE)[1:100]
CleanData$historic_CollectedLastYear_totalAmount <- clean_raw_num(Data$UL_ENC_AN_PASSE)
table(CleanData$historic_CollectedLastYear_totalAmount)[1:50]

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



