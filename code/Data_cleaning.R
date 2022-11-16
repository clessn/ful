
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
                          UL_DON_PLANIF
                            FROM data_all
                            ORDER BY RANDOM() LIMIT 50000")

dbDisconnect(mydb)

##***********###
# Generate empty CleanData ####
##***********###

CleanData <- data.frame(id = 1:nrow(Data))


# SES ####

# EDUCATION ####

# EMPLOI ####

# CONTACT ####

# HISTORIQUE ####
unique(Data$UL_ENC_AN_PASSE)[1:10]
table(Data$UL_ENC_AN_PASSE)[1:100]
CleanData$historic_totalCollectedLastYear <- gsub(",", ".",
                                                  Data$UL_ENC_AN_PASSE)
CleanData$historic_totalCollectedLastYear[which(substr(CleanData$historic_totalCollectedLastYear, 1, 1) == ".")] <- paste0("0", CleanData$historic_totalCollectedLastYear[which(substr(CleanData$historic_totalCollectedLastYear, 1, 1) == ".")])
CleanData$historic_totalCollectedLastYear <- as.numeric(CleanData$historic_totalCollectedLastYear)
table(CleanData$historic_totalCollectedLastYear)[1:50]

# PROSPECTIF ####




