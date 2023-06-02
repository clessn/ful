library(dplyr)
library(ggplot2)

# Chargement des données
data <- readRDS("_SharedFolder_fondation-ulaval/Data/CleanData.rds")

data_prof_dons <- data %>%
    select(emploi_teacherUlaval,
           emploi_lecturerUlaval,
           historic_lifeInsuranceDonation,
           prospectif_plannedDonation)

data_prof_dons$job <- NA
data_prof_dons$job[data_prof_dons$emploi_teacherUlaval == 1] <- "prof"
data_prof_dons$job[data_prof_dons$emploi_lecturerUlaval == 1] <- "c_de_cours"
data_prof_dons$job[data_prof_dons$emploi_teacherUlaval == 0 &
                   data_prof_dons$emploi_lecturerUlaval == 0] <- "autre"

data_prof_dons$don <- 0
data_prof_dons$don[data_prof_dons$historic_lifeInsuranceDonation == 1] <- 1
data_prof_dons$don[data_prof_dons$prospectif_plannedDonation == 1] <- 1

data_prof_dons <- data_prof_dons %>%
    select(job, don)

table(data_prof_dons$job)
table(data_prof_dons$don)

data_prof_dons <- data_prof_dons %>%
    group_by(job, don) %>%
    summarise(n = n()) %>%
    group_by(job) %>%
    mutate(proportion = n / sum(n))

data_prof_graphs <- data_prof_dons %>%
    filter(don == 1)

ggplot(data_prof_graphs, aes(x = job, y = proportion)) +
    geom_bar(stat = "identity", position = "dodge")
# historic_lifeInsuranceDonation
# prospectif_plannedDonation
# emploi_teacherUlaval
# emploi_lecturerUlaval