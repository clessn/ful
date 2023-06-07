library(dplyr)
library(ggplot2)
library(ggpattern)
# Chargement des données
data <- readRDS("_SharedFolder_fondation-ulaval/Data/CleanData.rds")
options(scipen = 999)

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

data_prof_graphs$job <- factor(data_prof_graphs$job,
                               levels = c("prof", "c_de_cours", "autre"))


ggplot(data_prof_graphs, aes(x = job, y = proportion, fill = job)) +
  geom_bar(stat = "identity", position = "dodge",
           alpha = 0.7, show.legend = FALSE) +
   geom_text(label = paste(round(data_prof_graphs$proportion, 4), "%"),
             size = 5.5, nudge_y = 0.0002) +
  scale_x_discrete(labels = c(
    "Professeurs",
    "Chargés de cours",
    "Autres employés")) +
  scale_y_continuous(limits = c(0, 0.01)) +
  labs(title = "Dons en héritage") +
  ylab("Proportion du groupe qui a\nfait un don en héritage (%)") +
  xlab(element_blank()) +
  scale_fill_manual(values = c("#ed1b24", "#FFC904", "#ed1b24")) +
  clessnverse::theme_clean_light() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15,
                                    hjust = 0.5),
        title = element_text(size = 20))

ggsave("_SharedFolder_fondation-ulaval/graphs/prof_dons_heritage.png",
       width = 10, height = 7)


# historic_lifeInsuranceDonation
# prospectif_plannedDonation
# emploi_teacherUlaval
# emploi_lecturerUlaval