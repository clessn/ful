library(tidyverse)

nb_jour_reponse <- 30

# Potentiels donateurs de départ
data_FUL <- readRDS("_SharedFolder_fondation-ulaval/Data/CleanData.rds")

# Potentiels donateurs avec une date de dernière communication
comm_data_FUL <- data_FUL %>%
  filter(!is.na(contact_lastCommunicationDate))

# Remplacer la date de don par 1971-01-01 pour ceux n'ayant pas de date de dernier don
# 1971-01-01 est la plus petite date de don de la BD
# 1994-04-13 est la plus petite date de communication de la BD
dons_comm <- comm_data_FUL %>%
  select(
    id,
    starts_with("ses_"),
    starts_with("education_"),
    starts_with("emploi_"),
    contact_lastCommunicationDate,
    historic_lastDonation_date
  ) %>%
  mutate(lastDonation_date = ifelse(is.na(historic_lastDonation_date), "1971-01-01", as.character(historic_lastDonation_date)))

dons_comm <- dons_comm %>%
  mutate(diff_dons_comm = as.integer(historic_lastDonation_date - contact_lastCommunicationDate)) %>%
  mutate(diff_dons_comm_annee = as.integer(ifelse(diff_dons_comm < 0, -1, ceiling(diff_dons_comm / 365)))) %>%
  mutate(reponse_comm_positive = (diff_dons_comm >= 0 & diff_dons_comm <= nb_jour_reponse))

table(dons_comm %>% select(ses_female, ses_origin_qc, reponse_comm_positive))

# Stat pour les potentiels donateurs qui ont reçu une commmunication
Graph <- dons_comm %>%
  group_by(ses_female, ses_origin_qc) %>% 
  summarise(n_reponse_pos = sum(reponse_comm_positive, na.rm = T),
            n_total = n()) %>% 
  mutate(prop = n_reponse_pos/n_total * 100) %>% 
  drop_na()

ggplot(Graph, aes(y= prop, x= interaction(ses_female, ses_origin_qc))) + 
  geom_bar(stat = "identity", aes(fill = factor(ses_female), color = factor(ses_female)),
           alpha = 0.6, show.legend = FALSE) +
  geom_text(label = paste(round(Graph$prop, 2),"%"), nudge_y = 0.03, size = 5.5) + 
  scale_x_discrete(labels = c(
    "Homme hors-Québec",
    "Femme hors-Québec",
    "Homme du Québec",
    "Femme du Québec")) +
  scale_fill_manual(values = c( "1" = "#FFC103",
                                "0" = "#E30513")) +
  scale_color_manual(values = c( "1" = "#FFC103",
                                 "0" = "#E30513")) +
  scale_y_continuous(limits = c(0, 2), breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)) +
  labs(title = "Réponse positive à la dernière communication") +
  ylab("Proportion (%) \n") +
  xlab(element_blank()) +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        title = element_text(size = 20))

ggsave("_SharedFolder_fondation-ulaval/graphs/profilsDonsPlanifiés.png",
       width = 12, height = 9)
