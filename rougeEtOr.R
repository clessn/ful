# Packages ----------------------------------------------------------------
library(tidyverse)
library(lubridate)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_fondation-ulaval/Data/CleanData.rds") %>% 
  mutate(donated = ifelse(historic_lastDonation_amount > 0, 1, 0))


## Grouper les programmes ---------------------------------------------------------------

# Créer le vecteur "programs" avec les noms des programmes
programs <- c("Administration",
              "Agronomie",
              "Aménagement, architecture, art et design",
              "Diplôme multidisciplinaire ou collège universitaire",
              "Droit",
              "Éducation",
              "Foresterie, géographie et géomatique",
              "Lettres et sciences humaines",
              "Médecine",
              "Médecine dentaire",
              "Musique",
              "Sciences et génie",
              "Sciences sociales",
              "Théologie et sciences religieuses",
              "Études supérieures",
              "Pharmacie",
              "Philosophie",
              "Sciences infirmières")

# Créer le vecteur "groups" avec les groupes correspondants pour chaque programme
groups <- c("Administration et gestion",
            "Agriculture et environnement",
            "Aménagement et design",
            "Autres",
            "Sociales et éducatives",
            "Sociales et éducatives",
            "Agriculture et environnement",
            "Humaines et religieuses",
            "Santé",
            "Santé",
            "Art",
            "Sciences et génie",
            "Sociales et éducatives",
            "Humaines et religieuses",
            "Autres",
            "Santé",
            "Humaines et religieuses",
            "Santé")

# Associer les noms des groupes avec les éléments correspondants du vecteur "groups"
names(groups) <- programs


## Clean program names -----------------------------------------------------
clean_names <- c("Administration",
                 "Agronomie",
                 "Aménagement, architecture,\nart et design",
                 "Diplôme multidisciplinaire ou\ncollège universitaire",
                 "Droit",
                 "Éducation",
                 "Foresterie, géographie\net géomatique",
                 "Lettres et\nsciences humaines",
                 "Médecine",
                 "Médecine dentaire",
                 "Musique",
                 "Sciences\net génie",
                 "Sciences\nsociales",
                 "Théologie et\nsciences religieuses",
                 "Études supérieures",
                 "Pharmacie",
                 "Philosophie",
                 "Sciences infirmières")

names(clean_names) <- programs


# Descriptif --------------------------------------------------------------

table(Data$ro_athlete)

Data %>% 
  group_by(ro_athlete, donated) %>% 
  summarise(n = n()) %>%
  drop_na() %>% 
  group_by(ro_athlete) %>% 
  mutate(sum = sum(n),
         prop = n/sum,
         ro_athlete = factor(ro_athlete, levels = c("0",
                                                    "1"),
                             labels = c("0" = "Non-athlète",
                                        "1" = "Athlète du Rouge et Or"))) %>% 
  filter(donated == 1) %>% 
  ggplot(aes(x = ro_athlete, y = prop*100)) +
  geom_bar(stat = "identity",
           aes(y = 100), fill = "grey",
           alpha = 0.4) +
  geom_bar(stat = "identity", position = "dodge",
           alpha = 0.7, show.legend = FALSE,
           aes(fill = ro_athlete, color = ro_athlete)) +
  geom_text(aes(label = paste(round(prop*100), "%")),
            size = 5.5, nudge_y = 2.25) +
  geom_text(aes(label = paste(format(sum, big.mark = " "), " observations"),
                y = 95),
            size = 5.5, nudge_y = 2.25) +
  scale_y_continuous(limits = c(0, 100)) +
  ylab("Proportion du groupe\nqui a déjà donné à la FUL (%)") +
  xlab("") +
  scale_fill_manual(values = c("Non-athlète" = "#FFC904",
                               "Athlète du Rouge et Or"  = "#ed1b24")) +
  scale_color_manual(values = c("Non-athlète" = "#FFC904",
                               "Athlète du Rouge et Or"  = "#ed1b24")) +
  clessnverse::theme_clean_light() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15,
                                    hjust = 0.5))

ggsave("_SharedFolder_fondation-ulaval/graphs/athletes_descriptif.png",
       width = 8, height = 8)

## By sport

Graph2 <- Data %>% 
  pivot_longer(starts_with("ro_sport"),
               names_to = "type",
               values_to = "sport") %>% 
  drop_na(sport) %>% 
  group_by(sport, donated) %>% 
  summarise(n = n()) %>%
  drop_na() %>% 
  group_by(sport) %>% 
  mutate(sum = sum(n),
         prop = n/sum) %>% 
  filter(donated == 1)

ggplot(Graph2, aes(x = prop*100, y = reorder(sport, prop))) +
  geom_bar(stat = "identity",
           aes(alpha = sum),
           fill = "#E30513",
           show.legend = F) +
  geom_text(x = -1.75, hjust = 1,
            aes(label = paste0("n = ", sum))) +
  clessnverse::theme_clean_light() +
  xlab("Proportion des athlètes ayant\ndéjà donné à la FUL (%)") +
  ylab("") +
  scale_x_continuous(limits = c(-4, 100)) +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20,
                                    hjust = 0.5))

ggsave("_SharedFolder_fondation-ulaval/graphs/par_sport.png",
       width = 12, height = 9)
