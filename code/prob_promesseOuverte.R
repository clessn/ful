# Packages ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(nnet)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_fondation-ulaval/Data/CleanData.rds") %>% 
  select(ses_female, ses_origin_qc, Programme,
         prospectif_openPledges_number, yearSinceMIGraduation) %>%
  mutate(prospectif_openPledges_number = ifelse(prospectif_openPledges_number == 2, 1, prospectif_openPledges_number))

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

table(Data$prospectif_openPledges_number)


for (i in 1:length(unique(Data$Programme))){
  prog <- unique(Data$Programme)[i]
  d <- Data %>% 
    filter(Programme == prog) %>% 
    drop_na(ses_origin_qc, ses_female, yearSinceMIGraduation) %>% 
    select(prospectif_openPledges_number, ses_origin_qc, ses_female, yearSinceMIGraduation) %>% 
    mutate(yearSinceMIGraduation = factor(yearSinceMIGraduation, ordered = F))
  model <- glm(prospectif_openPledges_number ~ yearSinceMIGraduation * ses_origin_qc * ses_female, 
               data = d,
               family = binomial())
  years <- sort(unique(d$yearSinceMIGraduation))
  df <- d %>% 
    group_by(yearSinceMIGraduation, ses_origin_qc, ses_female) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    select(-n) %>%
    mutate(programme = prog) %>% 
    select(programme, yearSinceMIGraduation, ses_female, ses_origin_qc)
  df$pred <- predict(model, newdata = df, type = "response")
  df$yearSinceMIGraduation <- as.numeric(as.character(df$yearSinceMIGraduation))
  if (i == 1){
    GraphData <- df
  } else {
    GraphData <- rbind(GraphData, df)
  }
  print(paste0(prog, " done : ", i, "/", length(unique(Data$Programme))))
}
GraphData2 <- Data %>% 
  group_by(Programme, yearSinceMIGraduation, ses_origin_qc, ses_female) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  left_join(GraphData, ., by = c("programme" = "Programme",
                                 "yearSinceMIGraduation",
                                 "ses_origin_qc", "ses_female")) %>% 
  mutate(pred = ifelse(pred < 0, 0, pred))

#Graphique
GraphData2 %>%
  mutate(ses_female = factor(ses_female),
         ses_female = case_when(
           ses_female == "0" ~ "Homme",
           ses_female == "1" ~ "Femme"
         ),
         ses_origin_qc = factor(ses_origin_qc),
         ses_origin_qc = case_when(
           ses_origin_qc == "0" ~ "Hors-Québec",
           ses_origin_qc == "1" ~ "Du Québec"
         )) %>%
  filter(programme == "Musique") %>% 
  #filter(!(programme %in% c("Études supérieures",
  #                          "Pharmacie",
  #                          "Philosophie",
  #                          "Sciences infirmières"))) %>% 
  ggplot(aes(
    y = pred*100,
    x = -yearSinceMIGraduation,
    color = interaction(ses_female, ses_origin_qc),
    group = interaction(ses_female, ses_origin_qc)
  )) +
  geom_line(alpha = 0.2) +
  facet_wrap( ~ programme)+
  geom_smooth(se = F,
              aes(weight = n)
  ) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(-25, 0)) +
  #scale_x_continuous(labels = c(60, 40, 20, 0)) +
  scale_color_discrete(labels = c(
    "Femme du Québec",
    "Homme du Québec",
    "Femme hors-Québec",
    "Homme hors-Québec"
  ),
  type = c("#FFC103", "#E30513", "black", "#1B9DDA"))+
  ylab("Probabilité d'avoir planifier un don (%)") +
  xlab("Nombre d'années depuis\nle diplôme le plus important") +
  clessnverse::theme_clean_light() +
  theme(axis.title.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5))
