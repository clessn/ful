# Packages ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(nnet)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_fondation-ulaval/Data/CleanData.rds") %>% 
  mutate(donated = ifelse(historic_lastDonation_amount > 0, 1, 0)) %>% 
  filter((education_yearMIGraduation >= 1960) &
           (education_FirstDegree_Medecine == 1 |
            education_FirstDegree_Admin == 1 |
            education_FirstDegree_AgroNutri == 1 |
            education_FirstDegree_Music == 1 |
            education_FirstDegree_Pharm == 1 | 
            education_FirstDegree_ArchiArtDesign == 1 |
            education_FirstDegree_Droit == 1 |
            education_FirstDegree_EtudeSup == 1 |
            education_FirstDegree_Education == 1 |
            education_FirstDegree_ForestGeo == 1 |
            education_FirstDegree_LettresScienceHum == 1 |
            education_FirstDegree_MedDentaire == 1 |
            education_FirstDegree_Philo == 1 |
            education_FirstDegree_ScienceGenie == 1 |
            education_FirstDegree_ScienceInfirmiere == 1 |
            education_FirstDegree_ScienceSociale == 1 |
            education_FirstDegree_Theologie == 1 |
            education_FirstDegree_MultidispCollegeUni == 1))


# Wrangling ---------------------------------------------------------------
Data$Programme <- NA
Data$Programme[Data$education_FirstDegree_Medecine == 1] <- "Médecine"
Data$Programme[Data$education_FirstDegree_Admin == 1] <- "Administration"
Data$Programme[Data$education_FirstDegree_AgroNutri == 1] <- "Agronomie"
Data$Programme[Data$education_FirstDegree_Music == 1] <- "Musique"
Data$Programme[Data$education_FirstDegree_Pharm == 1] <- "Pharmacie"
Data$Programme[Data$education_FirstDegree_ArchiArtDesign == 1] <- "Aménagement, architecture, art et design"
Data$Programme[Data$education_FirstDegree_Droit == 1] <- "Droit"
Data$Programme[Data$education_FirstDegree_EtudeSup == 1] <- "Études supérieures"
Data$Programme[Data$education_FirstDegree_Education == 1] <- "Éducation"
Data$Programme[Data$education_FirstDegree_ForestGeo == 1] <- "Foresterie, géographie et géomatique"
Data$Programme[Data$education_FirstDegree_LettresScienceHum == 1] <- "Lettres et sciences humaines"
Data$Programme[Data$education_FirstDegree_MedDentaire == 1] <- "Médecine dentaire"
Data$Programme[Data$education_FirstDegree_Philo == 1] <- "Philosophie"
Data$Programme[Data$education_FirstDegree_ScienceGenie == 1] <- "Sciences et génie"
Data$Programme[Data$education_FirstDegree_ScienceInfirmiere == 1] <- "Sciences infirmières"
Data$Programme[Data$education_FirstDegree_ScienceSociale == 1] <- "Sciences sociales"
Data$Programme[Data$education_FirstDegree_Theologie == 1] <- "Théologie et sciences religieuses"
Data$Programme[Data$education_FirstDegree_MultidispCollegeUni == 1] <- "Diplôme multidisciplinaire ou collège universitaire"

Data$yearSinceMIGraduation <- NA
Data$yearSinceMIGraduation <- 2023 - Data$education_yearMIGraduation
table(Data$yearSinceMIGraduation)


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


# First models: lot of interaction ----------------------------------------

### ici, je vais essayer des modèles avec beaucoup d'interactions entre les VIs.
### Certains programmes n'auront donc pas de bon modèle si pas assez de répondants
### dans certaines strats.


## Long loop (SKIP) --------------------------------------------------------------------

for (i in 1:length(unique(Data$Programme))){
  prog <- unique(Data$Programme)[i]
  d <- Data %>% 
    filter(Programme == prog) %>% 
    drop_na(yearSinceMIGraduation, ses_origin_qc, ses_female) %>% 
    select(yearSinceMIGraduation, ses_origin_qc, ses_female, donated) %>% 
    mutate(yearSinceMIGraduation = factor(yearSinceMIGraduation, ordered = F))
  model <- glm(donated ~ yearSinceMIGraduation * ses_origin_qc * ses_female, 
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

#saveRDS(GraphData2, "_SharedFolder_fondation-ulaval/Data/marts/prob_don_yearSinceMIGradution.rds")

GraphData2 <- readRDS("_SharedFolder_fondation-ulaval/Data/marts/prob_don_yearSinceMIGradution.rds")

## Graph -------------------------------------------------------------------
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
  filter(!(programme %in% c("Études supérieures",
                            "Pharmacie",
                            "Philosophie",
                            "Sciences infirmières"))) %>% 
  ggplot(aes(
    y = pred*100,
    x = -yearSinceMIGraduation,
    color = interaction(ses_female, ses_origin_qc),
    group = interaction(ses_female, ses_origin_qc)
  )) +
  geom_line(alpha = 0.2) +
  facet_wrap( ~ programme) +
  geom_smooth(se = F,
              aes(weight = n)
              ) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(labels = c(60, 40, 20, 0)) +
  scale_color_discrete(labels = c(
    "Femme du Québec",
    "Homme du Québec",
    "Femme hors-Québec",
    "Homme hors-Québec"
  ),
  type = c("#FFC103", "#E30513", "black", "#1B9DDA")) +
ylab("Probabilité d'avoir\ndéjà donné à la FUL (%)") +
  xlab("Nombre d'années depuis\nle diplôme le plus important") +
  clessnverse::theme_clean_light() +
  theme(axis.title.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5))

ggsave("_SharedFolder_fondation-ulaval/graphs/prob_don_yearSinceMIGradution.png",
       width = 12, height = 9)


# Second models: less interaction -----------------------------------------



## Long loop (SKIP) --------------------------------------------------------
for (i in 1:length(unique(Data$Programme))){
  prog <- unique(Data$Programme)[i]
  d <- Data %>% 
    filter(Programme == prog) %>% 
    drop_na(yearSinceMIGraduation, ses_origin_qc, ses_female) %>% 
    select(yearSinceMIGraduation, ses_origin_qc, ses_female, donated) %>% 
    mutate(yearSinceMIGraduation = factor(yearSinceMIGraduation, ordered = F))
  model <- glm(donated ~ yearSinceMIGraduation + ses_origin_qc + ses_female, 
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
    GraphData3 <- df
  } else {
    GraphData3 <- rbind(GraphData3, df)
  }
  print(paste0(prog, " done : ", i, "/", length(unique(Data$Programme))))
}

GraphData4 <- Data %>% 
  group_by(Programme, yearSinceMIGraduation, ses_origin_qc, ses_female) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  left_join(GraphData3, ., by = c("programme" = "Programme",
                                 "yearSinceMIGraduation",
                                 "ses_origin_qc", "ses_female")) %>% 
  mutate(pred = ifelse(pred < 0, 0, pred))

#saveRDS(GraphData4, "_SharedFolder_fondation-ulaval/Data/marts/prob_don_yearSinceMIGradution2.rds")

GraphData4 <- readRDS("_SharedFolder_fondation-ulaval/Data/marts/prob_don_yearSinceMIGradution2.rds")

GraphData4$group <- groups[GraphData4$programme]

## Graph -------------------------------------------------------------------

for (i in 1:length(unique(GraphData4$programme))){
  prog <- unique(GraphData4$programme)[i]
  d <- GraphData4 %>%
    filter(programme == prog)
  model <- loess(pred ~ yearSinceMIGraduation,
                 data = d,
                 weights = n)
  df <- data.frame(programme = prog,
                   yearSinceMIGraduation = max(d$yearSinceMIGraduation)) %>% 
    mutate(loess_pred = predict(model, .),
           group = groups[programme])
  if (i == 1){
    loessd <- df
  } else {
    loessd <- rbind(loessd, df)
  }
}

loessd$programme <- clean_names[loessd$programme]

GraphData4 %>%
  mutate(programme = clean_names[programme]) %>% 
  ggplot(aes(
    y = pred*100,
    x = -yearSinceMIGraduation,
    group = programme,
    color = programme)) +
  geom_line(alpha = 0.1, show.legend = F) +
  facet_wrap( ~ group) +
  geom_smooth(se = F,
              aes(weight = n),
  show.legend = F) +
  scale_y_continuous(limits = c(0,115),
                     #expand = c(1, 1),
                     breaks = c(0, 25, 50, 75, 100)) +
  scale_x_continuous(labels = c(60, 40, 20, 0),
                     breaks = c(-60, -40, -20, 0),
                     limits = c(-90, 5),
                     expand = c(0.1, 0.1)) +
  ggrepel::geom_label_repel(data = loessd,
             aes(x = -yearSinceMIGraduation-14,
                 y = loess_pred*100 + 2.5,
                 label = programme,
                 fill = programme),
             color = "white",
             size = 2.3,
             show.legend = F,
             alpha = 0.8,
             force_pull = 10,
             force = 1.5,
             nudge_x = 0.5) +
  ylab("Probabilité d'avoir\ndéjà donné à la FUL (%)") +
  xlab("Nombre d'années depuis\nle diplôme le plus important") +
  clessnverse::theme_clean_light() +
  theme(axis.title.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5))

ggsave("_SharedFolder_fondation-ulaval/graphs/prob_don_yearSinceMIGradution2.png",
       width = 12, height = 9)



# ALL CRAP, SKIP Year as groups ----------------------------------------------------------

## Crap --------------------------------------------------------------------

## Make year groups with dpih()!!!!

Long <- Data %>% 
  filter(education_yearFirstGraduation == 2000) %>% 
  mutate(year_first_don = lubridate::year(historic_dateFirstDonation),
         delta_first_don1 = year_first_don - education_yearFirstGraduation,
         delta_first_don = ifelse(delta_first_don1 < 0, "donated_before_grad", delta_first_don1),
         delta_first_don = ifelse(is.na(delta_first_don), "not_donated", delta_first_don),
         delta_first_don = factor(delta_first_don),
         delta_first_don = relevel(delta_first_don, ref = "not_donated"),
         vd = cut(x = delta_first_don1,
                  breaks = quantile(delta_first_don1, c(0, 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9, 1), na.rm = T)),
         vd = ifelse(is.na(vd), vd, paste0("delta", vd)),
         vd2 = forcats::fct_explicit_na(vd, na_level = "not_donated"),
         vd2 = relevel(vd2, ref = "not_donated"),
         final_vd = ifelse(is.na(delta_first_don1), delta_first_don1, paste0("delta", delta_first_don1))) %>% 
  select(programme = Programme, year_first_don, ses_female, ses_origin_qc, final_vd, donated, vd, delta_first_don1)

model_prob_don <- glm(donated ~ programme + factor(ses_female) + factor(ses_origin_qc),
                      data = Long,
                      family = binomial())

model_lm <- lm(delta_first_don1 ~ programme + factor(ses_female) + factor(ses_origin_qc),
               data = Long)

model_delta_don <- multinom(vd ~ programme + factor(ses_female) + factor(ses_origin_qc),
                  data = Long,
                  maxit = 10000)

Df <- Long %>% 
  group_by(programme, ses_female, ses_origin_qc) %>% 
  summarise(n = n()) %>%
  ungroup() %>% 
  mutate(prob_don = clessnverse::normalize_min_max(predict(model_prob_don, ., type = "response"))) %>% 
  cbind(., round(predict(model_delta_don, ., type = "probs")*.[["prob_don"]],2)) %>% 
  pivot_longer(., cols = starts_with("delta"),
               names_to = "delta",
               names_prefix = "delta",
               values_to = "prob") %>%
  mutate(delta = gsub("\\(", "", delta),
         delta = gsub("]", "", delta)) %>%
  separate(., delta, sep = ",", into = c("delta1", "delta2")) %>% 
  mutate(delta = (as.numeric(delta1)+as.numeric(delta2))/2) %>% 
  select(-delta1, -delta2)

ggplot(Df, aes(x = delta, y = prob, group = programme, color = programme)) +
  geom_smooth(se = F)


Df <- Long %>% 
  #group_by(programme, ses_female, ses_origin_qc) %>% 
  #summarise(n = n()) %>%
  #ungroup() %>% 
  mutate(prob = clessnverse::normalize_min_max(predict(model_prob_don, ., type = "response")),
         pred = predict(model_lm, .))


## Second try --------------------------------------------------------------


### Long loop (SKIP) --------------------------------------------------------
for (i in 1980:2022){
  Long <- Data %>%
    ## Only keep people who had graduated in year i
    filter(education_yearFirstGraduation <= i) %>% 
    mutate(yearSinceFirstGraduation = i - education_yearFirstGraduation,
           year_first_don = lubridate::year(historic_dateFirstDonation),
           donated = ifelse(year_first_don > i, 0, 1),
           donated = ifelse(is.na(donated), 0, donated)) %>% 
    select(programme = Programme, ses_female, ses_origin_qc, year_first_don, yearSinceFirstGraduation, donated) %>% 
    mutate(yearSinceFirstGraduation = factor(yearSinceFirstGraduation))
  model <- glm(donated ~ programme + factor(ses_female) + factor(ses_origin_qc) + yearSinceFirstGraduation,
               data = Long,
               family = binomial())
  Df <- Long %>% 
    group_by(programme, ses_female, ses_origin_qc, yearSinceFirstGraduation) %>% 
    summarise(n = n()) %>%
    ungroup() %>% 
    mutate(prob_don = predict(model, ., type = "response"),
           year = i)
  if (i == 1980){
    Graph <- Df
  } else {
    Graph <- rbind(Graph, Df)
  }
  print(i)
}

Graph <- Graph %>% 
  mutate(yearSinceFirstGraduation = as.numeric(as.character(yearSinceFirstGraduation)))

#saveRDS(Graph, "_SharedFolder_fondation-ulaval/Data/marts/prob_don_acrossYears.rds")


### Graph -------------------------------------------------------------------
Graph2 <- Graph %>% 
  group_by(year, yearSinceFirstGraduation, programme) %>% 
  summarise(prob = weighted.mean(prob_don, w = n))

ggplot(Graph2, aes(y = yearSinceFirstGraduation,
                  x = year)) +
  geom_point(aes(color = prob),
             size = 2, stroke = NA) +
  scale_y_continuous(limits = c(0,50)) +
  scale_color_gradient2(mid = "#FFC103",
                        low = "#E30513",
                        high = "#1B9DDA",
                        midpoint = 0.5) +
  facet_wrap(~programme) +
  clessnverse::theme_clean_light()

ggsave("_SharedFolder_fondation-ulaval/graphs/prob_don_acrossYears.png",
       width = 12, height = 9)


Graph3 <- Graph %>% 
  group_by(year, yearSinceFirstGraduation, programme) %>% 
  summarise(prob = weighted.mean(prob_don, w = n)) %>% 
  group_by(year, yearSinceFirstGraduation, programme) %>% 
  filter(prob == max(prob))

ggplot(Graph3, aes(y = yearSinceFirstGraduation,
                   x = year)) +
  geom_smooth() +
  facet_wrap(~programme)


# Multinom: first donation ------------------------------------------------
Data2 <- Data %>% 
  mutate(year_first_don = lubridate::year(historic_dateFirstDonation),
         delta_first_don = year_first_don - education_yearFirstGraduation,
         delta_first_don = ifelse(delta_first_don < 0, -1, delta_first_don),
         delta_first_don = ifelse(delta_first_don >= 50, 50, delta_first_don),
         delta_first_don = ifelse(!is.na(delta_first_don), paste0("pivot", as.character(delta_first_don)),
                                  delta_first_don),
         delta_first_don = factor(delta_first_don, ordered = F)) %>% 
  drop_na(delta_first_don) %>% 
  select(programme = Programme, ses_female, ses_origin_qc, delta_first_don)


## Long loop (SKIP) --------------------------------------------------------
for (i in 1:length(unique(Data2$programme))){
  prog <- unique(Data2$programme)[i]
  d <- Data2 %>% 
    filter(programme == prog)
  model <- multinom(delta_first_don ~ factor(ses_female) *
                      factor(ses_origin_qc),
                    data = d,
                    maxit = 10000)
  df <- d %>% 
    group_by(ses_female, ses_origin_qc) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    cbind(., predict(model, ., type = "probs")) %>% 
    pivot_longer(., cols = starts_with("pivot"),
                 names_to = "delta_first_don",
                 names_prefix = "pivot",
                 values_to = "prob") %>% 
    mutate(delta_first_don = as.numeric(delta_first_don),
           prob2 = round(prob, 2),
           programme = prog) %>% 
    select(programme, ses_female, ses_origin_qc, n,
           delta_first_don, prob)
  if (i == 1){
    GraphData5 <- df
  } else {
    GraphData5 <- rbind(GraphData5, df)
  }
  print(paste0(prog, " done : ", i, "/", length(unique(Data2$programme))))
}

#saveRDS(GraphData5, "_SharedFolder_fondation-ulaval/Data/marts/delta_first_don.rds")

GraphData5 <- readRDS("_SharedFolder_fondation-ulaval/Data/marts/delta_first_don.rds")

## Graph 1: All programs -------------------------------------------------------------------

GraphData5 %>% 
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
  ggplot(aes(x = delta_first_don, y = prob*100,
                       group = interaction(ses_female, ses_origin_qc),
                       color = interaction(ses_female, ses_origin_qc))) +
  #geom_line(alpha = 0.2) +
  geom_smooth(method = "gam",
              aes(weight = n),
              se = F) +
  scale_y_continuous(limits = c(0,15)) +
  scale_x_continuous(breaks = seq(from=0,to=50,by=5)) +
  scale_color_discrete(labels = c(
    "Femme du Québec",
    "Homme du Québec",
    "Femme hors-Québec",
    "Homme hors-Québec"
  ),
  type = c("#FFC103", "#E30513", "black", "#1B9DDA")) +
  clessnverse::theme_clean_light() +
  ylab("Probabilité du<br>premier don (%)<br>") +
  xlab("<br>Nombre d'années depuis<br>le premier diplôme") +
  theme(axis.title.y = ggtext::element_markdown(hjust = 0.5, size = 18, lineheight = 1),
        axis.title.x = ggtext::element_markdown(hjust = 0.5, size = 18, lineheight = 1),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13))

ggsave("_SharedFolder_fondation-ulaval/graphs/delta_first_don_all.png",
       width = 8, height = 6)


## Graph 2: by program -----------------------------------------------------
for (i in 1:length(unique(GraphData5$programme))){
  prog <- unique(GraphData5$programme)[i]
  d <- GraphData5 %>%
    filter(programme == prog)
  model <- mgcv::gam(prob ~ delta_first_don,
                 data = d,
                 weights = n)
  df <- data.frame(programme = prog,
                   delta_first_don = -1) %>% 
    mutate(gam_pred = predict(model, .),
           group = groups[programme])
  if (i == 1){
    gamd <- df
  } else {
    gamd <- rbind(gamd, df)
  }
}

gamd$programme <- clean_names[gamd$programme]


GraphData5 %>%
  mutate(group = groups[programme],
         programme = clean_names[programme]) %>% 
  ggplot(aes(
    y = prob*100,
    x = delta_first_don,
    group = programme,
    color = programme)) +
  geom_line(alpha = 0.1, show.legend = F) +
  facet_wrap( ~ group) +
  geom_smooth(se = F,
              method = "gam",
              aes(weight = n),
              show.legend = F) +
  scale_y_continuous(limits = c(0,12.5)) +
  scale_x_continuous(breaks = seq(from=0, to = 50, by = 5),
                     limits = c(-20, 55),
                     expand = c(0.1, 0.1)) +
  ggrepel::geom_label_repel(data = gamd,
                            aes(x = -12,
                                y = gam_pred*100,
                                label = programme,
                                fill = programme),
                            color = "white",
                            size = 3,
                            show.legend = F,
                            alpha = 0.8,
                            force = 2,
                            nudge_x = -1,
                            direction = "y") +
  ylab("Probabilité du<br>premier don (%)<br>") +
  xlab("<br>Nombre d'années depuis<br>le premier diplôme") +
  clessnverse::theme_clean_light() +
  theme(axis.title.y = ggtext::element_markdown(hjust = 0.5, size = 18, lineheight = 1),
        axis.title.x = ggtext::element_markdown(hjust = 0.5, size = 18, lineheight = 1),
        axis.text.x = element_text(size = 9),
        strip.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 13))

ggsave("_SharedFolder_fondation-ulaval/graphs/delta_first_don_byProgram.png",
       width = 12, height = 9)


# Multinom: first donation (with MI grad) ------------------------------------------------
Data2 <- Data %>% 
  mutate(year_first_don = lubridate::year(historic_dateFirstDonation),
         delta_first_don1 = year_first_don - education_yearMIGraduation,
         delta_first_don = ifelse(delta_first_don1 < 0, -1, delta_first_don1),
         delta_first_don = ifelse(delta_first_don >= 50, 50, delta_first_don),
         delta_first_don = ifelse(!is.na(delta_first_don), paste0("pivot", as.character(delta_first_don)),
                                  delta_first_don),
         delta_first_don = factor(delta_first_don, ordered = F)) %>% 
  drop_na(delta_first_don) %>% 
  select(programme = Programme, ses_female, ses_origin_qc, delta_first_don)


## Long loop (SKIP) --------------------------------------------------------
for (i in 1:length(unique(Data2$programme))){
  prog <- unique(Data2$programme)[i]
  d <- Data2 %>% 
    filter(programme == prog)
  model <- multinom(delta_first_don ~ factor(ses_female) *
                      factor(ses_origin_qc),
                    data = d,
                    maxit = 10000)
  df <- d %>% 
    group_by(ses_female, ses_origin_qc) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    cbind(., predict(model, ., type = "probs")) %>% 
    pivot_longer(., cols = starts_with("pivot"),
                 names_to = "delta_first_don",
                 names_prefix = "pivot",
                 values_to = "prob") %>% 
    mutate(delta_first_don = as.numeric(delta_first_don),
           prob2 = round(prob, 2),
           programme = prog) %>% 
    select(programme, ses_female, ses_origin_qc, n,
           delta_first_don, prob)
  if (i == 1){
    GraphData6 <- df
  } else {
    GraphData6 <- rbind(GraphData5, df)
  }
  print(paste0(prog, " done : ", i, "/", length(unique(Data2$programme))))
}

#saveRDS(GraphData6, "_SharedFolder_fondation-ulaval/Data/marts/delta_first_don_MIgrad.rds")

GraphData6 <- readRDS("_SharedFolder_fondation-ulaval/Data/marts/delta_first_don_MIgrad.rds")

## Graph 1: All programs -------------------------------------------------------------------

GraphData6 %>% 
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
  ggplot(aes(x = delta_first_don, y = prob*100,
             group = interaction(ses_female, ses_origin_qc),
             color = interaction(ses_female, ses_origin_qc))) +
  #geom_line(alpha = 0.2) +
  geom_smooth(method = "gam",
              aes(weight = n),
              se = F) +
  scale_y_continuous(limits = c(0,15)) +
  scale_x_continuous(breaks = seq(from=0,to=50,by=5)) +
  scale_color_discrete(labels = c(
    "Femme du Québec",
    "Homme du Québec",
    "Femme hors-Québec",
    "Homme hors-Québec"
  ),
  type = c("#FFC103", "#E30513", "black", "#1B9DDA")) +
  clessnverse::theme_clean_light() +
  ylab("Probabilité du<br>premier don (%)<br>") +
  xlab("<br>Nombre d'années depuis<br>le diplôme le plus important") +
  theme(axis.title.y = ggtext::element_markdown(hjust = 0.5, size = 18, lineheight = 1),
        axis.title.x = ggtext::element_markdown(hjust = 0.5, size = 18, lineheight = 1),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13))

ggsave("_SharedFolder_fondation-ulaval/graphs/delta_first_don_mi_all.png",
       width = 8, height = 6)


## Graph 2: by program -----------------------------------------------------
for (i in 1:length(unique(GraphData6$programme))){
  prog <- unique(GraphData6$programme)[i]
  d <- GraphData6 %>%
    filter(programme == prog)
  model <- mgcv::gam(prob ~ delta_first_don,
                     data = d,
                     weights = n)
  df <- data.frame(programme = prog,
                   delta_first_don = -1) %>% 
    mutate(gam_pred = predict(model, .),
           group = groups[programme])
  if (i == 1){
    gamd <- df
  } else {
    gamd <- rbind(gamd, df)
  }
}

gamd$programme <- clean_names[gamd$programme]


GraphData6 %>%
  mutate(group = groups[programme],
         programme = clean_names[programme]) %>% 
  ggplot(aes(
    y = prob*100,
    x = delta_first_don,
    group = programme,
    color = programme)) +
  geom_line(alpha = 0.1, show.legend = F) +
  facet_wrap( ~ group) +
  geom_smooth(se = F,
              method = "gam",
              aes(weight = n),
              show.legend = F) +
  scale_y_continuous(limits = c(0,12.5)) +
  scale_x_continuous(breaks = seq(from=0, to = 50, by = 5),
                     limits = c(-20, 55),
                     expand = c(0.1, 0.1)) +
  ggrepel::geom_label_repel(data = gamd,
                            aes(x = -12,
                                y = gam_pred*100,
                                label = programme,
                                fill = programme),
                            color = "white",
                            size = 3,
                            show.legend = F,
                            alpha = 0.8,
                            force = 2,
                            nudge_x = -1,
                            direction = "y") +
  ylab("Probabilité du<br>premier don (%)<br>") +
  xlab("<br>Nombre d'années depuis<br>le diplôme le plus important") +
  clessnverse::theme_clean_light() +
  theme(axis.title.y = ggtext::element_markdown(hjust = 0.5, size = 18, lineheight = 1),
        axis.title.x = ggtext::element_markdown(hjust = 0.5, size = 18, lineheight = 1),
        axis.text.x = element_text(size = 9),
        strip.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 13))

ggsave("_SharedFolder_fondation-ulaval/graphs/delta_first_don_mi_byProgram.png",
       width = 12, height = 9)

