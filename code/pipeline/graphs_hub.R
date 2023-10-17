# Packages ----------------------------------------------------------------
library(tidyverse)
#library(randomForest)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/marts/data_for_models.rds")

# prob first don ------------------------------------------------------

# VIs: programs, decade, rolling_n_sollicitation
decade <- (Data$education_yearFirstGraduation %/% 10) * 10
programs <- unique(Data$Programme)
n_soll <- unique(Data$rolling_n_sollicitation)

model <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/marts/models/glms/Administration.rds")

df <- expand_grid(education_yearFirstGraduation = unique(decade),
                  Programme = programs,
                  rolling_n_sollicitation = n_soll) %>% 
  mutate(comm_type = "sollicitation",
         ses_female = 0.5,
         education_yearMIGraduation = 2015,
         ro_athlete = 0.5,
         ses_origin = "qc",
         rosport_NATATI = 0,
         rosport_ATHLET = 0,
         rosport_BASKET = 0,
         rosport_VOLLEY = 0,
         rosport_FOOTBA = 0,
         rosport_SOCCER = 0,
         `rosport_R&OGEN` = 0,
         rosport_SKIRO = 0,
         rosport_HOCKEY = 0.5,
         rosport_XCOUNT = 0.5,
         rosport_BADMIN = 0.5,
         rosport_RUGBY = 0.5,
         rosport_HANDBA = 0.5,
         rosport_TENNIS = 0.5,
         rosport_GOLFRO = 0.5,
         rolling_n_fidelisation = mean(Data$rolling_n_fidelisation, na.rm = TRUE),
         rolling_n_evenement = mean(Data$rolling_n_evenement, na.rm = TRUE),
         rolling_n_remerciement = mean(Data$rolling_n_remerciement, na.rm = TRUE),
         rolling_n_sondage = mean(Data$rolling_n_sondage, na.rm = TRUE)) %>% 
  drop_na()

df$rolling_n_sollicitation <- factor(df$rolling_n_sollicitation, levels = model$xlevels[['rolling_n_sollicitation']])
df$rolling_n_fidelisation <- factor("2", levels = model$xlevels[['rolling_n_fidelisation']])
df$rolling_n_evenement <- factor("2", levels = model$xlevels[['rolling_n_evenement']])
df$rolling_n_remerciement <- factor("2", levels = model$xlevels[['rolling_n_remerciement']])

df$rolling_n_fidelisation <- "1"
df$rolling_n_evenement <- "1"
df$rolling_n_remerciement <- "1"

df <- df %>% drop_na()

programs_loop <- programs[-1]

for (i in 1:length(programs_loop)){
  progi <- programs_loop[i]
  model <- readRDS(paste0("_SharedFolder_fondation-ulaval/Data/pipeline/marts/models/glms/", progi, ".rds"))
  if (i == 1){
    graph <- df %>% 
      filter(Programme == progi) %>% 
      mutate(pred = predict(model, newdata = ., type = "response"))
  } else {
    graph <- df %>% 
      filter(Programme == progi) %>% 
      mutate(pred = predict(model, newdata = ., type = "response")) %>% 
      rbind(graph, .)
  }
  print(i)
  print(progi)
}


t <- graph %>% 
  group_by(Programme) %>% 
  summarise(m = round(mean(pred),2))

graph %>% 
  filter(!(Programme %in% c("Philosophie",
                            "Diplôme multidisciplinaire ou collège universitaire",
                            "Aménagement, architecture, art et design")) &
           education_yearFirstGraduation >= 1950) %>%
  ggplot(aes(x = as.numeric(rolling_n_sollicitation), y = pred)) +
  geom_smooth(se = FALSE, color = "black",
              linewidth = 1) +
  facet_wrap(~Programme, scales = "free_y")


# Prob outcome reussi -----------------------------------------------------

n_soll <- unique(Data$rolling_n_sollicitation)

model <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/marts/models/modele_predict_outcome.rds")

df <- expand_grid(rolling_n_sollicitation = 1:15,
                  has_donated_asof_date = c(0,1)) %>% 
  mutate(comm_type = "sollicitation",
         Methode = "tel",
         education_yearFirstGraduation = 2000,
         ses_female = 0.5,
         Programme = "Médecine",
         education_yearMIGraduation = 2015,
         ro_athlete = 0.5,
         ses_origin = "qc",
         rosport_NATATI = 0,
         rosport_ATHLET = 0,
         rosport_BASKET = 0,
         rosport_VOLLEY = 0,
         rosport_FOOTBA = 0,
         rosport_SOCCER = 0,
         `rosport_R&OGEN` = 0,
         rosport_SKIRO = 0,
         rosport_HOCKEY = 0.5,
         rosport_XCOUNT = 0.5,
         rosport_BADMIN = 0.5,
         rosport_RUGBY = 0.5,
         rosport_HANDBA = 0.5,
         rosport_TENNIS = 0.5,
         rosport_GOLFRO = 0.5) %>% 
  drop_na()

df$rolling_n_sollicitation <- factor(df$rolling_n_sollicitation, levels = model$xlevels[['rolling_n_sollicitation']])

df$pred <- predict(model, newdata = df, type = "response")


ggplot(df, aes(x = as.numeric(rolling_n_sollicitation), y = pred)) +
  geom_smooth(aes(group = has_donated_asof_date,
                  color = has_donated_asof_date),
              span = 0.5)
