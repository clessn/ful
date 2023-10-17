library(tidyverse)
library(clessnverse)
library(lubridate)
library(nnet)
library(randomForest)
library(forcats)

### Data
DataMod <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/marts/data_for_models.rds")

#### Graph 1 ####
# Compute the count for each comm_type
comm_type_counts <- DataGraph1 %>%
  group_by(comm_type) %>%
  summarize(n = n()) %>%
  mutate(label = paste0(comm_type, " (n = ", n, ")"))

# Add the counts to the original data
DataGraph1 <- left_join(DataGraph1, comm_type_counts, by = "comm_type")

# Wrangling 
DataGraph1 <- DataGraph1 %>%
  select(UL_NO_CODE, label, date_comm)

p <- ggplot(DataGraph1, aes(x = date_comm)) +
  geom_density(aes(y = ..scaled..), fill = "#E30513", alpha = 0.7, color = NA, adjust = 2) +
  labs(y = "Densité (échelle)",
       title = "Évolution de la densité des communications par catégorie") +
  facet_wrap( ~ label, scales = "free_y", nrow = 2) +
  scale_x_date(breaks = as.Date(c("2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01")), 
               date_labels = "%Y") +
  theme_clean_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  )

print(p)

ggsave("_SharedFolder_fondation-ulaval/graphs/facetWrapCommType.png",
       width = 18, height = 12)

#### Graph 2 ####
### Wrangling ###
decade <- (DataMod$education_yearFirstGraduation %/% 10) * 10

df <- expand.grid(rolling_n_sollicitation = unique(DataMod$rolling_n_sollicitation), education_yearFirstGraduation = unique(decade)) %>% 
  drop_na() %>% 
  mutate(ses_female = 0.5,
         Programme = "Administration",
         education_yearMIGraduation = 2015,
         ro_athlete = 0.5,
         ses_origin = factor("qc"),
         rosport_NATATI = 0.5,
         rosport_ATHLET = 0.5,
         rosport_BASKET = 0.5,
         rosport_VOLLEY = 0.5,
         rosport_FOOTBA = 0.5,
         rosport_SOCCER = 0.5,
         `rosport_R&OGEN` = 0.5,
         rosport_SKIRO = 0.5,
         rosport_HOCKEY = 0.5,
         rosport_XCOUNT = 0.5,
         rosport_BADMIN = 0.5,
         rosport_RUGBY = 0.5,
         rosport_HANDBA = 0.5,
         rosport_TENNIS = 0.5,
         rosport_GOLFRO = 0.5,
         rolling_n_sollicitation = factor(rolling_n_sollicitation), 
         rolling_n_fidelisation = factor(mean(DataMod$rolling_n_fidelisation, na.rm = T)),
         rolling_n_evenement = factor(mean(DataMod$rolling_n_evenement, na.rm = T)),
         rolling_n_remerciement = factor(mean(DataMod$rolling_n_remerciement, na.rm = T)),
         rolling_n_sondage = factor(mean(DataMod$rolling_n_sondage, na.rm = T)),
         comm_type = factor("sollicitation"))

#levels(df$ses_origin) <- levels(DataMod$ses_origin)
#levels(df$comm_type) <- levels(DataMod$comm_type)

df$ses_origin <- fct_expand(df$ses_origin, model$forest$xlevels$ses_origin)
df$comm_type <- fct_expand(df$comm_type, model$forest$xlevels$comm_type)
df$rolling_n_sollicitation <- fct_expand(df$rolling_n_sollicitation, model$forest$xlevels$rolling_n_sollicitation)
df$rolling_n_evenement <- fct_expand(df$rolling_n_evenement, model$forest$xlevels$rolling_n_evenement)
df$rolling_n_remerciement <- fct_expand(df$rolling_n_remerciement, model$forest$xlevels$rolling_n_remerciement)
df$rolling_n_fidelisation <- fct_expand(df$rolling_n_fidelisation, model$forest$xlevels$rolling_n_fidelisation)
df$rolling_n_sondage <- fct_expand(df$rolling_n_sondage, model$forest$xlevels$rolling_n_sondage)



model <- readRDS("_SharedFolder_fondation-ulaval/Data/pipeline/marts/models/Administration.rds")

df$pred <- predict(model, newdata = df)

### Graph ###

p2 <- ggplot(df, aes(x = rolling_n_sollicitation, y = prob_donating, color = as.factor(decade))) +
  geom_line(aes(group = decade)) +
  labs(title = "Probabilité de donner selon le nombre de sollicitations et la décennie",
       x = "Nombre de sollicitations",
       y = "Probabilité de donner",
       color = "Décennie") +
  theme_clean_light()  

print(p2)

ggsave("_SharedFolder_fondation-ulaval/graphs/probSollDecade.png",
       width = 12, height = 9)
