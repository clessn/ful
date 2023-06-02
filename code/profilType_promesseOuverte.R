
library(tidyverse)
library(clessnverse)

Data <- readRDS("_SharedFolder_fondation-ulaval/Data/CleanData.rds") %>% 
  select(ses_female, ses_origin_qc, education_yearFirstGraduation, 
         education_yearMIGraduation, starts_with("education_FirstDegree"),
         prospectif_openPledges_number) %>%
  mutate(prospectif_openPledges_number = ifelse(prospectif_openPledges_number == 2, 1, prospectif_openPledges_number)) 
table(Data$prospectif_openPledges_number)

Graph <- Data %>%
  group_by(ses_female, ses_origin_qc) %>% 
  summarise(n_prom = sum(prospectif_openPledges_number, na.rm = T),
            n_total = n()) %>% 
  mutate(prop = n_prom/n_total * 100) %>% 
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
  scale_y_continuous(limits = c(0, 1.5), breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)) +
  labs(title = "Dons planifiés") +
  ylab("Proportion (%) \n" ) +
  xlab(element_blank()) +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        title = element_text(size = 20))


ggsave("_SharedFolder_fondation-ulaval/graphs/profilsDonsPlanifiés.png",
       width = 12, height = 9)  

