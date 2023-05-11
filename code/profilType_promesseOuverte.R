
library(tidyverse)

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
  geom_bar(stat = "identity", aes(fill = ses_origin_qc)) +
  theme()

