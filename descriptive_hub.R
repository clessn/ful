library(tidyverse)

# Police for plots --------------------------------------------------------
sysfonts::font_add_google("Roboto")
showtext::showtext_auto()


Data <- readRDS("_SharedFolder_fondation-ulaval/Data/CleanData.rds")

# 1. first graduation

ggplot(Data, aes(x = education_yearFirstGraduation)) +
  geom_histogram(binwidth = 1, fill = "#E30513",
                 color = "#E30513", alpha = 0.5) +
  scale_x_continuous(limits = c(1930, 2022),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  xlab("\nAnnée de promotion   \ndu premier diplôme   \n") +
  ylab("\nNombre d'étudiants   \n") +
  clessnverse::theme_clean_light(base_size = 30) +
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_line(linewidth = 0),
        text = element_text(lineheight = 0.3),
        axis.title.x = element_text(size = 45),
        axis.title.y = element_text(size = 45),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30))

ggsave("_SharedFolder_fondation-ulaval/pres_2022-12-06/graphs/2.2.1.distributionDiploma.png",
       width = 8, height = 5)

# 1.1 first graduation ~ historic_dateFirstDonation

class(Data$education_yearFirstGraduation)
class(Data$historic_dateFirstDonation)

Data$yearFirstDonation <- as.numeric(format(Data$historic_dateFirstDonation, "%Y"))
class(Data$yearFirstDonation)

Data$gap_Dipl_FirstDon <- Data$yearFirstDonation - Data$education_yearFirstGraduation

ggplot(Data, aes(x = gap_Dipl_FirstDon)) +
  geom_histogram(binwidth = 1, fill = "#FFC103",
                 color = "#FFC103", alpha = 0.5) +
  scale_x_continuous(limits = c(-10, 65),
                     breaks = seq(-10, 65, by = 5)) +
  xlab("\nNombre d'années entre le premier diplôme\net le premier don\n") +
  ylab("\nNombre d'étudiants\n") +
  clessnverse::theme_clean_light(base_size = 30) +
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_line(linewidth = 0),
        text = element_text(lineheight = 0.3),
        axis.title.x = element_text(size = 45),
        axis.title.y = element_text(size = 45),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30))

Data %>% 
  mutate(betw0_5 = ifelse(gap_Dipl_FirstDon >= 0 & gap_Dipl_FirstDon <= 5, 1, 0)) %>% 
  group_by(betw0_5) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  mutate(n_all = sum(n),
         prop = round(n/n_all*100, 2))

ggsave("_SharedFolder_fondation-ulaval/pres_2022-12-06/graphs/2.2.2.diplomaFirstDon.png",
       width = 8, height = 5)
