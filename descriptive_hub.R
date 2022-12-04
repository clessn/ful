library(tidyverse)

# Police for plots --------------------------------------------------------
sysfonts::font_add_google("Roboto")
showtext::showtext_auto()


Data <- readRDS("_SharedFolder_fondation-ulaval/Data/CleanData.rds")

# 1. first graduation

ggplot(Data, aes(x = education_yearFirstGraduation)) +
  geom_histogram(binwidth = 1, fill = "#E30513",
                 color = "#E30513", alpha = 0.5) +
  scale_x_continuous(limits = c(1930, 2022)) +
  xlab("\nAnnée de promotion\ndu premier diplôme\n") +
  ylab("\nNombre d'étudiants\n") +
  clessnverse::theme_clean_light(base_size = 30) +
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_line(linewidth = 0.2),
        text = element_text(lineheight = 0.3),
        axis.title.x = element_text(size = 45),
        axis.title.y = element_text(size = 45))

ggsave("_SharedFolder_fondation-ulaval/pres_2022-12-06/graphs/2.2.distributionDiploma.png",
       width = 8, height = 6)

# 1.1 first graduation ~ historic_dateFirstDonation


