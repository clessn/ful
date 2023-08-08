# Packages

library(tidyverse)

# Data

CleanData <- readRDS("_SharedFolder_fondation-ulaval/Data/CleanData.rds")

NewData <- CleanData %>%
  select(id, starts_with("ses_"), starts_with("education_"), historic_donationLevel, historic_lastDonation_amount)

NewData$donator <- 0
NewData$donator[CleanData$historic_lastDonation_amount > 0] <- 1


hist(NewData$donator)
hist(NewData$fdonator)
# analysis

# Police for plots --------------------------------------------------------
sysfonts::font_add_google("Roboto")
showtext::showtext_auto()

GraphData <- NewData %>%
  group_by(donator, ses_female) %>%
  summarise(n = n()) %>%
    drop_na() %>%  
    mutate(ndonator = sum(n), prop = n / ndonator*100, ses_female = as.character(ses_female))

facet_labels <- c("0" = "Non-donateurs", "1" = "Donateurs")

GraphData$donator <- facet_labels[as.character(GraphData$donator)]

ggplot(GraphData, aes(x = ses_female, y = prop)) +
  geom_bar(stat = "identity", aes(fill = ses_female, colour = ses_female, alpha = 40), show.legend = FALSE) +
  facet_wrap(~ donator) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_hline(yintercept = 50, linetype = "dotted",
             color = "#5B5B5B") +
  scale_x_discrete(breaks = c(0.0, 1.0), label = c("Hommes", "Femmes")) +
  labs(y = "\nProportion (%)\n", x = "") +
  scale_fill_manual(values = c("#FFC103", "#E30513")) +
  scale_colour_manual(values = c("#FFC103", "#E30513")) +
  clessnverse::theme_clean_light(base_size = 30) +
  theme(strip.background.x = element_rect(fill = "#D3D3D3", color = NA),
        text = element_text(lineheight = 0.3),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(size = 60),
        axis.text.x = element_text(size = 50),
        axis.text.y = element_text(size = 50),
        strip.text.x = element_text(size = 50))


ggsave("_SharedFolder_fondation-ulaval/pres_2022-12-06/graphs/2.1_gender.png",
       width = 7, height = 5) 

