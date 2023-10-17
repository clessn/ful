library(tidyverse)
library(clessnverse)
library(lubridate)
library(nnet)

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
  geom_density(aes(y = ..scaled..), fill = "#E30513", alpha = 0.7, color = NA) +
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


