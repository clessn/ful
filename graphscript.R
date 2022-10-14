library(ggplot2)
library(readxl)

Data <- read_xlsx("Dropbox/Travail/CLESSN/01x_fondation-ulaval/_SharedFolder_fondation-ulaval/snowballing/centralite_2022-02-03.xlsx") %>%
  mutate(id = gsub(",", "", id))

Data$id[10] <- "Baade1996a"
Data$id[36] <- "Baade1996b"
Data$id[45] <- "Grant1986a"
Data$id[46] <- "Grant1986b"

ggplot(Data, aes(x = centralite, y = reorder(id, centralite))) + 
  geom_point()
