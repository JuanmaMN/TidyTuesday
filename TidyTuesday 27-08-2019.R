
# Upload the dataset ------------------------------------------------------

simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")


simpsons2<-simpsons



# Upload the packages -----------------------------------------------------

library(ggplot2)
library(tidyverse)#
library(plotly)
library(hrbrthemes)


# Top 10 ------------------------------------------------------------------

top_10<-simpsons%>% group_by(guest_star)%>%summarise(n=n())%>%top_n(10, wt=n)
View(top_10)
unique(top_10$guest_star)


# Group the seasons -------------------------------------------------------

simpsons2$season[simpsons2$season %in% c(1:10)] <- "First 10 seasons"
simpsons2$season[simpsons2$season %in% c(11:20)] <- "Season 11-20"
simpsons2$season[simpsons2$season %in% c(21:30)] <- "Season 21-30"




top<-simpsons2 %>%
  filter(guest_star %in% c("Marcia Wallace",  
                           "Phil Hartman",
                           "Joe Mantegna",
                           "Maurice LaMarche",
                           "Frank Welker",
                           "Kelsey Grammer",
                           "Jon Lovitz",
                           "Kevin Michael Richardson",
                           "Jackie Mason",
                           "Glenn Close"))%>% 
  group_by(season,guest_star)%>%summarise(n=n())




View(top)

top$guest_star <- factor(top$guest_star, levels = c(
  "Glenn Close",
  "Jackie Mason",
  "Kevin Michael Richardson",
  "Jon Lovitz",
  "Kelsey Grammer",
  "Frank Welker",
  "Maurice LaMarche",
  "Joe Mantegna",
  "Phil Hartman",
  "Marcia Wallace"))



gS<- ggplot(top, aes(guest_star)) +
  geom_bar(aes(y = n, fill = season),stat="identity") +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  theme_ipsum_tw()  + 
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="bottom",
    axis.text = element_text( size=48 )
  ) +
  ylim(0,180) +
  ylab("Total Episodes") +
  xlab("") + 
  theme(legend.title = element_blank()) +
  labs(
    title = "Simpsons Guest Stars - Top 10",
    subtitle = "Total Guest Stars per season",
    caption = "\n Source: TidyTuesday 27.8.2019
      Visualization: JuanmaMN (Twitter @Juanma_MN)")
gS


ggplotly(gS)







