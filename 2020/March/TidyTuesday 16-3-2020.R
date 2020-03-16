# Upload the data ---------------------------------------------------------

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

# Upload the packages -----------------------------------------------------

library(tidyverse)
library(ggthemes)
library(scales)
library(ggplot2)

# Prepare the data --------------------------------------------------------

data<-office_ratings %>% group_by(season)%>%summarize(tota_votes=sum(total_votes))


data$season <- factor(data$season) %>%
  fct_reorder(data$tota_votes)





# Graph -------------------------------------------------------------------


graph <-ggplot(data, aes(season, tota_votes)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  guides(fill=FALSE)+
  geom_text(aes(label = comma(tota_votes)), hjust = -0.2, color = "#000000",size = 3) +
  labs(x = "Season",y = "Total number of votes by users",
       title = "The Office - Which season has the highest number of votes?",
       subtitle = "Highest number of votes by users in Season 2. Most engaging Season measured by number of votes",
       caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)")+
  guides(fill = NULL)+
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 14,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#22222b", size = 10, family = "Arial",
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10, family = "Arial",
                                 hjust = 0.95),
    axis.title.x = element_text(margin = margin(t = 10),
                                color = "#22222b"),
    axis.title.y = element_text(margin = margin(r = 15), 
                                color = "#22222b"),
    legend.position = "none",
    axis.text.x    = element_blank(),
    axis.text.y    = element_text(color = "#22222b"),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f4efe1"),
    plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank()
  ) + scale_y_continuous(label = comma_format())
  
graph

