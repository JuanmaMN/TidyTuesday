# Upload the packages -----------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(hrbrthemes)

# Data --------------------------------------------------------------------

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')


# Prepare the data --------------------------------------------------------

country<-food_consumption %>% group_by(country)%>%summarise(total=sum(co2_emmission))%>%top_n(10)%>%select(1)

data2<-food_consumption%>%select(1,2,4)%>% inner_join(country)



# Graph -------------------------------------------------------------------

data2 %>% 
  ggplot(aes(x = country, y = fct_reorder(food_category,co2_emmission))) +
  geom_tile(aes(fill = co2_emmission), color = "#2b2b2b") +
  geom_text(aes(label = co2_emmission), color = "#22292F") +
  scale_fill_gradient(low = "#20b2aa", high = "#2072b2") +
  labs(x = "",y = "",
       title = "Top 10 countries with the highest CO2 emissions",
       subtitle = "On which food category do they emit the highest level of CO2?",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  scale_x_discrete(position = "top") +
  guides(fill = NULL) +
  theme(
    plot.title = element_text(margin = margin(b = 10), 
                              color = "#22222b",face = "bold",size = 17,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(b = 25), 
                                 color = "#22222b", size = 12, family = "Arial"),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10, family = "Arial"),
    axis.title.x = element_text(margin = margin(t = 15),
                                color = "#e7e7e7"),
    legend.position = "none",
    axis.text.x    = element_text(color = "#22222b", margin = margin(t = 15)),
    axis.text.y    = element_text(color = "#22222b"),
    axis.title.y = element_text(margin = margin(r = 15), color = "#e7e7e7"),
    panel.background = element_blank(), panel.grid.major = element_blank(),
    panel.grid.major.y = element_line(color = "#dbdbdb"),
    panel.grid.minor = element_blank(), plot.background = element_rect(fill = "#e7e7e7"),
    plot.margin = unit(c(1, 2, 1, 1), "cm")
  ) 
