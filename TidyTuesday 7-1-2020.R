
# Upload data -------------------------------------------------------------

temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')


# Upload the packages -----------------------------------------------------

library(ggplot2)
library(tidyverse)
library(ggfittext)


# Prepare the data --------------------------------------------------------

temperatureperth <- filter(temperature, city_name == "PERTH")%>%select(1:4)

temperatureperth2<-temperatureperth %>% spread(temp_type, temperature) %>%
  mutate(diff=max-min) %>% select(1,2,5) %>% top_n(10, diff)

temperatureperth2$date <- factor(temperatureperth2$date) %>%
  fct_reorder(temperatureperth2$diff)

temperatureperth2$date<-format(as.POSIXct(temperatureperth2$date, format="%Y-%m-%d"),format="%d %B %Y")


# Graph -------------------------------------------------------------------

pAustralia <- temperatureperth2 %>% 
  ggplot(aes(date, diff, label = diff, fill = diff)) +
  scale_fill_gradient(low = "#20b2aa", high = "#2072b2") +
  geom_col() +
  geom_bar_text(place = "right", contrast = TRUE, size=10) + 
  coord_flip() +
  labs(x = "",y = "",
       title = "Climate Data - Australia",
       subtitle = "Top 10 days with the biggest difference in temperature on the day",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  #scale_x_discrete(position = "top") +
  guides(fill = NULL) +
  theme(
    plot.title = element_text(margin = margin(b = 10), 
                              color = "#22222b",face = "bold",size = 14,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(b = 25), 
                                 color = "#22222b", size = 12, family = "Arial"),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10, family = "Arial"),
    #axis.title.x = element_text(margin = margin(t = 15),color = "#f7f7f7"),
    legend.position = "none",
    axis.text.x  = element_blank(),
    axis.text.y    = element_text(color = "#22222b"),
    #axis.title.y = element_text(margin = margin(r = 15), color = "#f7f7f7"),
    axis.title.x = element_text(margin = margin(r = 15), color = "#f7f7f7"),
    panel.background = element_blank(), panel.grid.major = element_blank(),
    #panel.grid.major.y = element_line(color = "#dbdbdb"),
    #panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7"),
    plot.margin = unit(c(1, 2, 1, 1), "cm"),
    axis.ticks = element_blank()
  ) 


pAustralia

