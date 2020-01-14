# Upload the data ---------------------------------------------------------

nz_bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")

# Upload the packages -----------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggplot2)

# See Top 10 --------------------------------------------------------------

data1<-nz_bird%>%filter()%>% 
  group_by(bird_breed)%>%
  summarize(n=n())%>%top_n(10)

# Prepare the data for graph ----------------------------------------------

data<-nz_bird%>%filter(vote_rank == "vote_1" & bird_breed %in% c("Antipodean Albatross","Banded Dotterel","Black Robin","Blue Duck","Fantail", "Kākāpō","Kea","Kererū","New Zealand Falcon","Yellow-eyed penguin"))%>%
  mutate(Day=weekdays(as.Date(date))) %>%
  group_by(bird_breed,Day) %>%  summarize(n=n()) 


data$Day =  fct_relevel(data$Day,  c("Monday",
                                     "Tuesday",
                                     "Wednesday",
                                     "Thursday",
                                     "Friday",
                                     "Saturday",
                                     "Sunday"))

# Heatmap -----------------------------------------------------------------

data %>% 
  ggplot(aes(x = Day, y = fct_reorder(bird_breed,n))) +
  geom_tile(aes(fill = n), color = "#2b2b2b") +
  geom_text(aes(label = n), color = "#22292F") +
  scale_fill_gradient(low = "#20b2aa", high = "#2072b2") +
  labs(x = "",y = "",
       title = "New Zealand Bird of the Year - Top 10",
       subtitle = "Which bird breed is crowned **Bird of the Year** the most on each day of the week?",
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