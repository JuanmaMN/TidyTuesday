# Upload ------------------------------------------------------------------

tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")


# Packages ----------------------------------------------------------------

library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(ggplot2)
library(scales)




# Prepare the data --------------------------------------------------------

data<-tickets%>% mutate(month=format(issue_datetime,"%B")) %>% group_by(month,issuing_agency) %>%
  summarize(fine=sum(fine)) %>% filter(!is.na(issuing_agency))

data$month =  fct_relevel(data$month,  c("January",
                                         "February",
                                         "March",
                                         "April",
                                         "May",
                                         "June",
                                         "July",
                                         "August",
                                         "September",
                                         "October",
                                         "November",
                                         "December"))


data %>% 
  ggplot(aes(x = month, y = fct_reorder(issuing_agency,fine))) +
  geom_tile(aes(fill = fine), color = "#2b2b2b") +
  geom_text(aes(label = dollar_format()(fine)), color = "#22292F") +
  scale_fill_gradient(low = "#ececc2", high = "#20b2aa")+
  labs(x = "",y = "",
       title = "Philadelphia Parking Violations",
       subtitle = "Which agency issued the highest amount of fines in $?",
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
                                color = "#f7f7f7"),
    legend.position = "none",
    axis.text.x    = element_text(color = "#22222b", margin = margin(t = 15)),
    axis.text.y    = element_text(color = "#22222b"),
    axis.title.y = element_text(margin = margin(r = 15), color = "#f7f7f7"),
    panel.background = element_blank(), panel.grid.major = element_blank(),
    #panel.grid.major.y = element_line(color = "#dbdbdb"),
    #panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7"),
    plot.margin = unit(c(1, 2, 1, 1), "cm")
  ) 







