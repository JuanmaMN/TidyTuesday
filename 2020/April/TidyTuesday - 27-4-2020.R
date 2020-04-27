# Upload packages ---------------------------------------------------------

library(tidyverse)
library(lubridate)
library(scales)

# Upload data -------------------------------------------------------------

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv')


# Prepare the data --------------------------------------------------------

grosses2<-grosses%>%mutate(month=format(week_ending,"%B")) %>% 
  mutate(year = ifelse(week_ending <= "1999-12-31", Before, After)) %>%
  group_by(year, month)%>%
  summarize(total_seats_sold=sum(seats_sold, na.rm=TRUE))

grosses2$month <-  fct_relevel(grosses2$month, 
                                         c("January",
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





# Graph -------------------------------------------------------------------


g2<-grosses2 %>%
  ggplot(aes(month,total_seats_sold, group=year, col=factor(year))) +
  geom_line(size=1.5,linetype = "solid") + 
  geom_point(size=4, shape=21, fill="#f0f0f0") +
  scale_color_manual(name = "", labels = c("Before 2000", "2000 and after"), values=c("#20b2aa", "#44a6c6")) +
  scale_y_continuous(label = unit_format(unit = "m", scale = 1e-6), breaks=c(0,5000000,10000000,15000000,20000000, 25000000))+
  scale_x_discrete(position = "bottom")+
  labs(x = "",y = "",
       title = "Broadway Weekly Grosses - Monthly Analysis",
       subtitle = "Total seats sold for all performances and previews - Before and after 2000",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 14,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#22222b", size = 9, family = "Arial",
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10, family = "Arial",
                                 hjust = 0.95),
    axis.title.x = element_text(margin = margin(t = 10),
                                color = "#22222b"),
    axis.title.y = element_text(margin = margin(r = 15), 
                                color = "#22222b"),
    legend.position = c(0.9,0.1),
    legend.direction = "horizontal",
    axis.text.x    = element_text(color = "#22222b"),
    axis.text.y    = element_text(color = "#22222b"),
    panel.background = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7"),
    plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank(),
    legend.key = element_rect(fill = "#f7f7f7"),
    legend.background = element_rect(fill="#f7f7f7")
  ) 

g2
