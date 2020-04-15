# Upload the packages -----------------------------------------------------

library(scales)
library(tidyverse)
library(patchwork)


# Raw data ----------------------------------------------------------------

rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

View(rankings)


# Prepare the data --------------------------------------------------------

rankings_chart_year<-rankings%>% group_by(year) %>%
  summarise(total_points=sum(points))

rankings_chart_year_2<-rankings%>% group_by(year,gender) %>% filter(gender !="mixed")%>% 
  summarise(avg_points=sum(points)/sum(n))


# ribbon ------------------------------------------------------------------


g1<-ggplot(rankings_chart_year, aes(x = year, y = total_points)) + 
  geom_ribbon(aes(ymax = total_points, ymin = 0), 
              fill = "#ade6d8", alpha = 0.7) +
  geom_line(color = "#6F213F") +
  scale_x_continuous(
    breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015),
    limits = c(1979, 2019),
    expand = c(0, 0)
  ) +
  scale_y_continuous(limits = c(0, 350),
                     expand = c(0, 0)) +
  labs(x = "",y = "",
       title = "Total number of points",
       subtitle = " ",
       caption = "") +
  guides(fill = NULL) +
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
    legend.position = "none",
    axis.text.x    = element_text(color = "#22222b"),
    axis.text.y    = element_text(color = "#22222b"),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7"),
    #plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank()
  ) + 
  geom_point(x= 1994, y = 308,size=4, shape=21, fill="#CB454A")  +
  annotate("text", x =  2000, y =265,fontface =2,
         hjust = 0.5, color = "#CB454A",
         size = 2.5, label = paste0("The Notorious B.I.G. - 140 points \n Nas - 46 points  \n Nas ft. A.Z. - 20 points")) + 
  annotate("text", x =  2000, y = 300,fontface =2,
           hjust = 0.5, color = "#000000",
           size = 2.5, label = paste0("1994 - Highest number of points - 308"))


g2<-ggplot(rankings_chart_year_2, aes(x = year, y = avg_points)) + 
  
  geom_ribbon(aes(ymax = avg_points, ymin = 0), 
              fill = "#add8e6", alpha = 0.7) +
  
  geom_line(color = "#6F213F") +
  
  scale_y_continuous(expand = expand_scale(mult = 0)) +

  scale_x_continuous(
    breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015),
    limits = c(1979, 2019),
    expand = c(0, 0)
  )+
  
  labs(x = "",y = "",
       title = "Average number of points per vote",
       subtitle = "",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  guides(fill = NULL) +
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
    legend.position = "none",
    axis.text.x    = element_text(color = "#22222b"),
    axis.text.y    = element_text(color = "#22222b"),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7"),
    axis.ticks = element_blank()
  )  + 
  geom_hline(yintercept = 5, color = "red1", size = 0.7) 



patchwork <- g1 / g2
patchwork







