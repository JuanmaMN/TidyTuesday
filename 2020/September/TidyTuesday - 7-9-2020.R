# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2,  ggfittext, patchwork, 
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, cowplot)



# Upload the data ---------------------------------------------------------



friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')



# Prepare the data --------------------------------------------------------



lighter <- "#d3d3d3"
darker<-"#3a9fbf"

data1<-friends_info%>% group_by(season) %>%
  summarize(rating=round(mean(imdb_rating, na.rm=TRUE),2),
            viewings_pe_episode=round(sum(us_views_millions/sum(episode), na.rm=TRUE),2),
            number_episodes=n()) %>% ungroup()%>%
  mutate(fill = case_when(
    season %in% c(1,2,3,4,5,6,7,8,9)  ~ lighter,
    season == 10 ~ darker,
    T ~ "other"
  ))




# First graph -------------------------------------------------------------


p1 <- ggplot(data1, aes(season, viewings_pe_episode), group = 1) +
              geom_area(position = 'identity', alpha = .5, fill = "#d3d3d3") +
              geom_line(linetype = "dotted") +
              geom_point(aes(fill = season), shape = 21, size = 3, show.legend = F) +
        geom_text(aes(label =paste0(round(viewings_pe_episode,2), " ", "M")), data = data1, size = 3.5, vjust = -1.8) +
    coord_cartesian(ylim=c(1.5,3), xlim=c(0.5,10.5))+
  geom_point(x= 1, y = 1.98, size=3, shape=21, fill="#888e8c") +
  geom_point(x= 2, y =2.54,size=3, shape=21, fill="#888e8c") +
  geom_point(x= 3, y = 2.02,size=3, shape=21, fill="#888e8c")+
  geom_point(x= 4, y = 2,size=3, shape=21, fill="#888e8c")+
  geom_point(x= 5, y = 1.98,size=3, shape=21, fill="#888e8c")+
  geom_point(x= 6, y = 1.74, size=3, shape=21, fill="#888e8c") +
  geom_point(x= 7, y =1.76,size=3, shape=21, fill="#888e8c") +
  geom_point(x= 8, y = 2.14,size=3, shape=21, fill="#888e8c")+
  geom_point(x= 9, y = 1.91,size=3, shape=21, fill="#888e8c")+
  geom_point(x= 10, y = 2.75,size=3, shape=21, fill="#3a9fbf")+
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  labs(x = "",y = "",
       title = "")+
  guides(fill = NULL) +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 14,
                              hjust = 0.5,
                              family = "Arial"),
    axis.title.x = element_text(margin = margin(t = 10),
                                color = "#22222b"),
    axis.title.y = element_text(margin = margin(r = 15), 
                                color = "#22222b"),
    #axis.text.x    = element_blank(),
    axis.text.y    =  element_blank(),
    legend.position = "none",
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank(),
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.x =  element_blank(),
    panel.grid.minor = element_blank(), 
    strip.text.x = element_blank(),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  ) 




# Second graph ------------------------------------------------------------


p2<-data1%>%ggplot(aes(season, rating), label=rating) +
  geom_col(aes(fill = fill),alpha = .5) + 
  scale_fill_identity()+
  coord_cartesian(ylim=c(8,8.75), xlim=c(0.5,10.5)) +
  geom_text(position = position_dodge(0.9), 
            vjust = -0.9,
            color = "black", size = 3.5, aes(label=rating)) +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  labs(x = "",y = "",
       title = "")+
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
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #axis.text.x    = element_blank(),
    axis.text.y    =  element_blank(),
    legend.position = "none",
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank(),
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.x =  element_blank(),
    panel.grid.minor = element_blank(), 
    strip.text.x = element_blank(),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  ) 



# patchwork ---------------------------------------------------------------


PTT<-p1/p2


PTT2 <- PTT + plot_annotation(title = "Friends",
                              subtitle = "Average millions of viewers per episode and IMBD rating per season in The USA ",
                              caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)",
                              theme = theme(plot.title = element_text(margin = margin(t=15,b = 8), 
                                                                      color = "#000000",face = "bold",size = 14,
                                                                      hjust = 0.5,
                                                                      family = "Arial"),
                                            plot.subtitle = element_text(margin = margin(t=10, b = 25), 
                                                                         color = "#000000", size = 10, family = "Arial",
                                                                         hjust = 0.5),
                                            plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                                                         color = "#000000", size = 8, family = "Arial",
                                                                         hjust = 0.95),
                                            plot.background = element_rect(fill = "#f7f7f7"),
                                            panel.border = element_blank()))



PTT2




            
