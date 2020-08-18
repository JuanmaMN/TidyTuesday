

# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr)



# Upload the data ---------------------------------------------------------

threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')


# Prepare the data for first graph ----------------------------------------

threats$continent <- recode(threats$continent, "North America" = "N.America", 
                            "South America" = "S.America")

threats1<-threats%>%filter(threat_type !="Unknown")%>%
        group_by(threat_type, continent)%>%  summarize(sum=sum(threatened))

threats2_forgraph<-threats1%>% group_by(continent)%>% mutate(perc=sum/sum(sum)) %>% filter (sum >0)


threats2_forgraph$threat_type =  fct_relevel(threats2_forgraph$threat_type , 
                                         c("Transportation Corridor",
                                           "Pollution",
                                           "Geological Events",
                                           "Human Intrusions",
                                           "Climate Change",
                                           "Energy Production & Mining",
                                           "Invasive Species",
                                           "Commercial Development",
                                           "Natural System Modifications",
                                           "Biological Resource Use",
                                           "Agriculture & Aquaculture"))
                                         



# Graph -------------------------------------------------------------------


p1<-threats2_forgraph %>% ggplot(aes(continent,threat_type)) + geom_point(size=10,color="#efefef") +
  geom_point(size = 15, color="#a1cfbe",data = threats2_forgraph %>% filter(sum >= 100)) +
  geom_point(size = 14, color="#e6d492",data = threats2_forgraph %>% filter(sum < 100 & sum >= 50)) +  
  geom_point(size = 12, color="#add8e6",data = threats2_forgraph %>% filter(sum < 50 & sum >= 25)) +
  geom_point(size = 10, color="#f7f7f7",data = threats2_forgraph %>% filter(sum == 0)) +
  geom_text(aes(label=sum), color="black", size=3.5,data = threats2_forgraph%>% filter(sum > 0))+ 
  coord_equal() +
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
    legend.position = "none",
    #axis.text.x    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    #panel.grid.major.y = element_line(color="grey"),
    panel.grid.minor = element_blank(), 
    strip.text.x = element_blank(),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  )





# Prepare the data for second graph ---------------------------------------



threats3<-threats2_forgraph %>% group_by(threat_type) %>%
  summarize(total=sum(sum))%>% 
  mutate(fill = case_when(
    threat_type %in% c("Agriculture & Aquaculture", "Biological Resource Use","Natural System Modifications")  ~ "#a1cfbe",
    threat_type %in% c("Commercial Development", "Invasive Species")  ~ "#e6d492",
    threat_type %in% c("Energy Production & Mining", "Climate Change")  ~ "#add8e6",
    TRUE ~ "#efefef"))



# Second graph ------------------------------------------------------------



p2 <- threats3%>%
  ggplot(aes(x = fct_reorder(threat_type,total), y = total, fill = fill)) +
  geom_bar(stat="identity", width=0.6) +
  geom_text(aes(label = comma_format()(total)), color = "#22292F",hjust=-0.2, size = 3.5) +
  coord_flip() +
  scale_fill_identity() +
  scale_y_continuous(limit = c(0, 250), expand=c(0,1)) +
  #guides(fill = NULL) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(
    axis.title.x =  element_blank(),
    axis.title.y =  element_blank(),
    #legend.position = "right",
    axis.text.x    =  element_blank(),
    #axis.text.y    =  element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),
    plot.margin = unit(c(1, 1, 1, 2), "cm"),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) +
  annotate(
    geom = "text", x = 6, y = 150, label = "More than 100", 
    hjust = "middle", colour = "#a1cfbe", size = 5, fontface = 2 
  ) +
  annotate(
    geom = "text", x = 5, y = 150, label = "Between 50 and 99", 
    hjust = "middle", colour = "#e6d492", size = 5, fontface = 2 
  ) +
  annotate(
    geom = "text", x = 4, y = 150, label = "Between 25 and 49", 
    hjust = "middle", colour = "#add8e6", size = 5, fontface = 2 
  ) 





# patchwork ---------------------------------------------------------------


PTT<-p1 | p2



PTT2 <- PTT + plot_annotation(title = "Plants in danger",
                                                 subtitle = "Total number of plants in danger by type of threat",
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








