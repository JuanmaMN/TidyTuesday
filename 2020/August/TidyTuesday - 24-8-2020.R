
# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr)



# Upload dataset ----------------------------------------------------------


chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')



# First graph -------------------------------------------------------------


chopped2<-chopped%>%mutate(Year= str_sub(air_date,-4,-1)) %>% 
  group_by(Year)%>%
  summarize(avg=mean(episode_rating,na.rm = T))





graph2<-chopped2%>%ggplot(aes(Year, avg, fill=factor(Year))) + geom_bar(stat="identity")+
  guides(fill = NULL) +
  scale_fill_manual(values = c( "2009"      = "#d3d3d3",
                                "2010" = "#f08080",
                                "2011"      = "#f08080",
                                "2012"     = "#f08080",
                                "2013" = "#f08080",
                                "2014" = "#20b2aa",
                                "2015"      = "#20b2aa",
                                "2016" = "#f08080",
                                "2017" = "#20b2aa",
                                "2018" = "#20b2aa",
                                "2019" = "#20b2aa",
                                "2020" = "#20b2aa"))+  coord_flip()+
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
    axis.text.x  = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    #panel.grid.major.y = element_line(color="grey"),
    panel.grid.minor = element_blank(), 
    strip.text.x = element_blank(),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  )+ 
  geom_text(position = position_dodge(0.9), 
            vjust = 0.9, hjust= -0.5,
            color = "black", size = 3, aes(label=round(avg,2))) + 
  annotate(
    geom = "text", x = 1, y = 4, label = "First year of the show", 
    hjust = "middle", colour = "black", size = 3
  )





# Second graph ------------------------------------------------------------

chopped3<-chopped%>%
  mutate(Year= str_sub(air_date,-4,-1)) %>% 
  group_by(Year)%>%
  summarize(avg=mean(episode_rating,na.rm = T)) %>%mutate(lag1 = lag(avg),
                                                          increase = (avg/ lag1) - 1,
                                                          label=percent(increase))
  
  
  
chopped3$color <- factor(ifelse(chopped3$increase < 0, "low", "high"),   levels = c("low", "high"))

chopped3$increase<-as.numeric(chopped3$increase)



graph3<-ggplot(chopped3,aes(Year, increase, fill=color)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#20b2aa","#f08080"), name = NULL) +
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
    axis.text.y  = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    #panel.grid.major.y = element_line(color="grey"),
    panel.grid.minor = element_blank(), 
    strip.text.x = element_blank(),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  )+ 
  scale_x_discrete(breaks = c(2010:2020)) + 
  geom_text(position = position_dodge(0.9), 
            vjust = -0.9,
            color = "black", size = 3, aes(label=label))




# patchwork ---------------------------------------------------------------


Graph_combined<-graph2 | graph3



Graph_combined2 <-Graph_combined+ plot_annotation(title = "Chopped - American reality-based cooking television game show series",
                              subtitle = "IMDB sourced episode rating  & YOY chage in rating",
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



Graph_combined2
