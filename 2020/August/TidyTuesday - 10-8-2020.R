
# Upload the packages -----------------------------------------------------


pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr)


# Upload raw data ---------------------------------------------------------

avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')


# Prepare the data --------------------------------------------------------

data_graph_book<-avatar%>% group_by(book)%>% summarize(avg=mean(imdb_rating, na.rm = T),
                                                       remainig= 10-avg) %>%
  pivot_longer(cols = 2:3, names_to = "name", values_to = "value")


data_graph_book_earth<-data_graph_book %>% filter(book == "Earth")
data_graph_book_fire<-data_graph_book %>% filter(book == "Fire")
data_graph_book_water<-data_graph_book %>% filter(book == "Water")



# Graph -------------------------------------------------------------------

first_earth<- data_graph_book_earth%>% 
  ggplot(aes(x = 1, y = value)) + 
  geom_col(aes(fill = name), width = 0.8) + 
  coord_polar(theta = "y") + 
  scale_fill_manual(values = c("#a0c4a9", "#e6d492"), guide = F) +
  theme_void() +
  geom_text(data = data_graph_book_earth%>% filter(name == "avg"),aes(-0.9, label=paste0(book, "\n",round(value,2))),  colour="#a0c4a9",size = 6) +
  scale_x_discrete(position = "top") +
  guides(fill = NULL) +
  labs(x = NULL, y = NULL, title = "") +
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
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 2), "cm"),
    axis.ticks = element_blank(),
    strip.text.x = element_blank()
  ) 

first_fire<- data_graph_book_fire%>% 
  ggplot(aes(x = 1, y = value)) + 
  geom_col(aes(fill = name), width = 0.8) + 
  coord_polar(theta = "y") + 
  scale_fill_manual(values = c("#a0c4a9", "#e6d492"), guide = F) +
  theme_void() +
  geom_text(data = data_graph_book_fire%>% filter(name == "avg"),aes(-0.9, label=paste0(book, "\n",round(value,2))),  colour="#a0c4a9",size = 6) +
  scale_x_discrete(position = "top") +
  guides(fill = NULL) +
  labs(x = NULL, y = NULL, 
       title = "") +
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
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 2), "cm"),
    axis.ticks = element_blank(),
    strip.text.x = element_blank()
  ) 


first_water<- data_graph_book_water%>% 
  ggplot(aes(x = 1, y = value)) + 
  geom_col(aes(fill = name), width = 0.8) + 
  coord_polar(theta = "y") + 
  scale_fill_manual(values = c("#a0c4a9", "#e6d492"), guide = F) +
  theme_void() +
  geom_text(data = data_graph_book_water%>% filter(name == "avg"),aes(-0.9, label=paste0(book, "\n",round(value,2))),  colour="#a0c4a9",size = 6) +
  scale_x_discrete(position = "top") +
  guides(fill = NULL) +
  labs(x = NULL, y = NULL, 
       title = "") +
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
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 2), "cm"),
    axis.ticks = element_blank(),
    strip.text.x = element_blank()
  ) 



data_graph_book_2_earth<-avatar%>% group_by(director, book)%>% summarize(avg=mean(imdb_rating, na.rm = T)) %>% filter(book == "Earth")

second_earth<-data_graph_book_2_earth %>% ggplot(aes(x=fct_reorder(director,avg), y=avg)) +
  geom_bar(stat="identity", fill="#a0c4a9", width=0.6) +
  coord_flip() + 
  geom_text(aes(label=round(avg,2)),hjust=-0.5)+
  guides(fill = NULL) +
  labs(x = NULL, y = NULL, 
       title = "") +
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
    axis.text.x    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_blank(),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 2), "cm"),
    axis.ticks = element_blank()
  ) +
  ylim(0,10) +
  
  ylab("mark") +
  
  xlab("") 


data_graph_book_2_water<-avatar%>% group_by(director, book)%>% summarize(avg=mean(imdb_rating, na.rm = T)) %>% filter(book == "Water")



second_water<-data_graph_book_2_water %>% ggplot(aes(x=fct_reorder(director,avg), y=avg)) +
  geom_bar(stat="identity", fill="#a0c4a9", width=0.6) +
  coord_flip() + 
  geom_text(aes(label=round(avg,2)),hjust=-0.5) +
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
    legend.position = "none",
    axis.text.x    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    strip.text.x = element_blank(),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 2), "cm"),
    axis.ticks = element_blank()
  ) +
  ylim(0,10) +
  
  ylab("mark") +
  
  xlab("") 

data_graph_book_2_fire<-avatar%>% group_by(director, book)%>% summarize(avg=mean(imdb_rating, na.rm = T)) %>% filter(book == "Fire")



second_fire<-data_graph_book_2_fire %>% ggplot(aes(x=fct_reorder(director,avg), y=avg)) +
  geom_bar(stat="identity", fill="#a0c4a9", width=0.6) +
  coord_flip() + 
  geom_text(aes(label=round(avg,2)),hjust=-0.5) +
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
    legend.position = "none",
    axis.text.x    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    strip.text.x = element_blank(),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 2), "cm"),
    axis.ticks = element_blank()
  ) +
  ylim(0,10) +
  
  ylab("mark") +
  
  xlab("") 


# Patchwork ---------------------------------------------------------------


ggarrange(first_earth,first_fire,first_water,second_earth,second_fire,second_water, ncol=3, nrow=2, common.legend = TRUE, legend="bottom") + plot_annotation(title = "Average IMDB rating",
                                                                                                                                                             subtitle = "Average IMDB per book & Average IMDB of each director per book across all episodes",
                                                                                                                                                             caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)",
                                                                                                                                                             theme = theme(plot.background = element_rect(fill = "#f7f7f7",
                                                                                                                                                                                                          size = 10),
                                                                                                                                                                           plot.margin = unit(c(1, 2, 2, 1), "cm"),
                                                                                                                                                                           plot.title = element_text(margin = margin(t=5, b = 5), 
                                                                                                                                                                                                     color = "#22222b",face = "bold",size = 15,
                                                                                                                                                                                                     hjust = 0.5,
                                                                                                                                                                                                     family = "Arial"),
                                                                                                                                                                           plot.subtitle = element_text(margin = margin(t=5, b = 5), 
                                                                                                                                                                                                        color = "#22222b",size = 10,
                                                                                                                                                                                                        hjust = 0.5,
                                                                                                                                                                                                        family = "Arial"),
                                                                                                                                                                           plot.caption =  element_text(margin = margin(t = 5, b=10), 
                                                                                                                                                                                                        color = "#000000", size = 8, family = "Arial",
                                                                                                                                                                                                        hjust = 0.95)))  


