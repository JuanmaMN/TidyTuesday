
# Upload the data ---------------------------------------------------------

character_visualization <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/character_visualization.csv')


# Load packages -----------------------------------------------------------

pacman::p_load(dplyr, tidyverse, ggplot2, patchwork,hrbrthemes, waffle)


# Prepare the data --------------------------------------------------------

character_visualization2<-character_visualization%>% group_by(costume) %>%
  summarize (speech=sum(speech),
             thought = sum(thought),
             narrative=sum(narrative),
             depicted=sum(depicted))



# First graph -------------------------------------------------------------

first<-character_visualization2%>%select(1,2)

pwaffle <- first %>% 
  ggplot(aes(fill=costume,values=speech/1000))+
  geom_waffle(color="white",size=.25,n_rows = 5,flip = T) +
  scale_fill_manual(values = c("#8fb9c9", "#c0d8c5")) +
  labs(x = "",y = "",
       title = "Speech bubble",
       subtitle = "1 square = 1,000 Speech bubbles")+
  scale_x_discrete(position = "top") +
  theme(
    plot.title = element_text(margin = margin(b = 1), 
                              color = "#22222b",face = "bold",size = 9,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=5,b = 1), 
                                 color = "#22222b", size = 7, family = "Arial",
                                 hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10),
                                color = "#22222b"),
    axis.title.y = element_text(margin = margin(r = 15), 
                                color = "#22222b"),
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "#f7f7f7", color = NA),
    legend.position = "bottom",
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  ) 



# Second graph ------------------------------------------------------------



second<-character_visualization2%>%select(1,3)


pwaffle2 <- second %>% 
  ggplot(aes(fill=costume,values=thought/500))+
  geom_waffle(color="white",size=.25,n_rows = 5,flip = T) +
  scale_fill_manual(values = c("#8fb9c9", "#c0d8c5")) +
  labs(x = "",y = "",
       title = "Thought bubble",
       subtitle = "1 square = 500 Thought bubbles")+
  scale_x_discrete(position = "top") +
  theme(
    plot.title = element_text(margin = margin(b = 1), 
                              color = "#22222b",face = "bold",size = 9,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=5,b = 1), 
                                 color = "#22222b", size = 7, family = "Arial",
                                 hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10),
                                color = "#22222b"),
    axis.title.y = element_text(margin = margin(r = 15), 
                                color = "#22222b"),
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "#f7f7f7", color = NA),
    legend.position = "bottom",
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  ) 



# Third graph -------------------------------------------------------------


third<-character_visualization2%>%select(1,4)

pwaffle3 <- third %>% 
  ggplot(aes(fill=costume,values=narrative/500))+
  geom_waffle(color="white",size=.25,n_rows = 5,flip = T) +
  scale_fill_manual(values = c("#8fb9c9", "#c0d8c5")) +
  labs(x = "",y = "",
       title = "Narrative statements",
       subtitle = "1 square = 500 Narrative statements")+
  scale_x_discrete(position = "top") +
  theme(
    plot.title = element_text(margin = margin(b = 1), 
                              color = "#22222b",face = "bold",size = 9,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=5,b = 1), 
                                 color = "#22222b", size = 7, family = "Arial",
                                 hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10),
                                color = "#22222b"),
    axis.title.y = element_text(margin = margin(r = 15), 
                                color = "#22222b"),
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "#f7f7f7", color = NA),
    legend.position = "bottom",
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  ) 



# Fourth graph ------------------------------------------------------------

fourth<-character_visualization2%>%select(1,5)

pwaffle4 <- fourth %>% 
  ggplot(aes(fill=costume,values=depicted/1000))+
  geom_waffle(color="white",size=.25,n_rows = 5,flip = T) +
  scale_fill_manual(values = c("#8fb9c9", "#c0d8c5")) +
  labs(x = "",y = "",
       title = "Number of depictions",
       subtitle = "1 square = 1,000 Number of depictions")+
  scale_x_discrete(position = "top") +
  theme(
    plot.title = element_text(margin = margin(b = 1), 
                              color = "#22222b",face = "bold",size = 9,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=5,b = 1), 
                                 color = "#22222b", size = 7, family = "Arial",
                                 hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10),
                                color = "#22222b"),
    axis.title.y = element_text(margin = margin(r = 15), 
                                color = "#22222b"),
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "#f7f7f7", color = NA),
    legend.position = "bottom",
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  ) 





patchwork_tt<- (pwaffle | pwaffle2)/(pwaffle3  | pwaffle4)  +
  plot_layout(guides = "collect")


p_tt <-patchwork_tt+ plot_annotation(title = "The Claremont Run - Uncanny X-Men",
                                                   subtitle = "Counts of character speech, thought, narrative and visual depictions",
                                                   caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)",
                                                   theme = theme(plot.background = element_rect(fill = "#f7f7f7",
                                                                                                size = 1),
                                                                 legend.position = "bottom",
                                                                 legend.background = element_rect(fill = "#f7f7f7",
                                                                                                  size = 1),
                                                                 legend.title = element_blank(),
                                                                 plot.margin = margin(),
                                                                 plot.title = element_text(margin = margin(t=10, b = 10), 
                                                                                           color = "#22222b",face = "bold",size = 15,
                                                                                           hjust = 0.5,
                                                                                           family = "Arial"),
                                                                 plot.subtitle = element_text(margin = margin(b = 25), 
                                                                                              hjust = 0.5,
                                                                                              color = "#22222b", size = 10, family = "Arial"),
                                                                 plot.caption =  element_text(margin = margin(t = 10, b=10), 
                                                                                              color = "#000000", size = 8, family = "Arial",
                                                                                              hjust = 0.95)))  






p_tt




