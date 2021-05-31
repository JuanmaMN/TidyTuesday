
# Upload packages ---------------------------------------------------------


pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,sf,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm, 
               ggalt)


# Upload data -------------------------------------------------------------

summary <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/summary.csv')


# Prepare data ------------------------------------------------------------

data_s<-summary%>%filter (season <= 10) %>% select (season, country, viewers_finale, viewers_reunion, viewers_mean, rank)

# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Quicksand")

font_add_google("Work Sans")

font_labels <- "Quicksand"

font_labels2 <- "Work Sans"

showtext_auto()


# Graph -------------------------------------------------------------------

graph_sv <- data_s  %>% 
  ggplot(aes(x = season, y = viewers_mean)) +
  geom_segment(aes(x = season, xend = season, y = viewers_mean, yend = viewers_finale), color="#d3d3d3") +  
  geom_segment(aes(x = season, xend = season, y = viewers_mean, yend = viewers_reunion),color="#d3d3d3") +
  geom_point(aes(x = season, y = viewers_finale), size = 4, color = "#b8c375") +  
  geom_point(aes(x = season, y = viewers_reunion), size = 4, color = "#c27c7c")   +
  geom_point(size = 10, fill = "#a2bbc0",color = "#a2bbc0") +
  geom_line(size = 1, color = "#a2bbc0") +
  scale_x_continuous(breaks = seq(1,10, by = 1), limits=c(1, 10), labels = c("Season 1", "Season 2","Season 3","Season 4","Season 5",
                                                                             "Season 6","Season 7","Season 8","Season 9", "Season 10"))  +
  scale_y_continuous(breaks = seq(10,55, by = 10), limits=c(10,55), labels = c("10m","20m","30m","40m","50m"))  +
  geom_text(aes(label = number(viewers_mean,accuracy = 0.1)),   color = "white",   fontface = "bold",   size = 3,  
            hjust = 0.5, vjust = 0.5,family = font_labels)  +
  theme_ipsum_rc() +
  labs(y = "",
       title = " ",
       subtitle =  "",
       caption =  "")  +
  theme(
    plot.title = element_text(margin = margin(b = 10, t= 10), 
                              color = "#22222b",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.caption =  element_text(margin = margin(t= 10),
                                 color = "#22222b", size = 8,
                                 hjust = 1,
                                 family = font_labels),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.y    = element_text(color = "#525252", size = 10, family = font_labels,margin = margin(r = 0)),
    axis.text.x = element_text(color = "#525252", size = 10, family = font_labels,margin = margin(r = 0)),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(1, 2, 1, 2), "cm"),
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
    axis.ticks = element_blank()) +
    annotate("text",x = 3.8, y = 54, fontface = "italic", 
             label = str_wrap("Survivor TV Show! #TidyTuesday weekly data"), 
             family = font_labels, size = 10, hjust = 0) +
  annotate("text",x =4, y = 50, fontface = "italic", 
           label = str_wrap("First 10 seasons' comparison of "), 
           family = font_labels, size = 4, hjust = 0) +
  annotate("text",x = 5.25, y = 50, fontface = "bold", 
           label = str_wrap("Viewers average per season, "), 
           family = font_labels, size =4, hjust = 0, color = "#a2bbc0") +
  annotate("text",x = 6.5, y = 50, fontface = "bold", 
           label = str_wrap("Viewers at finale"), 
           family = font_labels, size =4, hjust = 0, color = "#b8c375")  +
  annotate("text",x = 7.25, y = 50, fontface = "italic", 
           label = str_wrap("  and"), 
           family = font_labels, size = 4, hjust = 0)  +
  annotate("text",x = 7.45, y = 50, fontface = "bold", 
           label = str_wrap(" Viewers for reunion"), 
           family = font_labels, size = 4, hjust = 0, color = "#c27c7c") +
  annotate("text",x = 5.5, y = 47, fontface = "italic", 
           label = str_wrap("Visualization: JuanmaMN (Twitter @Juanma_MN)"), 
           family = font_labels, size = 3, hjust = 0, color = "#22222b") +
  geom_segment(aes(x=1,xend=10,y=10,yend=10),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=1,xend=10,y=20,yend=20),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=1,xend=10,y=30,yend=30),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=1,xend=10,y=40,yend=40),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=1,xend=3,y=50,yend=50),linetype="dotted",colour = "#525252") 


graph_sv

# I have used annotations for subtitles. However, It would be better to use <span style> ...