
# Upload raw data ---------------------------------------------------------

episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')


# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts, showtext,ggflags)



# Prepare the data --------------------------------------------------------


episodes <- episodes %>% select(season_number, episode_number, uk_viewers) %>% filter(season_number < 13)%>% na.omit()

episodes <- episodes %>%mutate(legend_uk = case_when(
  uk_viewers < 5 ~ "< 5",
  uk_viewers >= 5 & uk_viewers < 7.5  ~ "5 - 7.5",
  uk_viewers >= 7.5 & uk_viewers < 10  ~ "7.5 - 10",
  uk_viewers >= 10  ~ "> 10",
  TRUE ~ "others"
)
)
       


# Font --------------------------------------------------------------------

font_add_google("DM Serif Display")

font_labels <- "DM Serif Display"

font_add_google("Bodoni Moda")

font_labels2 <- "Bodoni Moda"

showtext_auto()



# Graph -------------------------------------------------------------------


episodes_graph <- episodes%>%ggplot(aes(x = season_number, y = episode_number, fill = legend_uk)) +
  geom_tile(colour = "grey30", size = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(-3, 15),breaks = 1:12, 
                     labels = c(1," ", " "," "," "," "," "," "," "," ", " ", 12)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-3, 15),breaks = 1:13, 
                     labels = c(1," ", " "," "," "," "," "," "," "," ", " ", " ", 13)) +
  scale_fill_manual(values = c( "< 5" = "#d9ed92",
                                "5 - 7.5" = "#99d98c",
                                "7.5 - 10" = "#52b69a",
                                "> 10" = "#168aad"),
                    expand = c(.007, .007)) +
  labs(y = "",
       x = "",
       title = "Dr. Who - UK Viewership analysis",
       subtitle = "Total UK viewership in millions by episode and season",
       caption =  "Source: #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")  +
  theme(
    plot.title = element_text(margin = margin(b = 25, t=10), 
                              color = "#525252",face = "bold",size = 20,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=5,b = 25), 
                                 color = "#525252", size = 12, family = font_labels2,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 50), 
                                 color = "#525252", size = 10,
                                 hjust = 0.94,
                                 family = font_labels2),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.x    = element_blank(),   
    axis.text.y    = element_blank(), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border,
    axis.ticks = element_blank(),
   legend.position = "bottom",
   legend.text=element_text(size=8, color = "#525252"),
   legend.key.size = unit(0.2, "cm"),
   legend.title = element_blank(),
   legend.key = element_blank(),
   legend.background=element_blank(),
   legend.margin=margin(b = 0.1, unit='cm'))   +
     guides(fill = guide_legend(
       label.position = "bottom",
       nrow = 1,
       family = font_labels2, 
       color = "#525252",
       keywidth = 3, keyheight = 0.5)) + 
  geom_segment(aes(x = 1.5, y = -1, xend = 5, yend = -1),linetype="solid", colour = "#b4b4b4", size = 0.25)  +
  geom_segment(aes(x = 7, y = -1, xend = 11.5, yend = -1),linetype="solid", colour = "#b4b4b4", size = 0.25,   
               arrow = arrow(length = unit(0.25, "cm"))) +
  geom_segment(aes(x = 0, y = 1.5, xend = 0, yend = 4),linetype="solid", colour = "#b4b4b4", size = 0.25) +
  geom_segment(aes(x = 0, y = 9, xend = 0, yend =12.5),linetype="solid", colour = "#b4b4b4", size = 0.25,    
               arrow = arrow(length = unit(0.25, "cm"))) +
  annotate(geom = "text", label = c("1", "12"),    colour = "#525252", x = c(1, 12), y = c(-1, -1), size = 4.5,   
           family = font_labels2) +
  annotate(geom = "text", label = "Season number",    colour = "#525252", x = 6, y = -1, size = 4.5,   
           family = font_labels2) +
  annotate(geom = "text", label = "Episode number", angle = 90,    colour = "#525252", x = 0, y = 6.5, size = 4.5,   
           family = font_labels2) +
  annotate(geom = "text", label = c("1", "13"), angle = 90,    colour = "#525252", x = c(0, 0), y = c(1, 13), size = 4.5,   
           family = font_labels2) 

episodes_graph

