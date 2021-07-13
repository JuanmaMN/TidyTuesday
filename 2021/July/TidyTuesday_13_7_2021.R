# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, scales, ggtext, ggpubr,ggbump,wesanderson,
               grid, gridtext, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm, ggforce)


# Upload the data ---------------------------------------------------------

scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')


# Prepare the data --------------------------------------------------------

scoobydoo_network2 <-  scoobydoo %>% filter(!engagement == "NULL",
                                            !imdb == "NULL") %>% 
  mutate(engagement2 = as.numeric(engagement),
         imdb = as.numeric(imdb)) %>%  
  filter (format == "TV Series" & !network == "The CW") %>%
  group_by(network, format)  %>% summarise(avg_imdb =round(mean(imdb,na.rm=TRUE),2), 
                                           max_imdb = max(imdb),  
                                           min_imdb = min(imdb))  

# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Quicksand")

font_add_google("Work Sans")

font_labels <- "Quicksand"

font_labels2 <- "Work Sans"

showtext_auto()

# Graph -------------------------------------------------------------------

scoobydoo_network2%>%ggplot(aes(network, avg_imdb, group = 1, color = network)) +geom_line(aes(color = network,  size = 2.5)) +
  geom_segment(aes(xend = network, yend = max_imdb)) +  
  geom_segment(aes(xend = network, yend = min_imdb)) +
  geom_point(aes(size = 14, network)) +  
  geom_point(aes(y = max_imdb), size = 4) +  geom_point(aes(y = min_imdb), size = 4) +  
  geom_point(size = 12) +
  geom_text(aes(label = number(avg_imdb, accuracy = 0.1)),   color = "white",   fontface = "bold",  family = font_labels,  size = 3,   hjust = 0.5, vjust = 0.5) +
  scale_y_continuous(breaks = seq(5.5,10, by = 1), limits=c(5.5, 10)) +
  theme_ipsum_rc() +
  scale_fill_manual(values = c( "ABC" = "#c3d6d0",
                                "CBS" = "#c3d6d0",
                                "The WB" = "#c3d6d0",
                                "Warner Home Video" = "#f7a188",
                                "Boomerang" = "#c3d6d0",  
                                "Cartoon Network"     = "#d3d9a9")) +
  scale_colour_manual(values = c( "ABC" = "#c3d6d0",
                                  "CBS" = "#c3d6d0",
                                  "The WB" = "#c3d6d0",
                                  "Warner Home Video" = "#f7a188",
                                  "Boomerang" = "#c3d6d0",  
                                  "Cartoon Network"     = "#d3d9a9")) +
  labs(y = "",
       title = "Scooby Doo Episodes - TV series ratings",
       subtitle =  "Average, maximum, minimum ratings by network",
       caption =  "Source: Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")  +
  theme(
    legend.position = "none",
    plot.title = element_text(margin = margin(b = 10, t= 10), 
                              color = "#22222b",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#22222b", size = 10, family = font_labels,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10,
                                 hjust = 0.95,
                                 family = font_labels),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.y    = element_blank(),
    axis.text.x    = element_text(color = "#525252", family = font_labels, size = 12),  # hjust = 1 is not needed for aligment
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(1, 1, 1, 2), "cm"),
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
    axis.ticks = element_blank()) +
  geom_segment(aes(x=1,xend=6,y=5.5,yend=5.5),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=1,xend=6,y=6.5,yend=6.5),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=1,xend=6,y=7.5,yend=7.5),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=1,xend=6,y=8.5,yend=8.5),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=1,xend=6,y=9.5,yend=9.5),linetype="dotted",colour = "#525252") +
  geom_text(aes(x = 0.7, y = 5.5, label = "5.5",fontface=1), hjust = 0, 
            nudge_x = 0.01, family = font_labels,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 0.7, y = 6.5, label = "6.5",fontface=1), hjust = 0, 
            nudge_x = 0.01, family = font_labels,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 0.7, y = 7.5, label = "7.5",fontface=1), hjust = 0, 
            nudge_x = 0.01, family = font_labels,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 0.7, y = 8.5, label = "8.5",fontface=1), hjust = 0, 
            nudge_x = 0.01, family = font_labels,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 0.7, y = 9.5, label = "9.5",fontface=1), hjust = 0, 
            nudge_x = 0.01, family = font_labels,size = 3.5, colour = "#a1a1a1")  










