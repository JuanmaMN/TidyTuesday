# Upload data -------------------------------------------------------------

captured_vs_farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv')


# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts)


# Font --------------------------------------------------------------------

font_add_google("DM Serif Display")

font_labels <- "DM Serif Display"

font_add_google("Lora")

font_labels2 <- "Lora"

showtext_auto()


# Prepare the data  -------------------------------------------------------


captured_data_area<-captured_vs_farmed%>% filter(Entity =="World") %>%rename('Aquaculture_production' = 'Aquaculture production (metric tons)',
                                                                      'Capture_fisheries_production' = 'Capture fisheries production (metric tons)') %>%
   group_by(Year) %>% summarize(Aquaculture_production = sum(Aquaculture_production),
                                Capture_fisheries_production = sum(Capture_fisheries_production)) %>% pivot_longer(2:3, names_to="variable", values_to="number")


# Graph -------------------------------------------------------------------

graph_fish <- captured_data_area %>% 
  ggplot(aes(Year, number, group = variable, fill = variable, colour = variable)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(values = c( "Aquaculture_production" = "#c3d6d0",
                                "Capture_fisheries_production" = "#f7a188")) +
  scale_colour_manual(values = c( "Aquaculture_production" = "#c3d6d0",
                                  "Capture_fisheries_production" = "#f7a188")) +
  scale_y_continuous(breaks = seq(0, 220000000,by =50000000),limits=c(0, 220000000), labels = c(" ","50m mt","100m mt","150m mt", "200m mt "),
                     expand = c(.007, .007)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1956, 2018),breaks = 1956:2018, 
                     labels = c(" ", " "," ", " ", 1960, " "," ", " "," "," "," "," "," "," ",
                                1970," "," "," "," "," "," "," "," "," ",
                                1980," "," "," "," "," "," "," "," "," ",
                                1990," "," "," "," "," "," "," "," "," ",
                                2000," "," "," "," "," "," "," "," "," ",
                                " "," "," "," ", " ", 2015, " ", " ", " ")) +
  geom_text(aes(x = 2000, y = 110000000, label = "Aquaculture production (metric tons)",fontface="bold"), hjust = 0, 
            nudge_x = 0.01, family = font_labels,size = 5, colour = "#9EBDB3") +
  geom_text(aes(x = 2000, y = 55000000, label = "Capture fisheries production (metric tons)",fontface="bold"), hjust = 0, 
            nudge_x = 0.01, family = font_labels,size = 5, colour = "#F37953") +
  labs(y = "",
       x = "",
       title = "Aquaculture and Capture fisheries production (metric tons) in the world",
       caption =  "Source: Data by OurWorldInData.org - #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")  +
  theme(
    plot.title = element_text(margin = margin(b = 15, t=10), 
                              color = "#808080",face = "bold",size = 14,
                              hjust = 0,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#808080", size = 12, family = font_labels,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 50), 
                                 color = "#808080", size = 10,
                                 hjust = 0.94,
                                 family = font_labels),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.x    = element_text(color = "#525252", family = font_labels, size = 11),  # hjust = 1 is not needed for aligment
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
    axis.ticks = element_blank(),
    legend.position = "none") +
  geom_segment(data= captured_data_area , mapping=aes(x=1960,xend=2015,y=50000000,yend=50000000),linetype="dotted",colour = "#000000") +
  geom_segment(aes(x=1960,xend=2015,y=100000000,yend=100000000),linetype="dotted",colour = "#000000") +
  geom_segment(aes(x=1960,xend=2015,y=150000000,yend=150000000),linetype="dotted",colour = "#000000") +
  geom_segment(aes(x=1960,xend=2015,y=200000000,yend=200000000),linetype="dotted",colour = "#000000") +
  annotate("text",x =1958, y = 200000000, label = "200 million\n metric tons",colour = "#525252", 
           vjust = 0.5, size = 3.5, family = font_labels, fontface = "plain")  +
  annotate("text",x =1958, y = 150000000, label = "150M",colour = "#525252", 
           vjust = 0.5, size = 3.5, family = font_labels, fontface = "plain")  +
  annotate("text",x =1958, y = 100000000, label = "100M",colour = "#525252", 
           vjust = 0.5, size = 3.5, family = font_labels, fontface = "plain") +
  annotate("text",x =1958, y = 50000000, label = "50M",colour = "#525252", 
           vjust = 0.5, size = 3.5, family = font_labels, fontface = "plain") 

graph_fish

