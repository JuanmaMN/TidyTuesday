# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)



# Raw data ----------------------------------------------------------------

soybean_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/soybean_use.csv')


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Quicksand")

font_add_google("Work Sans")

font_labels <- "Quicksand"

font_labels2 <- "Work Sans"

showtext_auto()


# Prepare the data --------------------------------------------------------


data<-soybean_use%>%pivot_longer(4:6, names_to = "Type", values_to = "Value")%>% filter (entity == "World")


# Graph -------------------------------------------------------------------

graph <- data %>% 
  ggplot(aes(year, Value, group = Type, fill = Type, colour = Type)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(values = c( "human_food" = "#f7a188",
                                "animal_feed" = "#c3d6d0",  
                                "processed"     = "#d3d9a9")) +
   scale_colour_manual(values = c(  "human_food" = "#f7a188",
                                    "animal_feed" = "#c3d6d0",  
                                    "processed"     = "#d3d9a9")) +
  scale_y_continuous(breaks = seq(0, 260000000,by =50000000),limits=c(0, 260000000), labels = c(" ","50m t","100m t","150m t","200m t", "250m t"),
                     expand = c(.007, .007)) +
  scale_x_continuous(expand = c(0, 0), limits = c(1961, 2021.5),breaks = 1961:2013, 
                     labels = c(1961," ", " "," "," "," "," "," "," ",
                                1970," "," "," "," "," "," "," "," "," ",
                                1980," "," "," "," "," "," "," "," "," ",
                                1990," "," "," "," "," "," "," "," "," ",
                                2000," "," "," "," "," "," "," "," "," ",
                                " "," "," ",2013)) +
  labs(y = "",
       x = "",
       title = "Soybean production and use  - World",
       subtitle =  "Data at the national level is based on soybean uses after trade (which is soybean production minus exports plus imports). Yearly production and use measured in tonnes.",
       caption =  "Source: Data by OurWorldInData.org - #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")  +
  theme(
    plot.title = element_text(
      color = "#22222b",face = "bold",size = 14,
      hjust = 0,
      family = font_labels),
    plot.subtitle = element_text(margin = margin(t=5, b= 30),
                                 color = "#22222b", size = 10, family = font_labels,
                                 hjust = 0),
    plot.caption =  element_text(margin = margin(t = 50), 
                                 color = "#22222b", size = 10,
                                 hjust = 0.99,
                                 family = font_labels),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.x    = element_text(color = "#525252", family = font_labels, size = 10),  # hjust = 1 is not needed for aligment
    axis.text.y    = element_text(color = "#525252",family = font_labels, size = 10),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
    axis.ticks = element_blank(),
    legend.position = "none") + 
  geom_text(aes(x = 2013.5, y = 25000000, label = "Processed (processed animal feed; \nbiofuels; vegetable oil)",fontface=1), hjust = 0, 
            nudge_x = 0.01, family = font_labels2,size = 3.5, colour = "#d3d9a9") + 
  geom_text(aes(x = 2013.5, y = 250000000, label = "Direct animal feed",fontface=1), hjust = 0, 
            nudge_x = 0.01, family = font_labels2,size = 3.5, colour = "#b4ccc4") + 
  geom_text(aes(x = 2013.5, y = 230311000, label = "Direct human food (tofu, \nsoy milk, tempeh etc.)",fontface=1), hjust = 0, 
            nudge_x = 0.01,family = font_labels2,size = 3.5, colour = "#f7a188") +
  geom_segment(data= data, mapping=aes(x=1961,xend=2013,y=50000000,yend=50000000),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=1961,xend=2013,y=100000000,yend=100000000),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=1961,xend=2013,y=150000000,yend=150000000),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=1961,xend=2013,y=200000000,yend=200000000),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=1961,xend=2013,y=250000000,yend=250000000),linetype="dotted",colour = "#525252") 
  


graph

