# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,sf,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm, 
               ggalt)


# Upload the data ---------------------------------------------------------

records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')


# Prepare the data --------------------------------------------------------

records2<-records %>% group_by(track, type) %>% summarize (record_max = max(record_duration)) %>% pivot_wider(names_from = type, values_from = record_max)
                                                 
names(records2)[2]<-"single"
names(records2)[3]<-"three"


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Quicksand")

font_add_google("Work Sans")

font_labels <- "Quicksand"

font_labels2 <- "Work Sans"

showtext_auto()



# Annotations -------------------------------------------------------------

grob_first <- grobTree(richtext_grob(
  sprintf("**Mario Kart 64 World Records duration in days by track** <br>"),  
  x=0.03, y=1, hjust=0, gp=gpar(col = "#000000", fontsize=18,  fontfamily = font_labels), vjust = 1))



grob_second <- grobTree(richtext_grob(
  sprintf("<br><b style='color:%s'>Single lap race world record</b>","#ffd164"),  
  x=0.03, y=0.95, hjust=0, gp=gpar(col = "#000000", fontsize=14,  fontfamily = font_labels), vjust = 1))

grob_third <- grobTree(richtext_grob(
    sprintf("<br><b style='color:%s'>Three lap race world record</b>","#4a8579"),  
            x=0.03,y=0.9, hjust=0, gp=gpar(col = "#000000", fontsize=14,  fontfamily = font_labels), vjust = 1))
    
# Graph -------------------------------------------------------------------

ggplot(records2, aes(x = three, xend = single, y=reorder(track,three))) + 
  geom_dumbbell(colour = "#e5e5e5", size = 3,  colour_x = "#4a8579",colour_xend = "#ffd164")+ 
 scale_x_continuous(breaks = seq(1000,4000, by = 1000), limits=c(750, 4200)) +
  coord_flip() +
  theme_ipsum_rc() +
    labs(y = "",
         title = "",
         subtitle =  "",
         caption =  "Source: Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")  +
    theme(
      plot.title = element_text(margin = margin(b = 10, t= 10), 
                                color = "#22222b",face = "bold",size = 14,
                                hjust = 0.5,
                                family = font_labels),
      plot.caption =  element_text(margin = margin(t = 20), 
                                   color = "#22222b", size = 10,
                                   hjust = 0.95,
                                   family = font_labels),
      axis.title.x = element_blank(), 
      axis.title.y = element_blank(), 
      axis.text.y    = element_blank(),
      axis.text.x = element_blank(), 
      panel.background = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(), 
      plot.margin = unit(c(1, 1, 1, 2), "cm"),
      plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
      axis.ticks = element_blank()) +
  geom_segment(aes(x=1000,xend=1000,y=1,yend=16),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=2000,xend=2000,y=1,yend=16),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=3000,xend=3000,y=1,yend=16),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=4000,xend=4000,y=8,yend=16),linetype="dotted",colour = "#525252") +
  annotate("text",x = 2, y = 18, fontface = "bold", label = " ", family = font_labels, size = 5, hjust = 0) +
  annotation_custom(grob_first) +
  annotation_custom(grob_second) +
  annotation_custom(grob_third) +
  geom_text(aes(x = 1000, y = 16.5, label = "1,000 \n days",fontface=1), hjust = 0, 
            nudge_x = 0.01, family = font_labels2,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 2000, y = 16.5, label = "2,000 \n days",fontface=1), hjust = 0, 
            nudge_x = 0.01, family = font_labels2,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 3000, y = 16.5, label = "3,000 \n days",fontface=1), hjust = 0, 
            nudge_x = 0.01,family = font_labels2,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 4000, y = 16.5, label = "4,000 \n days",fontface=1), hjust = 0, 
            nudge_x = 0.01,family = font_labels2,size = 3.5, colour = "#a1a1a1")  + 
  geom_text(aes(x = 820, y = 1, label = "Bowser's \n Castle",fontface=1),
            family = font_labels2,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 820, y = 2, label = "Moo Moo \n Farm",fontface=1),
            family = font_labels2,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 820, y = 3, label = "Banshee \n Boardwalk",fontface=1),
            family = font_labels2,size = 3.5, colour = "#a1a1a1")  + 
  geom_text(aes(x = 820, y = 4, label = "Mario \n Raceway",fontface=1),
            family = font_labels2,size = 3.5, colour = "#a1a1a1")  + 
  geom_text(aes(x = 820, y = 5, label = "Kalimari \n Desert",fontface=1),
            family = font_labels2,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 820, y = 6, label = "Toad's \n Turnpike",fontface=1),
            family = font_labels2,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 820, y = 7, label = "Wario \n Stadium",fontface=1),
            family = font_labels2,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 820, y = 8, label = "D.K.'s Jungle \n Parkway",fontface=1),
            family = font_labels2,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 820, y = 9, label = "Rainbow \n Road",fontface=1),
            family = font_labels2,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 820, y = 10, label = "Koopa Troopa \n  Beach",fontface=1),
            family = font_labels2,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 820, y = 11, label = "Frappe \n Snowland",fontface=1),
            family = font_labels2,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 820, y = 12, label = "Sherbet \n Land",fontface=1),
            family = font_labels2,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 820, y = 13, label = "Luigi \n Raceway",fontface=1),
            family = font_labels2,size = 3.5, colour = "#a1a1a1")  + 
  geom_text(aes(x = 820, y = 14, label = "Choco \n Mountain",fontface=1),
            family = font_labels2,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 820, y = 15, label = "Royal \n Raceway",fontface=1),
            family = font_labels2,size = 3.5, colour = "#a1a1a1") + 
  geom_text(aes(x = 820, y = 16, label = "Yoshi \n Valley",fontface=1),
            family = font_labels2,size = 3.5, colour = "#a1a1a1")


