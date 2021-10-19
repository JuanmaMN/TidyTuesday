
# Upload data -------------------------------------------------------------

pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')


# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, scales, ggtext, ggpubr,ggbump,wesanderson,
               grid, gridtext, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm,stringi)


# Font --------------------------------------------------------------------

font_add_google("DM Serif Display")

font_labels <- "DM Serif Display"

font_add_google("Lora")

font_labels2 <- "Lora"

showtext_auto()


# Prepare the data for first graph ----------------------------------------

pumpkins2P <- pumpkins %>% mutate (year = str_sub(id, 1, 4),
                                   type = str_sub(id, 6)) %>% filter (place == 1 & type == "P") %>% select(year, type,weight_lbs)


# Avoid NAs introduced by coercion 

pumpkins2P$weight_lbs <- gsub(",", "", pumpkins2P$weight_lbs)                          
pumpkins2P$weight_lbs<-as.numeric(pumpkins2P$weight_lbs)
pumpkins2P$year<-as.numeric(pumpkins2P$year)


# First graph -------------------------------------------------------------

graphpmP<-pumpkins2P%>%ggplot(aes(year, weight_lbs, group = type)) +
  geom_area(alpha = 0.2,fill="#ffa365",colour="#ffa365") +
  scale_y_continuous(limits = c(0, 3000), breaks = c(0,1000, 2000, 3000))  +
  scale_x_continuous(limits = c(2013, 2021), breaks = c(2013,2015,2017, 2019, 2021))  +
  guides(fill = NULL) +
  theme(legend.position = "none",
    panel.background = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.x    = element_text(size = 10, color = "#808080",family = font_labels), 
    axis.text.y    = element_blank(), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),    axis.line.x=element_line(color="#e2e2e2", size = 0.01)
  ) +
  geom_point(x = 2013, y = 2032.0,size=3, shape=21, fill="#ff9665") +
  geom_point(x = 2021, y = 2702.9,size=3, shape=21, fill="#ff9665") +
  annotate("text", x = 2013, y =2250.0,fontface =2,
           hjust = 0.5, color = "#ff9665",family = font_labels,
           size = 4.5, label = paste0("2032 lbs"))+
  annotate("text", x = 2021, y =2900.9,fontface =2,
           hjust = 0.5, color = "#ff9665",family = font_labels,
           size = 4.5, label = paste0("2702 lbs"))




# Prepare the data for second graph ---------------------------------------


pumpkins2S <- pumpkins %>% mutate (year = str_sub(id, 1, 4),
                                   type = str_sub(id, 6)) %>% filter (place == 1 & type == "S") %>% select(year, type,weight_lbs)


# Avoid NAs introduced by coercion 

pumpkins2S$weight_lbs <- gsub(",", "", pumpkins2S$weight_lbs)                          

pumpkins2S$weight_lbs<-as.numeric(pumpkins2S$weight_lbs)

pumpkins2S$year<-as.numeric(pumpkins2S$year)



# Graph -------------------------------------------------------------------

graphpmS<-pumpkins2S%>%ggplot(aes(year, weight_lbs, group = type)) +
  geom_area(alpha = 0.2,fill="#ffa365",colour="#ffa365") +
  scale_y_continuous(limits = c(0, 3000), breaks = c(0, 1000, 2000, 3000))  +
  scale_x_continuous(limits = c(2013, 2021), breaks = c(2013,2015,2017, 2019, 2021)) +
  guides(fill = NULL) +
  theme(
    legend.position = "none",
    panel.background = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.x    = element_text(size = 10, color = "#808080",family = font_labels),
    axis.text.y    = element_blank(), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),     axis.line.x=element_line(color="#e2e2e2", size = 0.01)
  ) +
  geom_point(x = 2013, y = 1264.0,size=3, shape=21, fill="#ff9665") +
  geom_point(x = 2021, y = 2164.0,size=3, shape=21, fill="#ff9665") +
  annotate("text", x = 2013, y =1500.0,fontface =2,
           hjust = 0.5, color = "#ff9665",family = font_labels,
           size = 4.5, label = paste0("1264 lbs"))+
  annotate("text", x = 2021, y =2400.9,fontface =2,
           hjust = 0.5, color = "#ff9665",family = font_labels,
           size = 4.5, label = paste0("2164 lbs"))




# Add text ----------------------------------------------------------------

textpmp2 <- data.frame(
  purpose = c("Giant Pumpkin \n maximum estimated weight in lbs \n per year"),
  purpose2 = c("Giant Squash \n maximum estimated weight in lbs \n per year")
)


textpmpgraph2<-ggplot(fill="#ffa365") +
  geom_text(data = textpmp2,  aes(x = 0.5, y = 0.15, label = purpose),
            color="#808080", size=6, fontface="bold", hjust = 0.5, family = font_labels) +
  geom_text(data = textpmp2,  aes(x = 0.5, y = -0.15, label = purpose2),
            color="#808080", size=6, fontface="bold", hjust = 0.5, family = font_labels) +
  scale_x_continuous(limits = c(-0.2,1)) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  theme(        legend.position = "none",
        panel.background = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x    = element_blank(), 
        axis.text.y    = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        axis.ticks = element_blank()
  ) 




# Final graph -------------------------------------------------------------



finalgraph <- (((textpmpgraph2 + plot_layout(widths = c(1, 2), nrow = 1)) | (graphpmP + plot_layout(nrow = 1)) / (graphpmS + plot_layout(nrow = 1))
           + plot_layout(guides = 'keep'))) + 
  plot_annotation(caption = "Source: #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)",
                  theme = theme(plot.background = element_rect(fill = "#fbfaf6", color = NA)
                                ) )


finalgraph