
# Upload the packages -----------------------------------------------------

library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(gridExtra)
library(patchwork)
library(systemfonts)
library(sysfonts)
library(showtext)

# Font --------------------------------------------------------------------

font_add_google("Lora")

font_labels <- "Lora"

showtext_auto()


# Upload the data ---------------------------------------------------------

hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))


# Prepare the data --------------------------------------------------------

hike_data2<-hike_data%>%mutate(length2 = gsub( "\\s.*", "", length),
                               rating=as.numeric(rating),
                               length2=as.numeric(length2),
                               gain=as.numeric(gain),
                               highpoint=as.numeric(highpoint),
                               length_band = case_when(length2 <= 2.5 ~ "0 - 2.5",
                                                       length2 <=5 ~"2.5 - 5",
                                                       length2 <= 7.5 ~ "5 - 7.5",
                                                       length2 <= 10 ~ "7.5 - 10",
                                                       length2 > 10 ~ "Higher than 10"),
                               length_band = as_factor(length_band),
                               gain_band = case_when(gain <= 2500~ "0 - 2500",
                                                       gain <=5000 ~"2500 - 5000",
                                                     gain <= 7500 ~ "5000 - 7500",
                                                     gain <= 10000 ~ "7500 - 10000",
                                                     gain > 10000 ~ "Higher than 10000"),
                               gain_band = as_factor(gain_band),
                               highpoint_band = case_when(highpoint <= 2500~ "0 - 2500",
                                                          highpoint <=5000 ~"2500 - 5000",
                                                          highpoint <= 7500 ~ "5000 - 7500",
                                                          highpoint <= 10000 ~ "7500 - 10000",
                                                          highpoint > 10000 ~ "Higher than 10000"),
                               highpoint_band = as_factor(highpoint_band)) 




# First graph -------------------------------------------------------------

hike_data_lenght<-hike_data2%>% group_by(length_band)%>%summarize(avg_rating=mean(rating,na.rm = TRUE))

g<- ggplot(hike_data_lenght, aes(x=reorder(length_band,avg_rating), y=avg_rating)) +
  geom_bar(stat="identity", fill="#69b3a2", width=0.4) +
  coord_flip(ylim=c(2.5,3.75)) +
  theme_ipsum() +
  labs(title = "Length of trail in miles") +
  guides(fill = NULL) +
  theme(plot.title = element_text(margin = margin(b = 8), 
                                  color = "#808080",face = "bold",size = 10,
                                  hjust = 0.5,
                                  family = font_labels),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    legend.position = "none",
    axis.text.x    = element_blank(), 
    axis.text.y    = element_text(face = "bold",size = 8, color = "#808080",margin = margin(t = 0, r = 20, b = 0, l = 0)),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  ) + geom_text(aes(label=round(avg_rating,2)),hjust=-0.5,color = "#808080", size = 3, fontface = "italic",family = font_labels) 



# Second graph ------------------------------------------------------------

hike_gain_band<-hike_data2%>% group_by(gain_band)%>%summarize(avg_rating=mean(rating,na.rm = TRUE))

g2<- ggplot(hike_gain_band, aes(x=reorder(gain_band,avg_rating), y=avg_rating)) +
  geom_bar(stat="identity", fill="#69b3a2", width=0.4) +
  coord_flip(ylim=c(1.5,4)) +
  theme_ipsum() +
  labs(title = "Gain in elevation (Feet above sea level)") +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size=48)
  ) +
  guides(fill = NULL) +
  theme(plot.title = element_text(margin = margin(b = 8), 
                                  color = "#808080",face = "bold",size = 10,
                                  hjust = 0.5,
                                  family = font_labels),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        axis.text.x    = element_blank(), 
        axis.text.y    = element_text(face = "bold",size = 8, color = "#808080",margin = margin(t = 0, r = 20, b = 0, l = 0)),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.ticks = element_blank()
  )  + geom_text(aes(label=round(avg_rating,2)),hjust=-0.5,color = "#808080", size = 3, fontface = "italic",family = font_labels) 




# Third graph -------------------------------------------------------------

hike_highpoint_band<-hike_data2%>% group_by(highpoint_band)%>%summarize(avg_rating=mean(rating,na.rm = TRUE))

g3<- ggplot(hike_highpoint_band, aes(x=reorder(highpoint_band,avg_rating), y=avg_rating)) +
  geom_bar(stat="identity", fill="#69b3a2", width=0.4) +
  coord_flip(ylim=c(2.5,5)) +
  theme_ipsum() +
  labs(title = "Highest point in feet above sea level") +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text( size=48 )
  ) +
  guides(fill = NULL) +
  theme(plot.title = element_text(margin = margin(b = 8), color = "#808080",face = "bold",size = 10,  hjust = 0.5,family = font_labels),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        axis.text.x    = element_blank(), 
        axis.text.y    = element_text(face = "bold",size = 8, color = "#808080",margin = margin(t = 0, r = 20, b = 0, l = 0)),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.ticks = element_blank()
  )  + geom_text(aes(label=round(avg_rating,2)),hjust=-0.5,color = "#808080", size = 3, fontface = "italic",family = font_labels) 





# Patchwork ---------------------------------------------------------------

plot_arrange <-(g|g2|g3) + plot_annotation(title = "Washington Hiking",
                                           subtitle = "Average ratings per each dimension",
                                          caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)",
                                          theme = theme(plot.title = element_text(margin = margin(t=10, b = 5), 
                                                                                  color = "#e3d18f",face = "bold", size = 15,
                                                                                  hjust = 0.5,
                                                                                  family = font_labels),
                                                        plot.subtitle = element_text(margin = margin(t=5, b = 5), 
                                                                                  color = "#808080",face = "bold",size = 8,
                                                                                  hjust = 0.5,
                                                                                  family = font_labels),
                                                        plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                                                                     color = "#808080", size = 8, family = font_labels,
                                                                                     hjust = 0.95),
                                                        plot.background = element_rect(fill = "#f7f7f7"),
                                                        panel.border = element_blank()))

plot_arrange

