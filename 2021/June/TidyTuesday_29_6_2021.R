# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, scales, ggtext, ggpubr,ggbump,wesanderson,
               grid, gridtext, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)

# install.packages("openintro")

library(openintro)

# Datasets ----------------------------------------------------------------

data(london_boroughs) 

animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')


data_animal<-animal_rescues %>% mutate(property_category = case_when(
  property_category == "Dwelling" ~ "Dwelling",
  TRUE ~ "Outdoors"))  %>% group_by (property_category)%>% summarize (n=n())


# Prepare the data --------------------------------------------------------

animal_rescues_c<-animal_rescues%>%mutate(borough = str_to_title(borough)) %>% group_by(borough,property_category) %>% summarize (n=n())


animal_rescues_c$borough<- fct_recode(animal_rescues_c$borough, 
                                      "Barking & Dagenham" = "Barking And Dagenham",
                                       "Hammersmith & Fulham" = "Hammersmith And Fulham",
                                      "Kensington & Chelsea" = "Kensington And Chelsea",
                                      "Kingston" = "Kingston Upon Thames",
                                      "Richmond" = "Richmond Upon Thames")



london_boroughs<-london_boroughs %>% na.omit()

borough_join<- animal_rescues_c%>% left_join(london_boroughs, by= "borough") 

borough_join_c <- borough_join %>% mutate(
  color = case_when(
    n < 70  ~ "< 70",
    n >= 70 & n < 100  ~ "70 to 100",
    n >= 100 & n < 150  ~ "100 to 150",
    n >= 150   ~ "> 150",
    TRUE ~ "Others"
  ))


borough_join_c$color <- fct_relevel(borough_join_c$color, c("> 150",
                                                                  "100 to 150","70 to 100","< 70"))



# borough_join_DF <- borough_join[is.na(borough_join$x),]  #see what's mmissing

# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Work Sans")

font_labels <- "Work Sans"

showtext_auto()



# First graph -------------------------------------------------------------

borough_join_dwelling <- borough_join_c  %>% filter (property_category == "Dwelling")

p_dwelling <- ggplot() +
  geom_polygon(data=borough_join_dwelling, aes(x=x, y=y, group = borough, fill = color),
               colour="white" ) +
  scale_fill_manual(values = c( "< 70" = "#E5E5E5",
                                "70 to 100" = "#DDA4AC",
                                "100 to 150"= "#E27080",
                                "> 150"= "#B84080")) +
  labs(title = "Dwelling") +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#B84080",face = "bold",size = 12,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#ffffff", size = 6, family = font_labels,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#ffffff", size = 5, family = font_labels,
                                 hjust = 0.95),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "#fbfaf6", color = NA), 
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
    panel.border = element_blank(),
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.margin=margin(b = 0.5, unit='cm'),
    legend.background = element_rect(fill = "#fbfaf6"),
    legend.text=element_text(size=8),
    strip.background = element_rect(fill = "#fbfaf6"),
    strip.text.x = element_blank(),
    legend.key.size = unit(0.1, "cm"))+ 
  guides(fill = guide_legend(
    label.position = "bottom",
    family = font_labels, 
    color = "#808080",
    keywidth = 4, keyheight = 0.5)) 




# Second graph ------------------------------------------------------------


borough_join_outdoors <- borough_join_c  %>% filter (property_category == "Outdoor")

View(borough_join_outdoors)
p_outdoor <- ggplot() +
  geom_polygon(data=borough_join_outdoors, aes(x=x, y=y, group = borough, fill = color),
               colour="white" ) +
  scale_fill_manual(values = c( "< 70" = "#E5E5E5",
                                "70 to 100" = "#DDA4AC",
                                "100 to 150"= "#E27080",
                                "> 150"= "#B84080")) +
  labs(title = "Outdoors") +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#B84080",face = "bold",size = 12,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#ffffff", size = 6, family = font_labels,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#ffffff", size = 5, family = font_labels,
                                 hjust = 0.95),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    #legend.position = "none",
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "#fbfaf6", color = NA), 
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
    panel.border = element_blank(),
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.margin=margin(b = 0.5, unit='cm'),
    legend.background = element_rect(fill = "#fbfaf6"),
    legend.text=element_text(size=8),
    strip.background = element_rect(fill = "#fbfaf6"),
    strip.text.x = element_blank(),
    legend.key.size = unit(0.3, "cm"))+ 
  guides(fill = guide_legend(
    label.position = "bottom",
    family = font_labels, 
    color = "#808080",
    keywidth = 4, keyheight = 0.5)) 
 


# Arrange -----------------------------------------------------------------

ggarrange(p_dwelling,p_outdoor, ncol=2, nrow=1, common.legend = TRUE, legend="bottom") +
  theme_ipsum() +
  labs(x = "",y = "",
       title = "Animal Rescues in London",
       subtitle = "Number of animal rescues attended by London firefighters across London from 2006 to 2021",
       caption = "Source: London.gov & #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#343434",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10, b = 25), 
                                 color = "#000000", size = 10, family = font_labels,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                 color = "#808080", size = 8, family = font_labels,
                                 hjust = 0.95),
    plot.background = element_rect(fill = "#fbfaf6", color = NA))  