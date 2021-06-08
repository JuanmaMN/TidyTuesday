
# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,sf,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm, 
               ggalt)

# Upload data source ------------------------------------------------------


fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Quicksand")

font_add_google("Work Sans")

font_labels <- "Quicksand"

font_labels2 <- "Work Sans"

showtext_auto()


# Prepare the data --------------------------------------------------------


fishing_fr2<-fishing%>% filter(species %in% c("Cisco", "Alewife", "Lake Whitefish", "Yellow Perch")) %>% group_by(year, species)%>%
  summarize(total=sum(values, na.rm=T)) 



fishing_fr2%>%ggplot(aes(x=year, y = total, group = species)) +
  geom_area(alpha = 0.4,fill="#b9cad4",colour="#8aa6b7") +
  facet_wrap(~ species, ncol = 2) +
  scale_y_continuous(breaks = seq(0, 150000,by =50000),limits=c(0, 150000), labels = c(" ","50,000","100,000", " "),
                     expand = c(.007, .007)) + 
  geom_text(aes(x = 1870, y = 125000, label = species), hjust = 0, nudge_x = 0.01, face = "bold",family = font_labels,size = 8, colour = "#8aa6b7") +
  labs(y = "",
       x = "",
       title = "Commercial Fishing - Top 4 species by production amounts",
       subtitle =  " ",
       caption =  "Source: #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")  +
  theme(
    plot.title = element_text(
      color = "#22222b",face = "bold",size = 20,
      hjust = 0,
      family = font_labels),
    plot.subtitle = element_text(margin = margin(t=5, b= 30),
                                 color = "#22222b", size = 12, family = font_labels,
                                 hjust = 0),
    plot.caption =  element_text(margin = margin(t = 50), 
                                 color = "#22222b", size = 10,
                                 hjust = 0.99,
                                 family = font_labels),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.x    = element_text(color = "#525252", family = font_labels, size = 12),  # hjust = 1 is not needed for aligment
    axis.text.y    = element_text(color = "#525252",family = font_labels, size = 12),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
    axis.ticks = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_blank()) +
  geom_segment(aes(x=1867,xend=2015,y=0,yend=0),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=1867,xend=2015,y=50000,yend=50000),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=1867,xend=2015,y=100000,yend=100000),linetype="dotted",colour = "#525252") 