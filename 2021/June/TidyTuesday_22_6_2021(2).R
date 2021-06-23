
# Upload ------------------------------------------------------------------

parks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv')

# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, scales, ggtext, ggpubr,ggbump,wesanderson,
               grid, gridtext, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)

# Prepare the data --------------------------------------------------------


parks_bump<-parks %>% filter(year %in% c("2016", "2017", "2018", "2019", "2020")) %>%select (year, rank, city) %>%
  filter (city %in% c("Minneapolis", "Washington, D.C.", "St. Paul", "Arlington, Virginia", "Cincinnati","Portland", "Irvine", "San Francisco",
                      "Boston", "Chicago")) 


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Quicksand")

font_add_google("Work Sans")

font_labels <- "Quicks&"

font_labels2 <- "Work Sans"

showtext_auto()


# Colours -----------------------------------------------------------------

wp_colours<-c("#ff4141", "#e6d492", "#86a7ae", "#b576af", "#e39e65","#80a8b0", "#ae633b","#b8c375","#A4D4CC","#D9D3B8") 


# Graph -------------------------------------------------------------------

cities_graph<-ggplot(parks_bumprank, aes(year, rank, color = city)) +
  geom_point(size = 7) +
  geom_text(data = parks_bumprank %>% filter(year == min(year)),
            aes(x = year, label = city), size = 4.5, hjust = 1,nudge_x = -0.2,family = font_labels) +
  geom_text(data = parks_bumprank%>% filter(year == max(year)),
            aes(x = year, label = city), size = 4.5, hjust = 0,nudge_x = 0.2,family = font_labels) +
  geom_bump(aes(smooth = 6), size = 1.5) +
  scale_color_manual(values = wp_colours) +
  scale_y_reverse(limits = c(10, 1),
                  breaks = seq(1, 10, 1)) +
  scale_x_continuous(limits = c(2015, 2021), breaks = seq(2015, 2021, 1), labels = c(" ", "2016","2017","2018","2019",
                                                                                     "2020"," " )) +
  labs(y = "",
       x = "",
       title = "",
       subtitle =  "",
       caption =  "")  +
  guides(fill = NULL) +
  theme(
    plot.title = element_text(margin = margin(b = 10, t= 10), 
                              color = "#e6d492",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#a1a1a1", size = 10,
                                 hjust = 0.95,
                                 family = font_labels),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    legend.position = "none",
    axis.text.x    = element_text(color = "#a1a1a1",family = font_labels, size = 10),
    axis.text.y    = element_blank(), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    plot.background = element_rect(fill = "#f2f5e5", color = NA),    # color removes the border,
    axis.ticks = element_blank()
  )  


ggarrange(cities_graph)+
  theme_ipsum() +
  labs(x = " ",y = "",
       title = " ParkScore Index - Top 10 by points",
       subtitle = "",
       caption = "Source: #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 10, t= 10), 
                              color = "#b8860b",face = "bold",size = 24,
                              hjust = 0.5,
                              family = font_labels),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#a1a1a1", size = 10,
                                 hjust = 0.95,
                                 family = font_labels),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    plot.background = element_rect(fill = "#f2f5e5", color = NA))  

