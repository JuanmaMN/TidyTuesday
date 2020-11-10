
# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts)


# Europe ------------------------------------------------------------------

mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')

landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')


# Prepare the data --------------------------------------------------------

mobile2<-mobile%>%filter(continent == "Oceania") %>% select(entity, year, mobile_subs) %>% na.omit()

font_add_google("Lora")

font_labels <- "Lora"


# Graph -------------------------------------------------------------------

mobile2%>%ggplot(aes(year, mobile_subs, group = entity)) +
  geom_area(alpha = 0.4,fill="#e13d3d",colour="#e13d3d") +
  facet_wrap(~ entity, ncol = 5) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(50, 100, by = 50)) +
  scale_x_continuous(limits = c(1990, 2017), breaks = seq(1990, 2017, by = 5)) +
  labs(x = "",y = "",
       title = "Historical Phone Usage",
       subtitle = "Fixed mobile subscriptions (per 100 people) in Oceania",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")+
  #scale_x_discrete(position = "top") +
  guides(fill = NULL) +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#22222b", size = 9,
                                 hjust = 0.5,
                                 family = font_labels),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10,
                                 hjust = 0.95),
    axis.title.x = element_text(margin = margin(t = 10),
                                color = "#22222b",
                                family = font_labels),
    axis.title.y = element_text(margin = margin(r = 15), 
                                color = "#22222b",
                                family = font_labels),
    legend.position = "none",
    #axis.text.x    = element_text(color = "#22222b"),
    axis.text.y    = element_text(color = "#22222b"),
    panel.background = element_blank(), 
    #panel.grid.major = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.major.y =  element_line(colour = "#f0f0f0", size = 0.25),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) + 
  geom_text(aes(x = 1990, y = 20, label = entity), hjust = 0, nudge_x = 0.01, face = "bold",family = font_labels,size = 3, colour = "#22222b")