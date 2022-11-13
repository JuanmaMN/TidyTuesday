# Upload packages ---------------------------------------------------------

# devtools::install_github("jeromefroe/circlepackeR")

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, ggtext, hrbrthemes, gridExtra,patchwork, systemfonts, sysfonts,showtext, ggpubr,
               extrafont, systemfonts,circlepackeR,packcircles,viridis, ggflags)

# Upload data -------------------------------------------------------------

state_stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/state_stations.csv')

station_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/station_info.csv')

# Font --------------------------------------------------------------------

extrafont::loadfonts(device = "all", quiet = TRUE)

font_add_google("Roboto Condensed")

font_labels <- "Roboto Condensed"

showtext_auto()

# Prepare the data --------------------------------------------------------

station_join <- state_stations %>% dplyr::right_join(station_info, by = c("call_sign"))

station_join2 <- station_join %>% group_by(state) %>%summarize (count = n())

quantile(station_join2$count, c(.33, .66)) # for colour

station_join2<-station_join2%>% na.omit() %>%
  mutate(count= as.numeric(count),
         colour = case_when(count < 23 ~ "#d1495b",
                            count >= 23  & count < 41 ~ "#edae49",
                            count >= 41 ~ "#00798c",
                            TRUE ~ "#d3d3d3"))


station_join2_packing_art <- station_join2 %>% 
  circleProgressiveLayout(sizecol = "count", sizetype = 'area') %>% 
  mutate(radius = radius * 0.85)

dat.gg_station <- circleLayoutVertices(station_join2_packing_art, npoints = 51)

# Graph -------------------------------------------------------------------

p_first_graph <- 
  ggplot() + 
  geom_polygon(data = dat.gg_station, aes(x, y, group = id, fill = factor(id)),
               colour = NA, alpha = 0.8) +
  scale_size_continuous(range = c(1, 3)) +
  scale_fill_manual(values = station_join2$colour) +
  coord_equal() + 
  theme(legend.position="none") +
  theme(
    legend.position = "none",
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y =  element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) 

station_join2_all_state<-station_join2 %>%
  arrange(desc(count))%>%
  mutate(x_axis = rep(1:5, each = 10, length = n()),
         y_axis = rep(10:1, length = n()),
         state = str_replace(state,"_", " "))

p_second_graph_all<-station_join2_all_state %>%ggplot() +
  geom_text(aes(x = x_axis, y = y_axis, label = state,color = colour), hjust = 0, 
            fontface = "bold",  size = 3.2, alpha = 0.8,
            family = font_labels) +
  scale_color_manual(values = c( "#edae49" = "#edae49",
                                 "#d1495b" = "#d1495b",
                                 "#00798c" = "#00798c")) +
  
  scale_x_continuous(limits = c(1,6)) +
  scale_y_continuous(limits = c(0,10.5)) +
  theme_void() + 
  theme(legend.position="none") +
  theme(
    legend.position = "none",
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y =  element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) 

# Both graphs -------------------------------------------------------------

ggarrange(p_first_graph,p_second_graph_all, ncol=2, nrow=1, common.legend = FALSE)+
  theme_ipsum() +
  labs(x = "",y = "",
       title = "FM Radio Stations in the US",
       subtitle = "States with<span style='color:#d1495b',text-align:left;>*less than 23*</span>,
       <span style='color:#edae49',text-align:left';> between 23 and 41</span>,
       <span style='color:#00798c',text-align:left';> more than 41</span> FM stations",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#0c1618",face = "bold",size = 14,
                              hjust = 0,
                              family = font_labels),
    plot.subtitle = element_markdown(hjust = 0, size = 12, family = font_labels, valign = 0),
    plot.caption =  element_text(margin = margin(t = 20, b = 10), face = "plain",
                                 color = "#0c1618", size = 8, family = font_labels, hjust = 0.5),
    plot.background = element_rect(fill = "#f7f7f7", color = NA))  


