
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


# Prepare the data for graph ----------------------------------------------

captured_data<-captured_vs_farmed%>%filter(Year == "2018") %>% rename('Aquaculture_production' = 'Aquaculture production (metric tons)',
                                                                      'Capture_fisheries_production' = 'Capture fisheries production (metric tons)')


captured_data_bi<-captured_data%>% group_by(Entity)  %>%
  summarize(
    Aquaculture_production = median(Aquaculture_production),
    Capture_fisheries_production = median(Capture_fisheries_production)
  ) %>% 
  bi_class(y = Aquaculture_production, x = Capture_fisheries_production, style = "quantile", dim = 3)


captured_data_bi$Entity <- recode(captured_data_bi$Entity, "United Kingdom" = "UK")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "United States" = "USA")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Russian Federation" = "Russia")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Antigua and Barbuda" = "Antigua")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Bahamas, The" = "Bahamas")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Cabo Verde" = "Cape Verde")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Congo, Dem. Rep." = "Democratic Republic of the Congo")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Egypt, Arab Rep." = "Egypt")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Gambia, The" = "Gambia")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Iran, Islamic Rep." = "Iran")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Kyrgyz Republic" = "Kyrgyzstan")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Lao PDR" = "Laos")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Micronesia, Fed. Sts." = "Micronesia")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Sint Maarten (Dutch part)" = "Sint Maarten")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Slovak Republic" = "Slovakia")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Syrian Arab Republic" = "Syria")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Trinidad and Tobago" = "Trinidad")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Venezuela, RB" = "Venezuela")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Virgin Islands (U.S.)" = "Virgin Islands")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Yemen, Rep." = "Yemen")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Korea, Dem. People's Rep." = "North Korea")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Korea, Rep." = "South Korea")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "North Macedonia" = "Macedonia")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Brunei Darussalam" = "Brunei")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Cote d'Ivoire" = "Ivory Coast")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Micronesia, Fed. Sts." = "Micronesia")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Yemen, Rep." = "Yemen")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Democratic Republic of Congo" = "Democratic Republic of the Congo")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Micronesia (country)" = "Micronesia")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Saint Vincent and the Grenadines" = "Saint Vincent")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Timor" = "Timor-Leste")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Congo" = "Republic of Congo")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Saint Kitts and Nevis" = "Saint Kitts")
captured_data_bi$Entity <- recode(captured_data_bi$Entity, "Czechia" = "Czech Republic")




# Join the data with world data set ---------------------------------------

world <- map_data("world") %>%   filter(region != "Antarctica") 

world_captured_data_join_bi <- captured_data_bi%>%
  left_join(world, by = c('Entity'='region'))


# check NA

world_captured_data_join_bi_Na<- world_captured_data_join_bi %>% filter(is.na(lat))

# Graph -------------------------------------------------------------------

world_fish_TT_join_bi_graphp <- ggplot() + 
  geom_map(data = world, map = world,
           aes(long, lat, group = group,  map_id = region),
           fill = "#282828", color = "#282828") +
  geom_map(data =world_captured_data_join_bi, map = world,
           aes(fill = bi_class, map_id = Entity),
           color = "#282828", size = 0.15, alpha = .8) +
  bi_scale_fill(pal = "DkCyan", dim = 3, guide = "none") +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  theme(
    plot.title = element_text(margin = margin(b = 15, t=10), 
                              color = "#808080",face = "bold",size = 14,
                              hjust = 0,
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
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#d4ebf2", color = NA),
    panel.border = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  ) 



# Legend ------------------------------------------------------------------

legend_fish <- 
  bi_legend(pal = "DkCyan",
            dim = 3,
            xlab = "Capture fisheries prod. (mt)",
            ylab = "Aquaculture prod. (mt)",
            size = 2.5) +
  theme(rect = element_rect(fill = "grey10"),
        panel.border = element_blank(),
        axis.text = element_blank(),
        plot.background = element_rect(fill = "#d4ebf2", color = NA),
        axis.title.x = element_text(size = 10,
                                    color = "#808080"),
        axis.title.y = element_text(size = 10,
                                    color = "#808080"),
        legend.text = element_text(size = 5),
        legend.text.align = 0)
 


# Cowplot -----------------------------------------------------------------

map_legend_TT_fish <- ggdraw() +
  draw_plot(world_fish_TT_join_bi_graphp, 0, 0, 1, 1) +
  draw_plot(legend_fish, 0, 0.1, 0.2, 0.2) +
  draw_label("Source:#Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)", 
             color = "#808080", size = 7.5, angle = 0, x = 0.9, y = 0.05, fontfamily = font_labels) +
  draw_label("Aquaculture and Capture fisheries production in 2018", 
             color = "#808080", size = 14, angle = 0, x =0.5, y = 0.97, fontfamily = font_labels) 


map_legend_TT_fish