
# Upload the data ---------------------------------------------------------

mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')

landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')


# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts)


# Join the data source ----------------------------------------------------

TT_join<- mobile%>% inner_join(landline, by= c("entity","code","year","continent")) %>% filter(year =="2017") %>% select(entity,mobile_subs,landline_subs) %>% na.omit()


# Prepare the data for graph ----------------------------------------------

TT_join_bi<-TT_join%>% group_by(entity)  %>%
  summarize(
    mobile_subs = median(mobile_subs),
    landline_subs = median(landline_subs)
  ) %>% 
  bi_class(x = mobile_subs, y =landline_subs, style = "quantile", dim = 3)


# Rename countries

TT_join_bi$entity <- recode(TT_join_bi$entity, "United Kingdom" = "UK")
TT_join_bi$entity <- recode(TT_join_bi$entity, "United States" = "USA")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Russian Federation" = "Russia")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Antigua and Barbuda" = "Antigua")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Bahamas, The" = "Bahamas")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Cabo Verde" = "Cape Verde")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Congo, Dem. Rep." = "Democratic Republic of the Congo")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Egypt, Arab Rep." = "Egypt")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Gambia, The" = "Gambia")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Iran, Islamic Rep." = "Iran")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Kyrgyz Republic" = "Kyrgyzstan")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Lao PDR" = "Laos")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Micronesia, Fed. Sts." = "Micronesia")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Sint Maarten (Dutch part)" = "Sint Maarten")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Slovak Republic" = "Slovakia")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Syrian Arab Republic" = "Syria")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Trinidad and Tobago" = "Trinidad")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Venezuela, RB" = "Venezuela")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Virgin Islands (U.S.)" = "Virgin Islands")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Yemen, Rep." = "Yemen")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Korea, Dem. People's Rep." = "North Korea")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Korea, Rep." = "South Korea")
TT_join_bi$entity <- recode(TT_join_bi$entity, "North Macedonia" = "Macedonia")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Brunei Darussalam" = "Brunei")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Cote d'Ivoire" = "Ivory Coast")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Micronesia, Fed. Sts." = "Micronesia")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Yemen, Rep." = "Yemen")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Democratic Republic of Congo" = "Democratic Republic of the Congo")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Micronesia (country)" = "Micronesia")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Saint Vincent and the Grenadines" = "Saint Vincent")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Timor" = "Timor-Leste")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Congo" = "Republic of Congo")
TT_join_bi$entity <- recode(TT_join_bi$entity, "Saint Kitts and Nevis" = "Saint Kitts")




# Join the data with world data set ---------------------------------------

world <- map_data("world") %>%   filter(region != "Antarctica") 


world_TT_join_bi <- TT_join_bi%>%
  left_join(world, by = c('entity'='region'))


font_add_google("Lora")

font_labels <- "Lora"

# Graph -------------------------------------------------------------------

world_TT_join_bi_graphp <- ggplot() + 
  geom_map(data = world, map = world,
           aes(long, lat, group = group,  map_id = region),
           fill = "#282828", color = "#282828") +
  geom_map(data =world_TT_join_bi, map = world,
           aes(fill = bi_class, map_id = entity),
           color = "#282828", size = 0.15, alpha = .8) +
  bi_scale_fill(pal = "GrPink", dim = 3, guide = F) +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#ffffff",face = "bold",size = 9,
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
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#4f4f4f"),
    panel.border = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  ) 


# Legend ------------------------------------------------------------------

legend_TT2 <- 
  bi_legend(pal = "GrPink",
            dim = 3,
            xlab = "Mobile subscriptions",
            ylab = "Telephone subscriptions",
            size = 2.5) +
  theme(rect = element_rect(fill = "grey10"),
        panel.border = element_blank(),
        axis.text = element_blank(),
        plot.background = element_rect(fill = "#4f4f4f"),
        axis.title.x = element_text(size = 10,
                                    color = "grey70"),
        axis.title.y = element_text(size = 10,
                                    color = "grey70"),
        legend.text = element_text(size = 5),
        legend.text.align = 0)


# Cowplot -----------------------------------------------------------------

map_legend_TT_10_11 <- ggdraw() +
  draw_plot(world_TT_join_bi_graphp, 0, 0, 1, 1) +
  draw_plot(legend_TT2, 0, 0.1, 0.2, 0.2) +
  draw_label("Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)", 
             color = "grey70", size = 7.5, angle = 0, x = 0.9, y = 0.05) +
  draw_label("Fixed mobile & telephone subscriptions (per 100 people) in 2017", 
             color = "#faf0d2", size = 10, angle = 0, x =0.5, y = 0.97) 


map_legend_TT_10_11 