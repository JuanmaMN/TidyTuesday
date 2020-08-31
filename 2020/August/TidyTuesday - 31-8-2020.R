
# Upload the data ---------------------------------------------------------

tractors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_yields_vs_tractor_inputs_in_agriculture.csv')


# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot)


# Prepare the data --------------------------------------------------------

# Rename columns

names(tractors)[1]<-"Country"
names(tractors)[5]<-"Cereal_yield"
names(tractors)[6]<-"Population"

tractors$Country <- recode(tractors$Country, "Syrian Arab Republic" = "Syria")



tractors2<-tractors%>%filter(Year =="2016") %>% filter(Country != "World" & Country != "North America")%>%
  select(1,5,6)%>%group_by(Country)%>%
  summarize(
    Cereal_yield = sum(Cereal_yield, na.rm=TRUE),
    Population = sum(Population,  na.rm=TRUE)
  ) %>% filter(Cereal_yield>0 & Population>0)


# Prepare the data for graph

tractors_bi2<-tractors2%>% na.omit()%>%group_by(Country)  %>%
  summarize(
    Cereal_yield = median(Cereal_yield),
    Population = median(Population)
  ) %>% 
  bi_class(x = Cereal_yield, y = Population, style = "quantile", dim = 3)


# Rename countries

tractors_bi2$Country <- recode(tractors_bi2$Country , "United Kingdom" = "UK")
tractors_bi2$Country <- recode(tractors_bi2$Country , "United States" = "USA")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Russian Federation" = "Russia")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Antigua and Barbuda" = "Antigua")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Bahamas, The" = "Bahamas")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Cabo Verde" = "Cape Verde")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Congo, Dem. Rep." = "Democratic Republic of the Congo")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Egypt, Arab Rep." = "Egypt")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Gambia, The" = "Gambia")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Iran, Islamic Rep." = "Iran")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Kyrgyz Republic" = "Kyrgyzstan")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Lao PDR" = "Laos")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Micronesia, Fed. Sts." = "Micronesia")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Sint Maarten (Dutch part)" = "Sint Maarten")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Slovak Republic" = "Slovakia")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Syrian Arab Republic" = "Syria")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Trinidad and Tobago" = "Trinidad")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Venezuela, RB" = "Venezuela")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Virgin Islands (U.S.)" = "Virgin Islands")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Yemen, Rep." = "Yemen")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Korea, Dem. People's Rep." = "North Korea")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Korea, Rep." = "South Korea")
tractors_bi2$Country <- recode(tractors_bi2$Country , "North Macedonia" = "Macedonia")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Brunei Darussalam" = "Brunei")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Cote d'Ivoire" = "Ivory Coast")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Micronesia, Fed. Sts." = "Micronesia")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Yemen, Rep." = "Yemen")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Democratic Republic of Congo" = "Democratic Republic of the Congo")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Micronesia (country)" = "Micronesia")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Saint Vincent and the Grenadines" = "Saint Vincent")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Timor" = "Timor-Leste")
tractors_bi2$Country <- recode(tractors_bi2$Country , "Congo" = "Republic of Congo")



# Join the data with world data set ---------------------------------------

world <- map_data("world") %>%   filter(region != "Antarctica") 

world_tractors_by_2 <- tractors_bi2%>%
  left_join(world, by = c('Country'='region'))


# Check NA values ---------------------------------------------------------

NAvalues<-world_tractors_by[is.na(world_tractors_by$lat),]
NAvalues2<-world_tractors_by[is.na(world_tractors_by$Cereal_yield),]


# Graph -------------------------------------------------------------------

ptidyTuesday2 <- ggplot() + 
  geom_map(data = world, map = world,
           aes(long, lat, group = group,  map_id = region),
           fill = "#282828", color = "#282828") +
  geom_map(data =world_tractors_by_2, map = world,
           aes(fill = bi_class, map_id = Country),
           color = "#282828", size = 0.15, alpha = .8) +
  bi_scale_fill(pal = "GrPink", dim = 3, guide = F) +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#ffffff",face = "bold",size = 9,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#ffffff", size = 6, family = "Arial",
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#ffffff", size = 5, family = "Arial",
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
            xlab = "Cereal yield in kg per hectare",
            ylab = "Total Population",
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

map_legend_TT2 <- ggdraw() +
  draw_plot(ptidyTuesday2, 0, 0, 1, 1) +
  draw_plot(legend_TT2, 0, 0.1, 0.2, 0.2) +
  draw_label("Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)", 
             color = "grey70", size = 7.5, angle = 0, x = 0.8, y = 0.05) +
  draw_label("Countries with no data in 2016,\n no color has been assigned", 
             color = "grey70", size = 7.5, angle = 0, x = 0.1, y = 0.05)  +
  draw_label("Cereal yield (kg per hectare) and Population analysis in 2016", 
             color = "#faf0d2", size = 14, angle = 0, x =0.5, y = 0.97) 


map_legend_TT2





