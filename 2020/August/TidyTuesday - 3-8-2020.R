
# Upload packages ---------------------------------------------------------


pacman::p_load(tidyverse,lubridate, tidyverse, ggplot2, hrbrthemes,patchwork,ggimage,ggdraw, maps, viridis,
               biscale, cowplot, grid, gridtext,hrbrthemes,scales,ggtext, ggpubr)


# Raw data ----------------------------------------------------------------

energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')

View(energy_types)

# Prepare the data --------------------------------------------------------

# Add Greece and United Kingdom

energy_types<-energy_types%>% select(1:4,7) %>% 
  mutate(country_name = case_when(
            country =="EL" ~ "Greece",
            country == "UK"  ~ "UK",
            T ~ as.character(country_name))
)


# Modify column names

names(energy_types)[5]<-"Energy_2018"

names(energy_types)[2]<-"region"

#Modify row names to prepare the data for join

energy_types$region <- recode(energy_types$region, "Czechia" = "Czech Republic", 
                                    "North Macedonia" = "Macedonia",
                                    "Bosnia & Herzegovina" = "Bosnia and Herzegovina")


energy_types<-energy_types%>% filter (type !="Other") %>%  
mutate(type_2 = case_when(
  (type== "Conventional thermal" | type  ==  "Nuclear") ~ "Nonrenewable",
  (type== "Geothermal" | type  ==  "Wind" | type  ==  "Solar" | type  ==  "Hydro" | type  ==  "Pumped hydro power")  ~ "Renewable",
  T ~ "Others"
)) %>% group_by(country, region, type_2) %>%
  summarize (energy_2018 = sum(Energy_2018))%>% 
  pivot_wider(names_from = type_2, values_from = energy_2018)%>%
  group_by(country,region)%>%
  summarize(
    Renewable = median(Renewable),
    Nonrenewable = median(Nonrenewable)
  ) %>% 
  bi_class(x = Renewable, y = Nonrenewable, style = "quantile", dim = 3)


# World Data --------------------------------------------------------------

world <- map_data("world")

worldmap <- ggplot() + theme(
  panel.background = element_rect(fill = "lightcyan1",
                                  color = NA),
  panel.grid = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
)


map_europe <- inner_join(world, energy_types, by =  "region")

# Graph -------------------------------------------------------------------

europe <- worldmap + coord_fixed(xlim = c(-9, 42.5),
                                 ylim = c(36, 70.1),
                                 ratio = 1.5)


europe2 <- europe + geom_polygon(data = map_europe,
                                 aes(fill= bi_class,
                                   x = long,
                                     y = lat,
                                     group = group),
                                 color = "grey70") +
  bi_scale_fill(pal = "DkCyan", dim = 3, guide = F) +
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
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f3f3f3", color = NA),
    panel.border = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.ticks = element_blank()
  ) 



legend_europe <- 
  bi_legend(pal = "DkCyan",
            dim = 3,
            xlab = "Renewable prod",
            ylab = "Non-Renewable prod",
            size = 5) +
  theme(rect = element_rect(fill = "grey10"),
        panel.border = element_blank(),
        axis.text = element_blank(),
        plot.background = element_rect(fill = "#f3f3f3", color = NA),
        axis.title.x = element_text(size = 10,
                                    color = "#a1a1a1",
                                    face = "bold"),
        axis.title.y = element_text(size = 10,
                                    color = "#a1a1a1",
                                    face = "bold"),
        legend.text = element_text(size = 5),
        legend.text.align = 0)


grob_energy <- grobTree(richtext_grob(
  sprintf("Renewable and Non-renewable energy production of<br><br><b style='color:%s'>European countries</b><br><br>in 2018 in GWh  (Gigawatt hours)","#e3d18f"),  
  x=.2,y=.9, hjust=0.5, gp=gpar(col = "#a1a1a1", fontsize=15), vjust = 1))




grob_energy_Norway  <- grobTree(richtext_grob(
  sprintf("<b style='color:%s'>Norway</b> was the largest producer <br>of renewable energy in hydropower.<br><br>
          95.12 pct of total energy production .<br>(renewable and non-renewable) in 2018 <br>was from  hydropower (hydro, pumped).","#00008b"),  
  x=.8,y=.6, hjust=0.5, gp=gpar(col = "#a1a1a1", fontsize=10), vjust = 1))



map_legend_europe <- ggdraw() +
  draw_plot(europe2, 0, 0, 1, 1) +
  draw_plot(legend_europe, 0, 0.1, 0.2, 0.2) +
  draw_label("Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)", 
             color = "#a1a1a1", size = 7.5, angle = 0, x = 0.8, y = 0.05) +
  theme(
    plot.background = element_rect(fill = "#f3f3f3", color = NA)
  ) +
  annotation_custom(grob_energy) +
  annotation_custom(grob_energy_Norway)  
  
  


map_legend_europe



########################################################
###############     Change colours       ###############
########################################################


# Upload packages ---------------------------------------------------------


pacman::p_load(tidyverse,lubridate, tidyverse, ggplot2, hrbrthemes,patchwork,ggimage,ggdraw, maps, viridis,
               biscale, cowplot, grid, gridtext,hrbrthemes,scales,ggtext, ggpubr)


# Raw data ----------------------------------------------------------------

energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')


# Prepare the data --------------------------------------------------------

# Add Greece and United Kingdom

energy_types<-energy_types%>% select(1:4,7) %>% 
  mutate(country_name = case_when(
    country =="EL" ~ "Greece",
    country == "UK"  ~ "UK",
    T ~ as.character(country_name))
  )


# Modify column names

names(energy_types)[5]<-"Energy_2018"

names(energy_types)[2]<-"region"

#Modify row names to prepare the data for join

energy_types$region <- recode(energy_types$region, "Czechia" = "Czech Republic", 
                              "North Macedonia" = "Macedonia",
                              "Bosnia & Herzegovina" = "Bosnia and Herzegovina")


energy_types_second_graph<-energy_types%>% filter (type !="Other") %>%  
  mutate(type_2 = case_when(
    (type== "Conventional thermal" | type  ==  "Nuclear") ~ "Nonrenewable",
    (type== "Geothermal" | type  ==  "Wind" | type  ==  "Solar" | type  ==  "Hydro" | type  ==  "Pumped hydro power")  ~ "Renewable",
    T ~ "Others"
  )) %>% group_by(country, region, type_2) %>%
  summarize (energy_2018 = sum(Energy_2018))%>% 
  pivot_wider(names_from = type_2, values_from = energy_2018)%>%
  group_by(country,region)%>%
  summarize(
    Nonrenewable = median(Nonrenewable),
    Renewable = median(Renewable)
  ) %>% 
  bi_class(x =Nonrenewable, y = Renewable, style = "quantile", dim = 3)


# World Data --------------------------------------------------------------

world <- map_data("world")

worldmap <- ggplot() + theme(
  panel.background = element_rect(fill = "lightcyan1",
                                  color = NA),
  panel.grid = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
)


map_europe <- inner_join(world, energy_types_second_graph, by =  "region")

# Graph -------------------------------------------------------------------

europe <- worldmap + coord_fixed(xlim = c(-9, 42.5),
                                 ylim = c(36, 70.1),
                                 ratio = 1.5)


europe2_second_graph <- europe + geom_polygon(data = map_europe,
                                              aes(fill= bi_class,
                                                  x = long,
                                                  y = lat,
                                                  group = group),
                                              color = "grey70") +
  bi_scale_fill(pal = "DkCyan", dim = 3, guide = F) +
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
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f3f3f3", color = NA),
    panel.border = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.ticks = element_blank()
  ) 



legend_europe <- 
  bi_legend(pal = "DkCyan",
            dim = 3,
            xlab = "Non-Renewable prod",
            ylab = "Renewable prod",
            size = 5) +
  theme(rect = element_rect(fill = "grey10"),
        panel.border = element_blank(),
        axis.text = element_blank(),
        plot.background = element_rect(fill = "#f3f3f3", color = NA),
        axis.title.x = element_text(size = 10,
                                    color = "#a1a1a1",
                                    face = "bold"),
        axis.title.y = element_text(size = 10,
                                    color = "#a1a1a1",
                                    face = "bold"),
        legend.text = element_text(size = 5),
        legend.text.align = 0)


grob_energy <- grobTree(richtext_grob(
  sprintf("Renewable and Non-renewable energy production of<br><br><b style='color:%s'>European countries</b><br><br>in 2018 in GWh  (Gigawatt hours)","#e3d18f"),  
  x=.2,y=.9, hjust=0.5, gp=gpar(col = "#a1a1a1", fontsize=15), vjust = 1))




grob_energy_Norway  <- grobTree(richtext_grob(
  sprintf("<b style='color:%s'>Norway</b> was the largest producer <br>of renewable energy in hydropower.<br><br>
          95.12 pct of total energy production .<br>(renewable and non-renewable) in 2018 <br>was from  hydropower (hydro, pumped).","#20b261"),  
  x=.8,y=.6, hjust=0.5, gp=gpar(col = "#a1a1a1", fontsize=10), vjust = 1))



map_legend_europe_second_graph <- ggdraw() +
  draw_plot(europe2_second_graph , 0, 0, 1, 1) +
  draw_plot(legend_europe, 0, 0.1, 0.2, 0.2) +
  draw_label("Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)", 
             color = "#a1a1a1", size = 7.5, angle = 0, x = 0.8, y = 0.05) +
  theme(
    plot.background = element_rect(fill = "#f3f3f3", color = NA)
  ) +
  annotation_custom(grob_energy) +
  annotation_custom(grob_energy_Norway)  




map_legend_europe_second_graph 






