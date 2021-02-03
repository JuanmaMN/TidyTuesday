
# Upload data -------------------------------------------------------------

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')



# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggflags, showtext)



# Prepare the data for graph ----------------------------------------------


plastics_wrangle_2019<- plastics%>% filter(parent_company == "Grand Total" & year == "2019") %>% 
  select(country, year, grand_total, volunteers)

plastics_wrangle_2019_bi<-plastics_wrangle_2019 %>% 
  bi_class(x = grand_total, y = volunteers, style = "quantile", dim = 3)


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Lora")

font_labels <- "Lora"

showtext_auto()


# Rename countries (not all  countries are in the dataset)

plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "United Kingdom" = "UK")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "United States of America" = "USA")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Russian Federation" = "Russia")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Antigua and Barbuda" = "Antigua")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Bahamas, The" = "Bahamas")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Cabo Verde" = "Cape Verde")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Congo, Dem. Rep." = "Democratic Republic of the Congo")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Egypt, Arab Rep." = "Egypt")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Gambia, The" = "Gambia")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Iran, Islamic Rep." = "Iran")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Kyrgyz Republic" = "Kyrgyzstan")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Lao PDR" = "Laos")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Micronesia, Fed. Sts." = "Micronesia")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Sint Maarten (Dutch part)" = "Sint Maarten")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Slovak Republic" = "Slovakia")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Syrian Arab Republic" = "Syria")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Trinidad and Tobago" = "Trinidad")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Venezuela, RB" = "Venezuela")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Virgin Islands (U.S.)" = "Virgin Islands")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Yemen, Rep." = "Yemen")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Korea, Dem. People's Rep." = "North Korea")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Korea, Rep." = "South Korea")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "North Macedonia" = "Macedonia")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Brunei Darussalam" = "Brunei")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Cote D_ivoire" = "Ivory Coast")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Micronesia, Fed. Sts." = "Micronesia")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Yemen, Rep." = "Yemen")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Democratic Republic of Congo" = "Democratic Republic of the Congo")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Micronesia (country)" = "Micronesia")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Saint Vincent and the Grenadines" = "Saint Vincent")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Timor" = "Timor-Leste")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Congo" = "Republic of Congo")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Saint Kitts and Nevis" = "Saint Kitts")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "Taiwan_ Republic of China (ROC)" = "Taiwan")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "NIGERIA" = "Nigeria")
plastics_wrangle_2019_bi$country <- recode(plastics_wrangle_2019_bi$country, "ECUADOR" = "Ecuador")





# Join the data with world data set ---------------------------------------

world <- map_data("world") %>%   filter(region != "Antarctica") 

world_TT_join_bi_plastics <- plastics_wrangle_2019_bi%>%
  left_join(world, by = c('country'='region'))



world_TT_join_bi_graphp_plastics <- ggplot() + 
  geom_map(data = world, map = world,
           aes(long, lat, group = group,  map_id = region),
           fill = "#282828", color = "#282828") +
  geom_map(data =world_TT_join_bi_plastics, map = world,
           aes(fill = bi_class, map_id = country),
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
    plot.background = element_rect(fill = "#f7f7f7"),
    panel.border = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  ) 


# Legend ------------------------------------------------------------------

legend_TT2_plastics <- 
  bi_legend(pal = "GrPink",
            dim = 3,
            xlab = "Total count (all types of plastic)",
            ylab = "Number of volunteers",
            size = 2.5) +
  theme(rect = element_rect(fill = "grey10"),
        panel.border = element_blank(),
        axis.text = element_blank(),
        plot.background = element_rect(fill = "#f7f7f7"),
        axis.title.x = element_text(size = 10,
                                    family = font_labels,
                                    color = "grey70"),
        axis.title.y = element_text(size = 10,
                                    family = font_labels,
                                    color = "grey70"),
        legend.text = element_text(size = 5,family = font_labels),
        legend.text.align = 0)


# Cowplot -----------------------------------------------------------------

map_legend_TT_10_11_plastics <- ggdraw() +
  draw_plot(world_TT_join_bi_graphp_plastics, 0, 0, 1, 1) +
  draw_plot(legend_TT2_plastics, 0, 0.1, 0.2, 0.2) +
  draw_label("Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)", 
             color = "grey70", size = 7.5, angle = 0, x = 0.9, y = 0.05, fontfamily = font_labels) +
  draw_label("Plastic Pollution in 2019", 
             color = "#e13d3d", size = 10, angle = 0, x =0.5, y = 0.97,fontfamily = font_labels) + 
  draw_label("Countries with no data in 2019,\n no color has been assigned", 
             color = "grey70", size = 7.5, angle = 0, x = 0.1, y = 0.05,fontfamily = font_labels) 


map_legend_TT_10_11_plastics



# IMAGES ------------------------------------------------------------------

img = c("Unilever_nobg.png","Coca_Cola_bg.png","Blue_water_nobg.png","Pepsi_nobg.png","Nestl_nobg.png", "Universal_Robina.png") # Logos are there in my directory


# Upload the packages -----------------------------------------------------
pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext)


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Lora")

font_labels <- "Lora"

showtext_auto()

# plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')


plastics<-plastics


plastics_wrangle_2019_total_companies_1<- plastics%>% filter(parent_company %in% c("Universal Robina Corporation",
                                                                                   "Unilever",
                                                                                   "PepsiCo",
                                                                                   "Nestlé",
                                                                                   "Pure Water, Inc.",
                                                                                   "The Coca-Cola Company")  & year =="2019")%>%
  group_by(parent_company) %>% summarize(volunteers_total=sum(volunteers, na.rm = TRUE),
                                         grand_total=sum(grand_total,na.rm = TRUE))  %>%
  mutate(x_axis_grand = c(17.5,17.5,17.5,17.5,17.5,17.5),
         x_axis_grand_2 = x_axis_grand,
         y_axis = c(0.27,0.27,0.27,0.27,0.27,0.27),
         y_axis_2 = c(0.3,0.3,0.3,0.3,0.3,0.3)) %>%
  mutate(images = c(img[5],img[4], img[3],img[2], img[1],img[6]),
         grand_total = comma(grand_total),
         volunteers_total = comma(volunteers_total),
         #label = paste0(grand_total, "\n", volunteers_total),
         label = paste0("Total plastic count"," ", "-"," ", grand_total),
         label_2 = paste0("Total number of volunteers"," ", "-"," ", volunteers_total)) %>% 
  arrange(desc(grand_total)) %>% arrange(desc(grand_total))


# Ordenar companies

plastics_wrangle_2019_total_companies_1$parent_company <- fct_relevel(plastics_wrangle_2019_total_companies_1$parent_company, 
                                                                      c("The Coca-Cola Company","Pure Water, Inc.","Nestlé",
                                                                        "PepsiCo","Unilever","Universal Robina Corporation" ))


graph<-plastics_wrangle_2019_total_companies_1 %>% 
  ggplot() +
  geom_text(aes(x = x_axis_grand, 
                y = y_axis,
                label = label_2),
            size = 3.5,
            family = font_labels) +
  geom_text(aes(x = x_axis_grand_2, 
                y = y_axis_2,
                label = label),
            size = 3.5,
            family = font_labels) +
  geom_image(aes(x = 17.5, y = 0.4, image = images), size = .5) +
  xlim(17.45,17.55) +
  ylim(0.25, 0.43) +
  facet_wrap(~parent_company, 
             strip.position = "bottom")+
  theme_ipsum() +
  theme(plot.title = element_text(margin = margin(t=10, b = 10), 
                                  color = "#e13d3d", size = 20, family = font_labels,
                                  face = "bold",
                                  hjust = 0.5),
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
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank()
  ) 


# Country -----------------------------------------------------------------

plastics_wrangle_2019_total_country_top<- plastics%>% filter (parent_company == "Grand Total" &
                                                                year == "2019") %>% select (1, grand_total, volunteers)


plastics_wrangle_2019_total_country_top_six<- plastics%>% filter (parent_company == "Grand Total" &
                                                                    year == "2019") %>% select (1, grand_total, volunteers) %>%
  filter (country %in% c("Taiwan_ Republic of China (ROC)",
                         "NIGERIA",
                         "Philippines",
                         "Indonesia",
                         "ECUADOR",
                         "Vietnam")) %>%  
  mutate(code = case_when(
    country == "Taiwan_ Republic of China (ROC)" ~ "tw",
    country == "NIGERIA" ~ "ng",
    country == "Philippines"  ~ "ph",
    country == "Indonesia"  ~ "id",
    country == "ECUADOR"  ~ "ec",
    country == "Vietnam"  ~ "vn",
    TRUE ~ country)) %>%
  mutate(x_axis_grand = c(17.5,17.5,17.5,17.5,17.5,17.5),
         x_axis_grand_2 = x_axis_grand,
         y_axis = c(0.27,0.27,0.27,0.27,0.27,0.27),
         y_axis_2 = c(0.3,0.3,0.3,0.3,0.3,0.3),
         label = paste0(grand_total, "\n", volunteers),
         grand_total = comma(grand_total),
         volunteers_total = comma(volunteers),
         label = paste0("Total plastic count"," ", "-"," ", grand_total),
         label_2 = paste0("Total number of volunteers"," ", "-"," ", volunteers_total))




plastics_wrangle_2019_total_country_top_six$country <- fct_relevel(plastics_wrangle_2019_total_country_top_six$country , 
                                                                   c("Taiwan_ Republic of China (ROC)",
                                                                     "NIGERIA" ,
                                                                     "Philippines",
                                                                     "Indonesia",
                                                                     "ECUADOR" ,
                                                                     "Vietnam"))



graph_2<-plastics_wrangle_2019_total_country_top_six%>% 
  ggplot() +
  geom_text(aes(x = x_axis_grand, 
                y = y_axis,
                label = label_2),
            size = 3.5,
            family = font_labels) +
  geom_text(aes(x = x_axis_grand_2, 
                y = y_axis_2,
                label = label),
            size = 3.5,
            family = font_labels) +
  xlim(17.45,17.55) +
  ylim(0.25, 0.43) +
  geom_flag(aes(y = 0.4, x = 17.5, country = code), hjust = -2, size = 20)  +
  facet_wrap(~country,strip.position = "bottom") +
  theme_ipsum()  +
  theme(plot.title = element_text(margin = margin(t=10, b = 10), 
                                  color = "#e13d3d", size = 20, family = font_labels,
                                  face = "bold",
                                  hjust = 0.5),
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
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank()
  ) 




ggarrange(graph,graph_2, ncol=2, nrow=1, common.legend = FALSE)+
  theme_ipsum() +
  labs(x = "",y = "",
       title = "Plastic Pollution in 2019",
       subtitle = "",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(t=20,b = 8), 
                              color = "#e13d3d",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10, b = 25), 
                                 color = "#525252", size = 10, family = font_labels,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                 color = "#808080", size = 8, family = font_labels,
                                 hjust = 0.95),
    plot.background = element_rect(fill = "#f7f7f7", color = NA))  




