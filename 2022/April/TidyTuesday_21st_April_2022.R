
# Upload the data ---------------------------------------------------------

indoor_pollution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/indoor_pollution.csv')


# Upload packages ---------------------------------------------------------

pacman::p_load(lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts, showtext,ggflags,usmap)



# Prepare the data --------------------------------------------------------

indoor_pollution_2<- indoor_pollution %>% filter(Year == "2019") %>% rename(perct = contains('Deaths')) %>%
  mutate(colour = case_when(
    perct < 1 ~ "< 1%",
    perct >= 1 & perct < 2.5 ~ "1 - 2.5%",
    perct >= 2.5 & perct < 5 ~ "2.5 - 5%",
    perct >= 5 & perct < 7.5 ~ "5 - 7.5%",
    perct >= 7.5 & perct < 10 ~ "7.5 - 10%",
    perct >= 10 & perct < 12.5 ~ "10 - 12.5%",
    perct >= 12.5 ~ "> 12.5%",
    TRUE ~ "Others"
  ))


# Bring World Data --------------------------------------------------------

world <- map_data("world") %>%   filter(region != "Antarctica") 
world <- world %>% mutate (region = case_when(subregion == " US" & region == "Virgin Islands" ~ "United States Virgin Islands",TRUE ~ region))

world$region <- recode(world$region, "UK" = "United Kingdom")
world$region <- recode(world$region, "USA" = "United States")
world$region <- recode(world$region, "Trinidad" = "Trinidad and Tobago")
world$region <- recode(world$region, "Saint Vincent" = "Saint Vincent and the Grenadines")
world$region <- recode(world$region, "Saint Kitts" = "Saint Kitts and Nevis")
world$region <- recode(world$region, "Macedonia" = "North Macedonia")
world$region <- recode(world$region, "Micronesia" = "Micronesia (Entity)")
world$region <- recode(world$region, "Democratic Republic of the Congo" = "Democratic Republic of Congo")
world$region <- recode(world$region, "Republic of Congo" = "Congo")
world$region <- recode(world$region, "Czech Republic" = "Czechia")
world$region <- recode(world$region, "Ivory Coast" = "Cote d'Ivoire")
world$region <- recode(world$region, "Antigua" = "Antigua and Barbuda")


# Join both tables --------------------------------------------------------

indoor_pollution_3 <- indoor_pollution_2%>%
  left_join(world, by = c('Entity' ='region'))


# I haven't matched Eswatini, Timor, Tokelau and Tuvalu as they are not in the world data.

# Check NA ----------------------------------------------------------------

indoor_pollution_3_Na <-indoor_pollution_3%>% filter(is.na(lat)) # ensure I'm not missing any country.


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("DM Serif Display")

font_labels <- "DM Serif Display"

showtext_auto()


# Graph  ------------------------------------------------------------------

indoor_pollution_graph_2022 <- ggplot() + 
  geom_map(data = world, map = world,
           aes(long, lat, group = group,  map_id = region),
           fill = "#525252", color = "#525252") +
  geom_map(data =indoor_pollution_3, map = world,
           aes(fill = colour, map_id = Entity),
           color = "#525252", size = 0.15, alpha = .8) +
  scale_fill_manual(values = c(  
    "< 1%" = "#ffffcc",
    "1 - 2.5%" = "#c7e9b4",
    "2.5 - 5%" = "#7fcdbb",
    "5 - 7.5%" = "#41b6c4",
    "7.5 - 10%" = "#1d91c0",
    "10 - 12.5%" = "#225ea8",
    "> 12.5%" = "#0c2c84",
    "No value"= "#525252"
  )) +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "",y = "",
       title = "Share of deaths from indoor air pollution, 2019",
       subtitle = "Share of deaths (%), from any cause, which are attributed to indoor air pollution - from burning solid fuels - as a risk factor.",
       caption = "Source: https://ourworldindata.org/\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 20,
                              hjust = 0,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#22222b", face = "plain", size = 14, family = font_labels,
                                 hjust = 0),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10,family = font_labels,
                                 hjust = 0.95),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_text(size=8, color = "#22222b",family = font_labels),
    legend.title.align = 0,
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#d9e2e7"),
    panel.border = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "bottom",
    legend.text=element_text(size=8, color = "#22222b", family = font_labels),
    legend.key.size = unit(0.2, "cm"),
    legend.key = element_blank(),
    legend.background=element_blank(),
    axis.ticks = element_blank(),
    legend.margin=margin(b = 0.1, unit='cm')
  ) +
  guides(fill = guide_legend(
    title="",
    title.position = "top",
    label.position = "bottom",
    nrow = 1,
    family = font_labels, 
    color = "black",
    keywidth = 3, keyheight = 0.5))


indoor_pollution_graph_2022
