# Upload the packages -----------------------------------------------------

pacman::p_load(readxl,readr, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, scales, ggtext, ggpubr,ggbump,wesanderson,
               grid, gridtext, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm, stringr, maps, socviz, ggthemes)

library(tidycensus)

county_us <- read_csv("county_us.csv")

county_us <- county_us %>%select(2:7)

county_us<-county_us%>% mutate(id = recode(id,"46113" = "46102"))


# TidyTuesday -------------------------------------------------------------


drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')

drought_fips<-drought_fips %>% mutate(
  state_code = substr(FIPS, 1, 2),
  county_code = substr(FIPS, 3, 5)
) %>% filter (date == "2022-06-07" ) %>%
  rename(state = State, fips = FIPS)%>%left_join(fips_codes,by = c("state",
                                                                   "state_code",
                                                                   "county_code"))  



drought_fips<-drought_fips%>%left_join(county_us, by = c("fips" = "id"))

drought_fips<- drought_fips %>% mutate(color=
                                         case_when(DSCI == 0 ~ "Zero",
                                                   DSCI > 0 & DSCI < 100  ~ "D0 - Abnormally Dry",
                                                   DSCI >= 100 & DSCI < 200  ~ "D1 - Moderate Drought",
                                                   DSCI >= 200 & DSCI < 300  ~ "D2 - Severe Drought",
                                                   DSCI >= 300 & DSCI < 400  ~ "D3 - Extreme Drought",
                                                   DSCI >= 0 & DSCI <= 500  ~ "D4 - Exceptional Drought",
                                                   TRUE ~ "Others"
                                         ))


# Check the data ----------------------------------------------------------

drought_fips_others<-drought_fips %>% filter(color == "Others")

# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("DM Serif Display")

font_labels <- "DM Serif Display"

showtext_auto()


# graph -------------------------------------------------------------------


p_US <- ggplot(data = drought_fips,
               mapping = aes(x = long, y = lat,
                             fill = color, 
                             group = group,
                             label = state)) +
  scale_fill_manual(values = c( "D0 - Abnormally Dry" = "#ffff00",
                                    "D1 - Moderate Drought" = "#ffcc99",
                                    "D2 - Severe Drought"= "#ff6600",
                                    "D3 - Extreme Drought"= "#ff0000",
                                    "D4 - Exceptional Drought"= "#660000",
                                "Zero" = "#dad9d9")) +
  geom_polygon(color = "gray90", size = 0.05) + coord_equal() +
  theme_map() +
  guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "",y = "",
       title = "Drought Conditions in the US",
       subtitle = "Date - 2022-06-07",
       caption = "Source: National Integrated Drought Information System\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 12,
                              hjust = 0,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#22222b", size = 10, family = font_labels,
                                 hjust = 0),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 7, family = font_labels,
                                 hjust = 0.95),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    #legend.position = "none",
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border,
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border,
    panel.border = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "bottom",
    legend.text=element_text(size=6, color = "#22222b"),
    legend.key.size = unit(0.2, "cm"),
    legend.key = element_blank(),
    legend.background=element_blank(),
    axis.ticks = element_blank(),
    legend.margin=margin(b = 0.1, unit='cm')
  ) +
  guides(fill = guide_legend(
    label.position = "bottom",
    nrow = 1,
    family = font_labels, 
    color = "#525252",
    keywidth = 3, keyheight = 0.5))


