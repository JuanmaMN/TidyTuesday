# Upload data -------------------------------------------------------------

stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')


# Upload packages ---------------------------------------------------------

pacman::p_load(lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts, showtext,ggflags,usmap)


# Prepare the data --------------------------------------------------------

stations_group <- stations %>% mutate(Year= substr(DATE_LAST_CONFIRMED, start = 1, stop = 4)) %>% 
  filter(Year == "2022" & FUEL_TYPE_CODE == "ELEC") %>% group_by(STATE)%>%summarize(n=n())
 

stations_group_3b<-stations_group  %>% left_join(statepop, by= c("STATE" = "abbr")) %>%  ungroup()

stations_group_3c<-stations_group_3b%>%
  mutate(
    color = case_when(n < 1000 ~ "< 1000",
                      # n >= 250 & n < 500  ~ "250 - 500",
                     # n >= 500 & n < 1000 ~ "500 - 1000",
                      n >= 1000 & n < 2000 ~ "1000 - 2000",
                      #n >= 1500 & n < 2000 ~ "1500 - 2000",
                      n >= 2000 ~ "> 2000",
                      TRUE ~ "Outliers"))

stations_group_3c$color <- fct_relevel(stations_group_3c$color, c("< 1000",
                                                                  "1000 - 2000","> 2000"))

# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("DM Serif Display")

font_labels <- "DM Serif Display"

showtext_auto()

# Graph -------------------------------------------------------------------

stations_count_3c_map<-plot_usmap(data = stations_group_3c, values = "color",labels = TRUE) + 
  scale_fill_manual(values = c( "< 1000" = "#e3eddf",
                               # "500 - 1000" = "#B7E4C7",
                                "1000 - 2000"= "#c6dabf",
                                #"1500 - 2000"= "#74C69D",
                                "> 2000" = "#b8c375"))+
  labs(fill = "color") +
  labs(x = "",y = "",
       title = "Total number of alternative stations in 2022 - Electric",
       x = "",
       y = "",
       caption = "Source: U.S. Department of Transportation\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 15, t=10), 
                              color = "#22222b",face = "bold",size = 12,
                              hjust = 0.5,
                              family = font_labels),
    legend.position = "bottom",
    legend.title=element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    legend.margin=margin(b = 0.5, unit='cm'),
    legend.text=element_text(size=8,family = font_labels),
    legend.key.size = unit(0.3, "cm"),
    legend.background =  element_rect(fill = "#fbfaf6", color = NA),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border
    panel.background = element_rect(color=NA, fill = "#fbfaf6")
  ) +  guides(fill = guide_legend(
    label.position = "bottom",
    family = font_labels, 
    color = "#808080",
    keywidth = 3, keyheight = 0.5))



stations_count_3c_map
