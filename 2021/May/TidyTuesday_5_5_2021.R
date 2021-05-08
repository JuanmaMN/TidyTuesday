
# Upload the data ---------------------------------------------------------

water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')



# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,sf,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)



# Prepare the data --------------------------------------------------------


water2<-water%>% mutate(year = str_sub(report_date, 7,10)) %>% group_by(country_name)%>% summarize(n=n()) 

water2$country_name <- recode(water2$country_name, "Congo - Brazzaville" = "Congo",
                              "Congo - Kinshasa" = "Congo")




Country_code_11_10_2020 <- read_excel("Country_code - 11-10-2020.xlsx")

names(Country_code_11_10_2020)[1]<-"country_name"

water2_joing <-
  water2 %>% 
  left_join(Country_code_11_10_2020, by = "country_name")%>%
  mutate(
    color=case_when(
      n < 10000 ~ "< 10k",
      n >= 10000 & n <= 30000 ~ "10k to 30k",
      TRUE ~  "> 30k"
    )
  ) 


water2_joing$color <- fct_relevel(water2_joing$color, c("> 30k","10k to 30k","< 10k"))


water2_joinga <-
  sf_world %>% 
  full_join(water2_joing, by = c("ISO_A3" = "Country Code")) 

water2_joinga<-water2_joinga %>% filter(continent == "Africa")  %>% na.omit() 


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Quicksand")

font_add_google("Work Sans")

font_labels <- "Quicksand"

font_labels2 <- "Work Sans"

showtext_auto()


# Graph -------------------------------------------------------------------


p<-water2_joinga %>%
  ggplot(aes(fill = color)) +
  geom_sf() +
  coord_sf() + 
  scale_fill_manual(values = c( "< 10k" = "#cbe1ee",
                                "10k to 30k" = "#6baace",
                                "> 30k"= "#35779e")) +
  labs(y = "",
       x = "",
       title = "Water Access Points",
       subtitle =  "Total number of water acccess points in Africa",
       caption =  "Source: TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(
      color = "#22222b",face = "bold",size = 14,
      hjust = 0,
      family = font_labels),
    plot.subtitle = element_text(margin = margin(t=5, b= 30),
                                 color = "#22222b", size = 10, family = font_labels,
                                 hjust = 0),
    plot.caption =  element_text(margin = margin(t = 50), 
                                 color = "#22222b", size = 8,
                                 #hjust = 0.99,
                                 family = font_labels),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), 
    axis.ticks.x=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.y =  element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border
    panel.background = element_rect(fill = "#fbfaf6", color = NA), 
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text=element_text(size=8, color = "#22222b",family = font_labels),
    legend.key.size = unit(0.2, "cm"),
    legend.key = element_blank(),
    legend.background=element_blank(),
    legend.margin=margin(b = 0.1, unit='cm')
  ) +
  guides(fill = guide_legend(
    label.position = "bottom",
    nrow = 1,
    family = font_labels, 
    color = "#525252",
    keywidth = 2, keyheight = 0.5))



cowplot::ggdraw(p) + theme(panel.background = element_rect(fill = "#fbfaf6", color = NA))

