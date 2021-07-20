
# Upload data -------------------------------------------------------------

drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

# Upload the packages -----------------------------------------------------


pacman::p_load(readxl,readr, lubridate, tidyverse, ggplot2, scales, ggtext, hrbrthemes, gridExtra,patchwork, systemfonts, sysfonts,showtext, ggpubr, choroplethr,choroplethrMaps,
               choroplethrZip,mapproj,hrbrthemes, usmap,ggfittext,htmltools,reactable,patchwork,scales,ggtext, ggpubr)



# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Quicksand")

font_add_google("Work Sans")

font_labels <- "Quicksand"

font_labels2 <- "Work Sans"

showtext_auto()



# First graph -------------------------------------------------------------


drought_count_3<-drought %>% filter (drought_lvl == "D3" & area_pct > 0) %>% group_by(state_abb)%>% summarize(n=n(),
                                                                                                              percentage2= n/1044)
drought_count_3b<- statepop %>% left_join(drought_count_3, by= c("abbr" = "state_abb")) %>%  ungroup()

drought_count_3b<-drought_count_3b%>%
  mutate(
    color = case_when(percentage2 < 0.1 ~ "< 10%",
                      percentage2 >= 0.1 & percentage2 < 0.2 ~ "10% - 20%",
                      percentage2 >= 0.2 & percentage2 < 0.3 ~ "20% - 30%",
                      percentage2 >= 0.3 & percentage2 < 0.4 ~ "30% - 40%",
                      percentage2 >= 0.4 ~ "> 40%",
                      TRUE ~ "No droughts"))

drought_count_3b$color <- fct_relevel(drought_count_3b$color, c("> 40%","30% - 40%",
                                                                  "20% - 30%","10% - 20%","< 10%"))




drought_count_3b_map<-plot_usmap(data = drought_count_3b, values = "color",labels = TRUE) + 
  scale_fill_manual(values = c( "< 10%" = "#ffdfa4",
                                "10% - 20%" = "#FFC457",
                                "20% - 30%"= "#E4683F",
                                "30% - 40%"= "#C03434",
                                "> 40%" = "#98103E",
                                "No droughts"="white")) +
  labs(fill = "color") +
  labs(x = "",y = "",
       title = "% of Weeks with extreme drought",
       x = "",
       y = "") +
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



# Second graph ------------------------------------------------------------


drought_count_4<-drought %>% filter (drought_lvl == "D4" & area_pct > 0) %>% group_by(state_abb)%>% summarize(n=n(),
                                                                                                              percentage2= n/1044)


drought_count_4b<- statepop %>% left_join(drought_count_4, by= c("abbr" = "state_abb")) %>%  ungroup()

drought_count_4b<-drought_count_4b%>%
  mutate(
    color = case_when(percentage2 < 0.1 ~ "< 10%",
                      percentage2 >= 0.1 & percentage2 < 0.2 ~ "10% - 20%",
                      percentage2 >= 0.2 & percentage2 < 0.3 ~ "20% - 30%",
                      percentage2 >= 0.3 & percentage2 < 0.4 ~ "30% - 40%",
                      percentage2 >= 0.4 ~ "> 40%",
                      TRUE ~ "No droughts"))



drought_count_4bb$color <- fct_relevel(drought_count_4b$color, c("> 40%","30% - 40%",
                                                                "20% - 30%","10% - 20%","< 10%"))


drought_count_4b_map<-plot_usmap(data = drought_count_4b, values = "color",labels = TRUE) + 
  scale_fill_manual(values = c( "< 10%" = "#ffdfa4",
                                "10% - 20%" = "#FFC457",
                                "20% - 30%"= "#E4683F",
                                "30% - 40%"= "#C03434",
                                "> 40%" = "#98103E",
                                "No droughts"="white")) +
  labs(fill = "color") +
  labs(x = "",y = "",
       title = "% of Weeks with exceptional drought",
       x = "",
       y = "") +
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



# Patchwork ---------------------------------------------------------------

patch_dr<- (drought_count_3b_map | drought_count_4b_map)  +
  plot_layout(guides = "collect")


Ppatch_dr <- patch_dr + plot_annotation(title = "Where are the Most Drought-Prone Areas?",
                                    subtitle = "",
                                    caption = "Source: Inspired by Becky Bolinger map, data from U.S. Drought Monitor, 2001-2021 \n\n Visualization: JuanmaMN (Twitter @Juanma_MN)",
                                    theme = theme(plot.title = element_text(margin = margin(t=15,b = 8), 
                                                                            color = "#000000",face = "bold",size = 14,
                                                                            hjust = 0.5,
                                                                            family = font_labels),
                                                  plot.subtitle = element_text(margin = margin(t=10, b = 25), 
                                                                               color = "#000000", size = 10, family = font_labels,
                                                                               hjust = 0.5),
                                                  plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                                                               color = "#000000", size = 8, family = font_labels,
                                                                               hjust = 0.95),                                                  ,
                                                  plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border
                                                  panel.background = element_rect(color=NA, fill = "#fbfaf6"),
                                                  legend.position = "bottom",
                                                  legend.background = element_rect(fill = "#fbfaf6",
                                                                                   size = 1),
                                                  legend.title = element_blank(),
                                                  panel.border = element_blank()))


Ppatch_dr