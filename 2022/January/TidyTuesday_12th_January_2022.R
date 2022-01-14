
# Upload data -------------------------------------------------------------

stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')


# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts, showtext,ggflags,usmap)



# Font --------------------------------------------------------------------

font_add_google("DM Serif Display")

font_labels <- "DM Serif Display"

font_add_google("Lora")

font_labels2 <- "Lora"

showtext_auto()


# Prepare the data --------------------------------------------------------

# Compare 2015 - 2021 January-March Bivariate mapping

stressor_new<-stressor%>% filter(months == "January-March" & 
                                   year %in% c("2015", "2021") &
                                   stressor == "Varroa mites") %>%
  mutate(state=str_to_lower(state))

#Join

stressor_new_2map<- stressor_new%>% full_join(statepop, by= c("state"="full"))

# Prepare 

stressor_new_2map<-stressor_new_2map%>%
  mutate(
    color = case_when(stress_pct < 10 ~ "< 10%",
                      stress_pct >= 10 & stress_pct < 30 ~ "10% - 30%",
                      stress_pct >= 30 & stress_pct < 50 ~ "30% - 50%",
                      stress_pct >= 50 ~ "> 50%",
                      TRUE ~ "No value"))

stressor_new_2map$color <- fct_relevel(stressor_new_2map$color, c("No value","> 50%","30% - 50%",
                                                                "10% - 30%","< 10%"))

stressor_new_2map$color <- factor(stressor_new_2map$color, c("> 50%","30% - 50%",
                                                                  "10% - 30%","< 10%","No value"))


# 2015 --------------------------------------------------------------------

stressor_new_2map2015<-stressor_new_2map%>%filter(year == "2015" | color == "No value")

stressor_new_2015_map<-plot_usmap(data = stressor_new_2map2015, values = "color",labels = TRUE) + 
  scale_fill_manual(values = c(  "No value"= "#c3c3c3",
                                 "< 10%" = "#ffdfa4",
                               "10% - 30%" = "#ea8c55",
                                "30% - 50%"= "#d15656",
                                "> 50%" = "#701f1f"
                               )) + 
  labs(fill = "color") +
  labs(x = "",y = "",
       title = "Jan to March 2015",
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
    legend.background =  element_rect(fill = "#f7f7f7", color = NA),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    panel.background = element_rect(color=NA, fill = "#f7f7f7")
  ) +  guides(fill = guide_legend(
    label.position = "bottom",
    family = font_labels, 
    color = "#808080",
    keywidth = 3, keyheight = 0.5))




# 2021 --------------------------------------------------------------------



stressor_new_2map2021<-stressor_new_2map%>%filter(year == "2021" | color == "No value")


stressor_new_2021_map<-plot_usmap(data = stressor_new_2map2021, values = "color",labels = TRUE) + 
                                    scale_fill_manual(values = c(  "No value"= "#c3c3c3",
                                                                   "< 10%" = "#ffdfa4",
                                                                   "10% - 30%" = "#ea8c55",
                                                                   "30% - 50%"= "#d15656",
                                                                   "> 50%" = "#701f1f"))+
          labs(fill = "color") +
           labs(x = "",y = "", title = "Jan to March 2021",   x = "", y = "") +
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
    legend.background =  element_rect(fill = "#f7f7f7", color = NA),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    panel.background = element_rect(color=NA, fill = "#f7f7f7")
  ) +  guides(fill = guide_legend(
    label.position = "bottom",
    family = font_labels, 
    color = "#808080",
    keywidth = 3, keyheight = 0.5))
                                    
                                  
                                  



ggarrange(stressor_new_2015_map,stressor_new_2021_map, ncol=2, nrow=1, common.legend = TRUE, legend="top") +
  theme_ipsum() +
  labs(x = "",y = "",
       title ="Bee Colony losses",
       subtitle = "Percent of colonies affected by stressor. Varroa mites analysis.",
              caption = "Source: #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)"
  ) +
  theme(
    plot.title = element_text(margin = margin(b = 20), 
                              color = "#343434",face = "bold",size = 24,
                              hjust = 0,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(b = 20), 
                              color = "#343434",size = 16,
                              hjust = 0,
                              family = font_labels),
    plot.caption =  element_text(margin = margin(t = 30, b = 10), 
                                 color = "#343434", size = 10, family = font_labels2,
                                 hjust = 0.5),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),
    legend.background =  element_rect(fill = "#f7f7f7", color = NA))  








