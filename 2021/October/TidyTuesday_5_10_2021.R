# Upload packages ---------------------------------------------------------

pacman::p_load(readxl,readr, lubridate, tidyverse, ggplot2, ggtext, hrbrthemes, gridExtra,patchwork, systemfonts, sysfonts,showtext, ggpubr, choroplethr,choroplethrMaps,
               choroplethrZip,mapproj,hrbrthemes, usmap,ggfittext,htmltools,reactable,patchwork,scales,ggtext, ggpubr)


# Upload the data ---------------------------------------------------------

nurses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv')

names(nurses)[3]<-"Total_nurses"

names(nurses)[21]<- "Total_Employed_Healthcare_State_Aggregate"

# Filter by 2020 ----------------------------------------------------------

Total_Employed_Registered_Nurses_2020<- nurses %>% filter (Year =="2020") %>% mutate(State = str_to_lower(State)) %>% group_by(State)%>%
  mutate(pect = Total_nurses/Total_Employed_Healthcare_State_Aggregate,
         pect = pect*100) %>% select(State,pect) %>%
  mutate(color = case_when(
    pect <= 30 ~ "Less than 30%",
    pect >= 30.01 & pect <= 35 ~ "30%-35%",
    pect >= 35.01 & pect <= 40 ~ "35%-40%",
    pect >= 40.01 ~ "More than 40%",
    TRUE ~ "Others"
  )
)

Total_Employed_Registered_Nurses_2020_2<- Total_Employed_Registered_Nurses_2020%>% left_join(statepop, by= c("State"="full")) %>% na.omit() 

# Puerto Rico, Guam, Virgin Islands are not included

# Font --------------------------------------------------------------------

font_add_google("DM Serif Display")

font_labels <- "DM Serif Display"

font_add_google("Lora")

font_labels2 <- "Lora"

showtext_auto()


# Graph -------------------------------------------------------------------


fnurses_map_2020<-plot_usmap(data = Total_Employed_Registered_Nurses_2020_2, values = "color",labels = TRUE) + 
  scale_fill_manual(values = c( 
                                "30%-35%" = "#e3eddf",
                                "35%-40%"= "#c6dabf",
                                "More than 40%" = "#b8c375")) +
  labs(x = "",y = "",  title = "2020") +
  theme(
    plot.title = element_text(margin = margin(b = 15, t=10), 
                              color = "#000000",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(b = 25), 
                                 color = "#000000", size = 12,hjust = 0.5, family = font_labels),
    legend.position = "bottom",
    legend.title=element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    legend.margin=margin(b = 2, unit='cm'),
    legend.text=element_text(size=8,family = font_labels2),
    legend.background =  element_rect(fill = "#f7f7f7", color = NA),
    legend.key.size = unit(0.3, "cm"),
  ) +  guides(fill = guide_legend(
    label.position = "bottom",
    family = font_labels, 
    color = "#808080",
    keywidth = 3, keyheight = 0.5)) 



# 1998 --------------------------------------------------------------------


Total_Employed_Registered_Nurses_1998<- nurses %>% filter (Year =="1998") %>% mutate(State = str_to_lower(State)) %>% group_by(State)%>%
  mutate(pect = Total_nurses/Total_Employed_Healthcare_State_Aggregate,
         pect = pect*100) %>% select(State,pect) %>%
  mutate(color = case_when(
    pect <= 30 ~ "Less than 30%",
    pect >= 30.01 & pect <= 35 ~ "30%-35%",
    pect >= 35.01 & pect <= 40 ~ "35%-40%",
    pect >= 40.01 ~ "More than 40%",
    TRUE ~ "Others"
  )
  )


Total_Employed_Registered_Nurses_1998_2<- Total_Employed_Registered_Nurses_1998%>% left_join(statepop, by= c("State"="full")) %>% na.omit()

# Puerto Rico, Guam, Virgin Islands are not included

# Graph -------------------------------------------------------------------


fnurses_map_1998<-plot_usmap(data = Total_Employed_Registered_Nurses_1998_2, values = "color",labels = TRUE) + 
  scale_fill_manual(values = c( 
                                "30%-35%" = "#e3eddf",
                                "35%-40%"= "#c6dabf",
                                #"30 to 40"= "#f9cca5",
                                #"7.5 to 10"= "#fec072",
                                "More than 40%" = "#b8c375")) +
  labs(x = "",y = "", title = "1998") +
  theme(
    plot.title = element_text(margin = margin(b = 15, t=10), 
                              color = "#000000",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(b = 25), 
                                 color = "#000000", size = 12,hjust = 0.5, family = font_labels),
    legend.position = "bottom",
    legend.title=element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    legend.margin=margin(b = 2, unit='cm'),
    legend.text=element_text(size=8,family = font_labels2),
    legend.background =  element_rect(fill = "#f7f7f7", color = NA),
    legend.key.size = unit(0.3, "cm"),
  ) +  guides(fill = guide_legend(
    label.position = "bottom",
    family = font_labels, 
    color = "#808080",
    keywidth = 3, keyheight = 0.5)) 




# Arrange -----------------------------------------------------------------


ggarrange(fnurses_map_1998,fnurses_map_2020, ncol=2, nrow=1, common.legend = TRUE, legend="top") +
  theme_ipsum() +
  labs(x = "",y = "",
      title = "Percentage of Employed Registered Nurses of the Total Healthcare Workforce by State",
       caption = "Source: #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)"
      ) +
  theme(
    plot.title = element_text(margin = margin(b = 20), 
                              color = "#343434",face = "bold",size = 16,
                              hjust = 0,
                              family = font_labels),
    plot.caption =  element_text(margin = margin(t = 30, b = 10), 
                                 color = "#808080", size = 10, family = font_labels2,
                                 hjust = 0.5),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),
    legend.background =  element_rect(fill = "#f7f7f7", color = NA))  






