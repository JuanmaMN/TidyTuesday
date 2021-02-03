
# Upload dataset ----------------------------------------------------------

hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv')

View(hbcu_all)

# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext)

# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Lora")

font_labels <- "Lora"

showtext_auto()


# First graph -------------------------------------------------------------

hbcu_allmg<- hbcu_all %>%  select(1,3,4)  %>%
  mutate (Females = -Females) %>%
  pivot_longer(2:3,
               names_to = "type",
               values_to = "enrolment"
  ) %>% filter (Year >=2000)



graph_1<-hbcu_allmg %>% 
  ggplot(aes(x = Year, color = type))+
  geom_linerange(data = hbcu_allmg %>% filter(type == "Males"), aes(ymin = -0.8, ymax = -0.8 + enrolment), size = 7) +
  geom_linerange(data = hbcu_allmg %>% filter(type == "Females"), aes(ymin = 0.8, ymax = 0.8 + enrolment), size = 7) + 
  geom_label(aes(x = Year, y = 0, label = Year), family = font_labels, size = 2.5, label.padding = unit(1, "lines"), 
             label.size = 0, label.r = unit(0.25, "lines"), fill = "#f7f7f7", color = "#525252") + 
  coord_flip() +
  scale_y_continuous(breaks = c(-150000,-100000,-50000, 50000, 100000,150000),
                     label = c("150,000","100,000","50,000", "50,000", "100,000","150,000")) +
  scale_color_manual(values = c("#a0c4a9", "#e6d492")) +
  scale_x_discrete(position = "bottom") +
  guides(fill = NULL) +
  theme(axis.title.x =  element_blank(), 
    axis.title.y =  element_blank(), 
    legend.position = "none",
    axis.text.x    = element_text(color = "#525252",size = 6.5, family = font_labels),
    axis.text.y    = element_text(color = "#525252",size = 6.5, family = font_labels),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank()
  ) 



# Second graph ------------------------------------------------------------

hbcu_allmg_2015<- hbcu_all %>%  select(1,3,4) %>% filter (Year == 2015) %>%
  pivot_longer(2:3,
               names_to = "type",
               values_to = "enrolment"
  ) 


hbcu_allmg_2000<- hbcu_all %>%  select(1,3,4) %>% filter (Year == 2000) %>%
  pivot_longer(2:3,
               names_to = "type",
               values_to = "enrolment"
  ) 


names(hbcu_allmg_2000)[3]<-"enrolment_2020"

hbcu_allmg_2015_2<-hbcu_allmg_2015%>%left_join(hbcu_allmg_2000, by ="type") %>% mutate(enrolment = comma(enrolment),
                                                                                       enrolment_2020 = comma(enrolment_2020),
                                                                                       label = paste0("Enrollment of", " ", type, " ", "-", " ",enrolment, " " , "in", " ", Year.x, ".", " ", "Increase from", " ", enrolment_2020, " ", "in"," ",Year.y),
                                                                                       x_axis_grand = c(17.49, 17.49),
                                                                                       x_axis_point =  c(17.485,17.485),
                                                                                       x_axis_grand_2 = x_axis_grand,
                                                                                       y_axis = c(0.3,0.33),
                                                                                       y_axis_2 = c(0.3,0.3)
)

img = c("HBCU-bg.png") # image in the directory


graph_2<-hbcu_allmg_2015_2%>% 
  ggplot() +
  geom_text(aes(x = x_axis_grand,   y = y_axis,label = label),size = 4, face = "bold",family = font_labels,hjust = 0, color="#525252", size=8) +
  geom_image(aes(x = 17.51, y = 0.22, image = img), size = .5) +
  geom_point(aes(x = x_axis_point, y = y_axis, colour= type), hjust = -2, size = 6) +
  scale_colour_manual(values = c( "Females" = "#a0c4a9","Males" = "#e6d492"))+
  scale_x_continuous(limits = c(17.48,17.55)) +
  scale_y_continuous(limits = c(0.1,0.4)) +
  theme_ipsum()  +
  theme(legend.position = "none",
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



# Both plots --------------------------------------------------------------


ggarrange(graph_1,graph_2, ncol=2, nrow=1, common.legend = FALSE)+
  theme_ipsum() +
  labs(x = "",y = "",
       title = "College Enrollment",
       subtitle = "",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(t=20,b = 8), 
                              color = "#3b9ab2",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                 color = "#525252", size = 8, family = font_labels,
                                 hjust = 0.95),
    plot.background = element_rect(fill = "#f7f7f7", color = NA))  

