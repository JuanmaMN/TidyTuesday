
# Upload data -------------------------------------------------------------

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')


# Upload packages ---------------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext,patchwork, 
               hrbrthemes, scales,ggtext, ggpubr)


# Prepare the data --------------------------------------------------------

total_occ<-astronauts%>% mutate(age=(year_of_selection - year_of_birth),
                                age=case_when(
                                  age >= 21 & age <= 30 ~ "21-30",
                                  age >= 31 & age <= 40 ~ "31-40",
                                  age >= 41 & age <= 50 ~ "41-50",
                                  age >= 51 & age <= 60 ~ "51-60"),
                                age=as_factor(age))%>%
  group_by(age,military_civilian) %>%summarize(total=sum(total_number_of_missions)) %>%
  mutate(ftotal=total/sum(total), 
         pcnt=round(ftotal*100, 2),
         label = paste0(pcnt, "%"))



# First graph -------------------------------------------------------------


g<-ggplot(total_occ, aes(x = age, y = pcnt, fill = military_civilian)) +
  #stacked bar
  geom_bar(stat = "identity") +
  geom_text(aes(x =age, y = pcnt, label = label), position = position_stack(vjust = 0.5), color="white",family = "Comfortaa", size = 3.5) +
  #coord_flip() +
  scale_x_discrete(position = "top") +
  theme(
    plot.title = element_text(margin = margin(b = 1), 
                              color = "#22222b",face = "bold",size = 9,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=5,b = 1), 
                                 color = "#22222b", size = 7, family = "Arial",
                                 hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "#f7f7f7", color = NA),
    legend.position = "bottom",
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank(),
    axis.text.x = element_text(family = "Comfortaa", color="#22211d", 
                               size=10,face = "bold")
  ) +
  guides(fill = guide_legend(title = "Species",
                             title.position = "top",
                             title.hjust = 0.5,
                             label.position = "bottom",
                             keywidth = 3, keyheight = 0.5)) +scale_fill_manual(values = c("#a0c4a9", "#e6d492"))


g_first_graph<-g+ 
  annotate(
    geom = "text", x = 1, y = 105, label = "737 total missions", 
    hjust = "middle", colour = "black", size = 3
  )+
  annotate(
    geom = "text", x = 2, y = 105, label = "2,858 total missions", 
    hjust = "middle", colour = "black", size = 3
  ) +
  annotate(
    geom = "text", x = 3, y = 105, label = "201 total missions", 
    hjust = "middle", colour = "black", size = 3
  ) +
  annotate(
    geom = "text", x = 4, y = 105, label = "13 total missions", 
    hjust = "middle", colour = "black", size = 3
  ) 





# Second graph ------------------------------------------------------------


total_nationality<-astronauts%>% mutate(age=(year_of_selection - year_of_birth),
                                        nationality=case_when(
                                          nationality == "U.S." ~ "United States",
                                          nationality == "U.S.S.R/Russia" ~ "U.S.S.R/Russia",
                                          TRUE ~ "Others"))%>%
  
  group_by(nationality,military_civilian) %>%summarize(total=sum(total_number_of_missions))



gnationality<-ggplot(total_nationality, aes(x = nationality, y = total, fill = military_civilian)) +
  #stacked bar
  geom_bar(width= 0.4,stat = "identity",position = 'stack') +
  geom_text(aes(x =nationality, 
                y = total, label = comma_format()(total)), position = position_stack(vjust = 0.5), color="white", family = "Comfortaa", size = 3.5)+
  coord_flip() +
  theme(
    plot.title = element_text(margin = margin(b = 1), 
                              color = "#22222b",face = "bold",size = 9,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=5,b = 1), 
                                 color = "#22222b", size = 7, family = "Arial",
                                 hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x    = element_blank(),
    ##axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "#f7f7f7", color = NA),
    legend.position = "bottom",
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    axis.ticks = element_blank(),
    axis.text.y = element_text(family = "Comfortaa", color="#22211d", 
                               size=10,face = "bold")
  ) +
  guides(fill = guide_legend(title = "Species",
                             title.position = "top",
                             title.hjust = 0.5,
                             label.position = "bottom",
                             keywidth = 3, keyheight = 0.5)) +scale_fill_manual(values = c("#a0c4a9", "#e6d492")) + 
  annotate(
    geom = "text", x = 1, y = 2000, label = "Total number of missions: \n3809", 
    hjust = "middle", colour = "grey", size = 7
  )








# Multiple plots ----------------------------------------------------------

ggarrange(g_first_graph,gnationality, ncol=2, nrow=1, common.legend = TRUE, legend="bottom") +
  theme_ipsum() +
  labs(x = "",y = "",
       title = "Astronaut Database - Analysis of the total number of missions",
       subtitle = "",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 14,
                              hjust = 0.5,
                              family = "Arial"),
    plot.background = element_rect(fill = "#f7f7f7", color = NA))  












