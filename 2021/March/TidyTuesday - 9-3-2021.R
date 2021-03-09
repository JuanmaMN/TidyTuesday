
# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)




# Upload data -------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2021-03-09')
tuesdata <- tidytuesdayR::tt_load(2021, week = 11)

movies<- tuesdata$movies


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Lora")

font_labels <- "Lora"

showtext_auto()


# Prepare the data --------------------------------------------------------

movies_2<-movies%>%
  mutate(budget_2013_tier=case_when(
    budget_2013 < 10000000 ~ "Less_than_10M",
    budget_2013 >= 10000000 & budget_2013 < 25000000~ "Between_10M_and_25M",
    budget_2013 >= 25000000 & budget_2013 < 50000000  ~ "Between_25M_and_50M",
    budget_2013 >= 50000000 & budget_2013 < 100000000  ~ "Between_50M_and_100M",
    budget_2013 >= 100000000 & budget_2013 < 200000000  ~ "Between_100M_and_200M",
    budget_2013 >= 200000000  ~ "More_than_200M",
    TRUE ~ "Others"),
    budget_2013_tier = factor(budget_2013_tier,
                              levels = c("Less_than_10M",
                                         "Between_10M_and_25M",
                                         "Between_25M_and_50M",
                                         "Between_50M_and_100M",
                                         "Between_100M_and_200M",
                                         "More_than_200M")),
    budget_2013_tier=recode(budget_2013_tier,"Less_than_10M" = "Less than 10M", 
                            "Between_10M_and_25M" = "10M to 25M",
                            "Between_25M_and_50M" = "25M to 50M",
                            "Between_50M_and_100M" = "50M to 100M",
                            "Between_100M_and_200M" = "100M to 200M",
                            "More_than_200M" = "> 200M"))%>%
  select(title, imdb_rating,budget_2013,binary,budget_2013_tier) %>%
  group_by(budget_2013_tier)%>% mutate(median=median(imdb_rating,na.rm = T),
                                       mean=mean(imdb_rating,na.rm = T)) %>% na.omit()



# Graph -------------------------------------------------------------------

movies_2%>%ggplot(aes(budget_2013_tier, imdb_rating,colour = binary)) +    geom_quasirandom( size = 2, alpha = 0.6) +
  stat_summary(fun = "median", geom = "point", colour = "#e6d492", size = 8, shape = 18) +
  scale_color_manual(values = c( "FAIL" = "#e33d53",
                                 "PASS" = "#4d926a")) +
  scale_y_continuous(breaks = c(2.5, 5 ,7.5), limits = c(1,10)) + 
  annotate("text", x = 1, y =1.5, size = 6,
           hjust = 0.5, color = "#e6d492", fontface =0,family = font_labels,
           size = 3, label = paste0("Median = 7")) +
  annotate("text", x = 2, y =1.5,size = 6,
           hjust = 0.5, color = "#e6d492", fontface =0,family = font_labels,
           size = 3, label = paste0("Median = 6.9")) +
  annotate("text", x = 3, y =1.5,size = 6,
           hjust = 0.5, color = "#e6d492", fontface =0,family = font_labels,
           size = 3, label = paste0("Median = 6.8")) +
  annotate("text", x = 4, y =1.5,size = 6,
           hjust = 0.5, color = "#e6d492", fontface =0,family = font_labels,
           size = 3, label = paste0("Median = 6.7")) +
  annotate("text", x = 5, y =1.5,size = 6,
           hjust = 0.5, color = "#e6d492", fontface =0,family = font_labels,
           size = 3, label = paste0("Median = 6.8")) +
  annotate("text", x = 6, y =1.5,size = 6,
           hjust = 0.5, color = "#e6d492", fontface =0,family = font_labels,
           size = 3, label = paste0("Median = 6.7")) +
  labs(x = "",y = "",
       title = "Film ratings (IMDB Rating) and budget analysis",
       subtitle = "Analysis of movies that failed and passed the Bechdel test from 1970 to 2013",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")+
  scale_x_discrete(position = "top") +
  guides(fill = NULL) +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#808080",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#808080", size = 9, family = font_labels,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#808080", size = 10, family = font_labels,
                                 hjust = 0.95),
    axis.title.x = element_text(margin = margin(t = 10),family = font_labels,
                                color = "#808080"),
    axis.title.y = element_text(margin = margin(r = 15), family = font_labels,
                                color = "#808080"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text=element_text(color = "#808080",family = font_labels,size=8),
    legend.background = element_rect(fill = "#fbfaf6", color = NA),
    #strip.text.x = element_blank(),
    legend.key.size = unit(0.3, "cm"),
    axis.text.x    = element_text(color = "#808080",family = font_labels,size = 12),
    axis.text.y    = element_text(color = "#808080",family = font_labels,size = 12),
    panel.background = element_blank(), 
    #panel.grid.major = element_blank(),
    panel.grid.major.y = element_line(colour = "#ebebeb"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border
    plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank()
  )  + 
  guides(fill = guide_legend(
    label.position = "bottom",
    family = font_labels, 
    color = "#fbfaf6",
    keywidth = 3, keyheight = 0.1)) 



