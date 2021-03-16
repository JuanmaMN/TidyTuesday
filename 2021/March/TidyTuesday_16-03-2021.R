
# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, scales, ggtext, ggpubr,ggbump,wesanderson,
               grid, gridtext, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)


# Upload data set ---------------------------------------------------------


games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')


# Prepare the data --------------------------------------------------------

games_top_10_2020<-games%>% filter(year == "2020") %>%group_by(year,gamename)%>%summarise(avg_2=mean(avg,na.rm=TRUE))

games_top_10_2020<-games_top_10_2020%>%top_n(10,avg_2)

top_10<-games_top_10_2020%>%ungroup()%>%select(2)

games_bump<-games%>% filter(year == "2020") %>% inner_join(top_10, by = "gamename") %>% select(gamename,month,avg)


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Lora")

font_labels <- "Lora"

showtext_auto()


# Graph -------------------------------------------------------------------


games_bumprank<-games_bump %>%
  group_by(month) %>%
  mutate(rank = min_rank(-avg) * 1) %>%
  ungroup()%>%
  mutate(month = case_when(
    month == "January" ~ 1,
    month == "February" ~2,
    month == "March" ~3,
    month == "April"~4,
    month == "May"~5,
    month == "June" ~6,
    month == "July" ~7,
    month == "August" ~8,
    month == "September" ~9,
    month == "October" ~10,
    month == "November" ~11,
    month == "December" ~12
  )) 



# Colours -----------------------------------------------------------------



wp_colours<-c("#ff4141", "#e6d492", "#86a7ae", "#b576af", "#e39e65","#80a8b0", "#ae633b","#b8c375","#548188","#36606f") 



# Graph -------------------------------------------------------------------

games_graph<-ggplot(games_bumprank, aes(month, rank, color = gamename)) +
  geom_point(size = 7) +
  geom_text(data = games_bumprank %>% filter(month == min(month)),
            aes(x = month, label = gamename), size = 4.5, hjust = 1,nudge_x = -0.2,family = font_labels,) +
  geom_text(data = games_bumprank %>% filter(month == max(month)),
            aes(x = month, label = gamename), size = 4.5, hjust = 0,nudge_x = 0.5,family = font_labels) +
  geom_bump(aes(smooth = 6), size = 1.5) +
  scale_color_manual(values = wp_colours) +
  scale_y_reverse(limits = c(10, 1),
                  breaks = seq(1, 10, 1)) +
  scale_x_continuous(limits = c(-2, 15), breaks = seq(1, 12, 1)) +
  labs(y = "",
       x = "",
       title = "",
      subtitle =  "",
       caption =  "")  +
  guides(fill = NULL) +
  theme(
    plot.title = element_text(margin = margin(b = 10, t= 10), 
                              color = "#e6d492",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10,
                                 hjust = 0.95,
                                 family = font_labels),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    legend.position = "none",
    axis.text.x    = element_text(color = "#22222b",family = font_labels, size = 10),
    axis.text.y    = element_blank(), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
    axis.ticks = element_blank()
  )  +
  annotate("text", family = font_labels, x = 1, y = 9.5,  hjust = 0, size = 3.5, color = "#808080",
           label = "Top 10 games with the highest average number of players at the same time in 2020")



ggarrange(games_graph)+
  theme_ipsum() +
  labs(x = " ",y = "",
       title = "Which games were played the most in each month of 2020?",
       subtitle = "",
       caption = "Source: Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 2), 
                              color = "#e6d492",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10, family = font_labels,
                                 hjust = 0.95),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    plot.background = element_rect(fill = "#fbfaf6", color = NA))  
