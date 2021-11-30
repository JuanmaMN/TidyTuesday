
# Upload the data ---------------------------------------------------------

matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts, showtext,ggflags)

# Font --------------------------------------------------------------------

font_add_google("DM Serif Display")

font_labels <- "DM Serif Display"

font_add_google("Bodoni Moda")

font_labels2 <- "Bodoni Moda"

font_add_google("Roboto")

font_labels3 <- "Roboto"

showtext_auto()


# Prepare the data --------------------------------------------------------

matches_ICC <-  matches %>%
  filter (series == "ICC World Cup") 

View(matches_ICC)

matches_3 <-  matches %>%
  filter (series == "ICC World Cup") %>%
  mutate(category = case_when(
    winner == toss & toss_decision == "bat first" ~ "Winner of the toss and bat first",
    winner == toss & toss_decision != "bat first" ~ "Winner of the toss and field first",
    winner != toss & toss_decision == "bat first" ~ "Non-winner of the toss and field first",
    winner != toss & toss_decision != "bat first" ~ "Non-winner of the toss and bat first",
    TRUE ~ "Others")) %>% select(winner, toss, toss_decision, category) %>% group_by(category) %>% 
  summarize(total=n()) %>%  mutate (ftotal=total/sum(total),
                                    pcnt=round(ftotal*100, 1), 
                                    yaxismax=cumsum(ftotal), 
                                    yaxismin = c(0, head(yaxismax, n=-1)),
                                    label_position = (yaxismax+yaxismin)/2)



# First graph -------------------------------------------------------------


graph_match_2<-ggplot(matches_3, aes(ymax=yaxismax, ymin=yaxismin, xmax=4, xmin=3, fill= factor(category))) +
  geom_rect(show.legend=T, alpha=0.8) + 
  coord_polar(theta="y") +
  xlim(c(1, 5))+ 
  theme_void()+
  expand_limits(x = 0, y = 0) +
  scale_fill_manual(values = c( "Winner of the toss and bat first" = "#38818c",
                                "Winner of the toss and field first" = "#c8a774",
                                "Non-winner of the toss and field first"= "#aaa4b0",
                                "Non-winner of the toss and bat first"= "#a0c4a9")) +
  geom_text(x=3.5, aes(y=label_position, label=paste(pcnt,"%",sep="")), size=3, family = font_labels3, color="black")  + 
  theme(legend.title = element_blank(),
        legend.position = "none",
        legend.margin=margin(b = 2, unit='cm'),
        legend.text=element_text(size=8),
        strip.background = element_rect(fill = "#f4efe1"),
        strip.text.x = element_blank(),
        legend.key.size = unit(0.3, "cm"))  


# Second graph ------------------------------------------------------------


test_matches <- matches_3 %>%
  mutate(x_axis = rep(4.58, each = 2, length = n()),
         y_axis = c(-7,-8,-7,-8),
         x_axis_2 = rep(4.53, each = 2, length = n()), 
         y_axis_2 = y_axis,
         facet  = c(2,2,1,1))



graph_test_matches_2<-test_matches %>%ggplot(fill= factor(category)) +
  geom_text(aes(x = x_axis, y = y_axis, label = category, hjust = 0, 
            color = factor(category),  size = 3.2, 
            family = font_labels)) +
  geom_point(aes(x = x_axis_2, y = y_axis_2, colour = factor(category)), hjust = -2, size = 8, alpha=0.6) +
  scale_colour_manual(values = c( "Winner of the toss and bat first" = "#38818c",
                                "Winner of the toss and field first" = "#c8a774",
                                "Non-winner of the toss and field first"= "#aaa4b0",
                                "Non-winner of the toss and bat first"= "#a0c4a9"))  +
  geom_text(x=3.5, aes(y=label_position, label=paste(pcnt,"%",sep="")), size=3, col="black",family = font_labels)  + 
  scale_x_continuous(limits = c(4.5,5.5)) +
  scale_y_continuous(limits = c(-9.5,-5.8)) +
  facet_wrap(vars(facet)) +
  theme(plot.title = element_text(margin = margin(t=10, b = 10), 
                                  color = "#525252", size = 10, family = font_labels,
                                  face = "bold",
                                  hjust = 0.6),
        legend.position = "none",
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
        plot.margin = unit(c(1, 2, 2, 1), "cm"),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank()
  ) 




# Both graphs -------------------------------------------------------------


ggarrange(graph_match_2,graph_test_matches_2, ncol=2, nrow=1, common.legend = FALSE)+
  theme_ipsum() +
  labs(x = "",y = "",
       title = "Cricket - ICC World Cup analysis - 1999 & 2003",
       subtitle = "Percentage of wins by toss win/loss and toss decision",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(t=20,b = 8), 
                              color = "#767676",face = "bold",size = 25,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10, b = 25), 
                                 color = "#767676", size = 15, family = font_labels,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                 color = "#808080", size = 8, family = font_labels,
                                 hjust = 0.95),
    plot.background = element_rect(fill = "#f7f7f7", color = NA))  
