
# Upload data -------------------------------------------------------------

nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')



# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)



# Prepare the data --------------------------------------------------------


top_10_books<- nyt_full%>% filter(rank == 1) %>% 
  mutate(label = paste0(title, " ", "by", " ", author))%>%
  group_by(label)%>%summarize(n=n())%>%top_n(10) %>% arrange(desc(n)) %>%
  mutate(position=1:10,
         x_axis=rep(1:2, c(5,5)),
         y_axis = rep(-7:-11, length = n()),
         x_axis_2 = x_axis +0.7,
         y_axis_2 = y_axis-0.2)


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Playfair Display")

font_labels <- "Playfair Display"

showtext_auto()



# Graph -------------------------------------------------------------------


graph_NYbest<-top_10_books %>%ggplot() +
  geom_text(aes(x = x_axis, y = y_axis, label = label), hjust = 0, 
            color = "#22222b",  size = 4.5, 
            family = font_labels) +
  scale_x_continuous(limits = c(0.9,3)) +
  scale_y_continuous(limits = c(-11.5,-6))  +
  labs(y = "",
       x = "",
       title = "Top 10 NY Times bestsellers of all time by number of weeks at No. 1",
       caption =  "Source: Post45 Data for #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(      plot.title = element_text(#margin = margin(b = 10), 
    color = "#000000",face = "bold",size = 20,
    hjust = 0.45,
    family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#808080", size = 12, family = font_labels,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 50), 
                                 color = "#22222b", size = 10,
                                 hjust = 0.45,
                                 family = font_labels),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
    axis.ticks = element_blank(),
    legend.position = "none") +
  geom_text(aes(x = x_axis_2, y = -6.5, label= "Weeks at No. 1"), hjust = 0.5, 
            fontface = "italic",  color = "#808080", size = 4, family = font_labels, alpha=0.4) +
  geom_text(aes(x =  x_axis_2, y = y_axis, label = n), hjust = 0, 
            color = "#22222b",  size = 5, 
            family = font_labels)



graph_NYbest