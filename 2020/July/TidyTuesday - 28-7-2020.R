
# Upload packages ---------------------------------------------------------


pacman::p_load(tidyverse,lubridate, tidyverse, ggplot2, hrbrthemes,patchwork,ggimage,ggdraw,cowplot,
               hrbrthemes,scales,ggtext, ggpubr)


# Upload the data ---------------------------------------------------------

penguins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')



# Prepare the data --------------------------------------------------------

penguins2<-penguins%>%filter(year == 2009)%>%group_by(species)%>% summarize(n=n())  %>%
  mutate(fill = case_when(
    species == "Adelie" ~ "#ff6b00",
    species == "Chinstrap" ~ "#c15ccb",
    species == "Gentoo" ~ "#067473"
  ))


# Graph -------------------------------------------------------------------

background <- 
  ggplot(penguins2) +
  theme_void() 


graph2<-penguins2%>% 
  ggplot(aes(fct_reorder(species, -n), n)) +
  geom_label(aes(label = n), 
             fill = "#DCDCD5", 
             color = "#6A6A68", 
             fontface="bold",
             vjust = 1,
             size = 5, 
             alpha = 0.9,
             family = 'Arial') +
  labs(x = "", 
       y = "",
       title = "",
       caption = "Source:TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)\nPicture:Unsplash") +
  ylim(c(0, 100)) +
  theme_minimal()+
  theme(plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10, family = "Arial",
                                 hjust = 0.95),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.title.y = element_text(family = "Arial", size = 8),
    plot.margin = unit(c(0.5, 1, 1, 0.5), "cm"),
    rect = element_rect(fill = NA, color = NA, linetype = 0)
  )



ggdraw(background) +
  draw_image("Penguin.png", hjust = 0, x = -0.315, y = 0.12, height = 0.75) +
  draw_image("Penguin.png", hjust = 0, x = 0.001, y = 0.12, height = 0.64) +
  draw_image("Penguin.png", hjust = 0, x = 0.295, y = -0.195, scale = 0.38) +
  geom_richtext(aes(x = 0.79, y = 0.55), label = "Total number of <br/>**24 Chinstrap penguins** <br/>in 2009", color = "#c15ccb", hjust = 0.5, label.color = NA)  +
  geom_richtext(aes(x = 0.49, y = 0.8), label = "Total number of  <br/>**44 Gentoo penguins** <br/>in 2009", color = "#067473", hjust = 0.5, label.color = NA)  +
  geom_richtext(aes(x = 0.19, y = 0.9), label = "Total number of <br/>**52 Adelie penguins** <br/>in 2009", color = "#ff6b00", hjust = 0.5, label.color = NA) +
  draw_plot(graph2) 
