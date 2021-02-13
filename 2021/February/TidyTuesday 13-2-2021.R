
# Upload the data ---------------------------------------------------------

home_owner <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')

# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext)

# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Lora")

font_labels <- "Lora"

showtext_auto()



# Prepare the data --------------------------------------------------------

home_owner_2<-home_owner %>%
  mutate(
    label = ifelse(year %in% c("1976", "1990","2000", "2007", "2010", "2016"), paste0(round(home_owner_pct*100,2), " ", "%"), " ")
  )



# Graph -------------------------------------------------------------------


graph <-home_owner_2 %>%
  ggplot(aes(x = year, y = home_owner_pct, group = race, color = race)) +
  geom_line(size=1.5,linetype = "solid") + 
  geom_point(size=4, shape=21, aes(fill=race))  +
  scale_color_manual(values = c( "White" = "#5c86b3",
                                "Black"      = "#69b3a2",
                                "Hispanic"     = "#e6d492"))  +
  scale_fill_manual(values = c( "White" = "#5c86b3",
                                 "Black"      = "#69b3a2",
                                 "Hispanic"     = "#e6d492")) +
  scale_x_continuous(breaks = c(1976,1990,2000,2010,2016)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),limits = c(0.35,0.75),
                     breaks = c(0.4,0.5,0.6,0.7,0.8)) + 
  geom_text(aes(year,home_owner_pct, label = label), data = home_owner_2, 
            size = 3, vjust = -1.2,family = font_labels,fontface =2) +
  annotate("segment", x = 2007, xend = 2007, y = 0.505, yend = 0.590,
           linetype = "dotted",   color = "#000000")+
  annotate("segment", x = 2007, xend = 2007, y = 0.611, yend = 0.718,
           linetype = "dotted",   color = "#000000")+
  annotate("text", x = 2007, y =0.6,
           hjust = 0.5, color = "#808080", fontface =0,family = font_labels,
           size = 3, label = paste0("2007 \n Global financial crisis starts")) +
  annotate("text", x = 2017, y =0.68,
           hjust = 0.5, color = "#5c86b3", fontface =2,family = font_labels,
           size = 3.5, label = paste0("White")) +
  annotate("text", x = 2017, y =0.416,
           hjust = 0.5, color = "#69b3a2", fontface =2,family = font_labels,
           size = 3.5, label = paste0("Black")) + 
  annotate("text", x = 2017, y = 0.46,
           hjust = 0.5, color = "#e6d492", fontface =2,family = font_labels,
           size = 3.5, label = paste0("Hispanic")) +
  theme_ipsum(grid="Y") + 
  labs(x = "",y = "",
       title = "Home ownership percentage for families - 1976 to 2016",
       subtitle = "Home ownership by race/ethnicity",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") + 
  theme( legend.position = "none",
        plot.title = element_text(margin = margin(t=5, b = 10), 
                                  color = "#22222b",face = "bold",size = 16,
                                  hjust = 0.5,
                                  family = font_labels),
        plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                     color = "#22222b", size = 9, family = font_labels,
                                     hjust = 0.5),
        plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                     color = "#808080", size = 8, family = font_labels,
                                     hjust = 0.95),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x    = element_text(face = "bold",size = 8, color = "#808080",family = font_labels),
        axis.text.y    = element_text(face = "bold",size = 8, color = "#808080",margin = margin(t = 0, r = 20, b = 0, l = 0),family = font_labels),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.minor.y = element_line(linetype = "dotted"),
        plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
        plot.margin = unit(c(1, 2, 2, 1), "cm"),
        axis.ticks = element_blank()) 

            