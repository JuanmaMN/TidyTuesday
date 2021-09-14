# #TidyTuesday - Week 38 - Billboard Top 100

# Upload data -------------------------------------------------------------

billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')

# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, scales, ggtext, ggpubr,ggbump,wesanderson,
               grid, gridtext, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)


# Prepare the data --------------------------------------------------------


## Unique songs per year

billboard_song<- billboard %>% mutate(year  = str_sub(week_id, -4, -1))  %>%
  distinct(song, performer, year) %>%
  group_by(year) %>% summarize (unique_song=n())

## Unique artists per year

billboard_performer<- billboard %>% mutate(year  = str_sub(week_id, -4, -1))  %>%
  distinct(performer, year) %>%
  group_by(year) %>% summarize (unique_performers=n())


billboard_performer$unique_performers <- -billboard_performer$unique_performers

## Join

billboard_join <- billboard_song %>% left_join(billboard_performer, by = "year") %>% pivot_longer(names_to = "Number", values_to = "Total", 2:3) %>%
  filter (year >= 1960 & year <= 2020)

billboard_join$year<- as.numeric(billboard_join$year)

# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Quicksand")

font_add_google("Work Sans")

font_labels <- "Quicksand"

font_labels2 <- "Work Sans"

showtext_auto()


# Graph -------------------------------------------------------------------


billboard_join %>%
  ggplot(aes(x = year, y = Total, fill = Number))+
  geom_bar(stat = "identity", position = "identity")+
  scale_x_continuous(expand = c(0, 0), limits = c(1960, 2023.5),breaks = 1960:2020, 
                     labels = c(1960," ", " "," "," "," "," "," "," "," ",
                                1970," "," "," "," "," "," "," "," "," ",
                                1980," "," "," "," "," "," "," "," "," ",
                                1990," "," "," "," "," "," "," "," "," ",
                                2000," "," "," "," "," "," "," "," "," ",
                                2010," "," "," "," "," "," "," "," "," ",
                                2020)) +
  scale_fill_manual(values = c( "unique_song" = "#b9cad4",
                                "unique_performers" = "#c6dabf"),
                    expand = c(.007, .007)) +
  scale_y_continuous(breaks = seq(from = -500, to = 1000, by = 100), limits=c(-500, 1000),
                     labels = c(" ","400"," ", " ", " ", " ",
                                " ", " ", " ", "400"," ", " ", " ", " ", " ", " "))  +
  labs(y = "",
       x = "",
       title = "Top 100 Billboard - Number of songs and artists per year",
       subtitle =  "The Billboard Hot 100 is the music industry standard record chart in the United States for songs, published weekly by Billboard magazine.",
       caption =  "Source:  Data.World - #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")  +
  theme(
    plot.title = element_text(
      color = "#22222b",face = "bold",size = 16,
      hjust = 0,
      family = font_labels),
    plot.subtitle = element_text(margin = margin(t=5, b= 30),
                                 color = "#22222b", size = 12, family = font_labels,
                                 hjust = 0),
    plot.caption =  element_text(margin = margin(t = 50), 
                                 color = "#22222b", size = 10,
                                 hjust = 0.99,
                                 family = font_labels),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.x    = element_text(color = "#525252", family = font_labels, size = 12),  # hjust = 1 is not needed for aligment
    axis.text.y    = element_text(color = "#525252",family = font_labels, size = 12),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
    axis.ticks = element_blank(),
    legend.position = "none") + 
  geom_text(aes(x = 2021.5, y = 400, label = "Number \nof songs",fontface=1), hjust = 0, 
            nudge_x = 0.01, family = font_labels2,size = 3.5, colour = "#b9cad4") + 
  geom_text(aes(x = 2021.5, y = -400, label = "Number \nof artists",fontface=1), hjust = 0, 
            nudge_x = 0.01, family = font_labels2,size = 3.5, colour = "#c6dabf") + 
  geom_segment(data= billboard_join, mapping=aes(x=1960.5,xend=2020.5,y=400,yend=400),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=1960.5,xend=2020.5,y=0,yend=0),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=1960.5,xend=2020.5,y=-400,yend=-400),linetype="dotted",colour = "#525252") 



