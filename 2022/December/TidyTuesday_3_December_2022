# Data sources ------------------------------------------------------------

wcmatches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv')
worldcups <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')

# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)

# Graph -------------------------------------------------------------------

attendance_graph <- ggplot(worldcups, aes(year, attendance)) +
 geom_area(fill= "#a6c5c5") +
  scale_x_continuous(expand = c(0, 0), limits = c(1920, 2030),breaks = 1920:2030) +
  scale_y_continuous(limits = c(-200000, 6000000))  + 
  geom_point(aes(year,attendance), shape = 20, fill="#a6c5c5", color="#808080") +
  annotate("text",x = 1930.5, y = 2000000, fontface = "bold", colour = "#1d3557",
           label = "Uruguay 1930",
           family = font_labels, size = 3, hjust = 0, vjust = 1.2) +
  annotate("text",x = 1930.5, y = 1850000, fontface = "italic", colour = "#1d3557",
           label = "First World Cup tournament held.\n13 teams took part in the tournament and\n434,000 people attended.",
           family = font_labels, size = 3, hjust = 0, vjust = 1.2) +
  geom_segment(aes(x=1930,xend=1930,y=444000,yend=2000000),colour = "#d3d3d3", size = 0.3) +
  annotate("text",x = 1950.5, y = 2300000, fontface = "bold", colour = "#1d3557",
           label = "Brazil 1950", 
           family = font_labels, size = 3, hjust = 0, vjust = 1.2) +
  annotate("text",x = 1950.5, y = 2150000, fontface = "italic", colour = "#1d3557",
           label = "First World Cup tournament \nto ever reach 1 million people.\nAttendance: 1.34 million", 
           family = font_labels, size = 3, hjust = 0, vjust = 1.2) +
  geom_segment(aes(x=1950,xend=1950,y=1337000,yend=2300000),colour = "#d3d3d3", size = 0.3) +
  annotate("text",x = 1981.5, y = 3000000, fontface = "bold", colour = "#1d3557",
           label = "Spain 1982", 
           family = font_labels, size = 3, hjust = 1, vjust = 1.2) +
  annotate("text",x = 1981.5, y = 2850000, fontface = "italic", colour = "#1d3557",
           label = "World Cup expanded to 24 teams,\nthe first expansion since 1934.\n Attendance: 1.86 million", 
           family = font_labels, size = 3, hjust = 1, vjust = 1.2) +
  geom_segment(aes(x=1982,xend=1982,y=1856277,yend=3000000),colour = "#d3d3d3", size = 0.3) +
  annotate("text",x = 1985.5, y = 4000000, fontface = "bold", colour = "#1d3557",
           label = "Mexico 1986", 
           family = font_labels, size = 3, hjust = 1, vjust = 1.2) +
  annotate("text",x = 1985.5, y = 3850000, fontface = "italic", colour = "#1d3557",
           label = "Mexico became the first country \nto host the World Cup for the second time in 1986.\n First time to ever reach 2 million people, 2.4 million", 
           family = font_labels, size = 3, hjust = 1, vjust = 1.2) +
  geom_segment(aes(x=1986,xend=1986,y=2407431,yend=4000000),colour = "#d3d3d3", size = 0.3) +
  annotate("text",x = 1994.5, y = 5500000, fontface = "bold", colour = "#1d3557",
           label = "USA 1994", 
           family = font_labels, size = 3, hjust = 0, vjust = 1.2) +
  annotate("text",x = 1994.5, y = 5350000, fontface = "italic", colour = "#1d3557",
           label = "The 1994 World Cup holds the record \nfor the highest total attendance, at 3.57 million.", 
           family = font_labels, size = 3, hjust = 0, vjust = 1.2) +
  geom_segment(aes(x=1994,xend=1994,y=3568567,yend=5500000),colour = "#d3d3d3", size = 0.3) +
  annotate("text",x = 1998.5, y = 4500000, fontface = "bold", colour = "#1d3557",
           label = "France 1998", 
           family = font_labels, size = 3, hjust = 0, vjust = 1.2) +
  annotate("text",x = 1998.5, y = 4350000, fontface = "italic", colour = "#1d3557",
           label = "World Cup expanded to 32 teams,\nthe second expansion since 1934.\nAttendance: 2.86 million", 
           family = font_labels, size = 3, hjust = 0, vjust = 1.2) +
  geom_segment(aes(x=1998,xend=1998,y=2859234,yend=4500000),colour = "#d3d3d3", size = 0.3) +
  geom_text(aes(x = 2019, y = 1000000, label = "1,000,000",fontface="plain"), hjust = 0, 
          nudge_x = 0.01, family = font_labels,size = 2.5, colour = "#1d3557") + 
  geom_text(aes(x = 2019, y = 2000000, label = "2,000,000",fontface="plain"), hjust = 0, 
            nudge_x = 0.01, family = font_labels,size = 2.5, colour = "#1d3557") + 
  geom_text(aes(x = 2019, y = 3000000, label = "3,000,000",fontface="plain"), hjust = 0, 
            nudge_x = 0.01, family = font_labels,size = 2.5, colour = "#1d3557") + 
  geom_segment(data= worldcups, 
               mapping=aes(x=2018,xend=1963,y=1000000,yend=1000000),linetype="dotted",
               colour = "#d3d3d3")+
  geom_segment(data= worldcups, 
               mapping=aes(x=2018,xend=1983,y=2000000,yend=2000000),linetype="dotted",
               colour = "#d3d3d3") +
  geom_segment(data= worldcups, 
               mapping=aes(x=2018,xend=1992,y=3000000,yend=3000000),linetype="dotted",
               colour = "#d3d3d3") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "#fbfaf6", color = NA), 
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
    panel.border = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank(),
    legend.position = "none",
    strip.background = element_rect(fill = "#fbfaf6"),
    strip.text.x = element_blank()) +
  annotate("text",x = 1930, y = -500, fontface = "plain", colour = "#1d3557",
           label = "Uruguay\n1930",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2)  +
  annotate("text",x = 1934, y = -500, fontface = "plain", colour = "#1d3557",
           label = "Italy\n1934",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 1938, y = -500, fontface = "plain", colour = "#1d3557",
           label = "France\n1938",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 1950, y = -500, fontface = "plain", colour = "#1d3557",
           label = "Brazil\n1950",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 1954, y = -500, fontface = "plain", colour = "#1d3557",
           label = "Switzerland\n1954",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 1958, y = -500, fontface = "plain", colour = "#1d3557",
           label = "Sweden\n1958",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 1962, y = -500, fontface = "plain", colour = "#1d3557",
           label = "Chile\n1962",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 1966, y = -500, fontface = "plain", colour = "#1d3557",
           label = "England\n1966",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 1970, y = -500, fontface = "plain", colour = "#1d3557",
           label = "Mexico\n1970",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 1974, y = -500, fontface = "plain", colour = "#1d3557",
           label = "Germany\n1974",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 1978, y = -500, fontface = "plain", colour = "#1d3557",
           label = "Argentina\n1978",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 1982, y = -500, fontface = "plain", colour = "#1d3557",
           label = "Spain\n1982",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 1986, y = -500, fontface = "plain", colour = "#1d3557",
           label = "Mexico\n1986",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 1990, y = -500, fontface = "plain", colour = "#1d3557",
           label = "Italy\n1990",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 1994, y = -500, fontface = "plain", colour = "#1d3557",
           label = "USA\n1994",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 1998, y = -500, fontface = "plain", colour = "#1d3557",
           label = "France\n1998",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 2002, y = -500, fontface = "plain", colour = "#1d3557",
           label = "Japan\nSouth Korea\n2002",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.11) +
  annotate("text",x = 2006, y = -500, fontface = "plain", colour = "#1d3557",
           label = "Germany\n2006",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 2010, y = -500, fontface = "plain", colour = "#1d3557",
           label = "S.Africa\n2010",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 2014, y = -500, fontface = "plain", colour = "#1d3557",
           label = "Brazil\n2014",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2) +
  annotate("text",x = 2018, y = -500, fontface = "plain", colour = "#1d3557",
           label = "Russia\n2018",
           family = font_labels, size = 2.5, hjust = 0.5, vjust = 1.2)  +
  geom_image(aes(x = 1934, y = 5000000, image = "https://cdn-icons-png.flaticon.com/512/8861/8861317.png"), 
             asp = 2, size = 0.11) +
  annotate("text",x = 1938, y = 5200000, fontface = "bold", colour = "#1d3557",
           label = "FIFA World Cup \nattendance analysis",
           family = font_labels, size = 8.5, hjust = 0) +
  annotate("text",x = 1938, y = 4500000, fontface = "bold", colour = "#1d3557",
           label = "From Uruguay 1930 to Russia 2018",
           family = font_labels, size = 4, hjust = 0)




attendance_graph



