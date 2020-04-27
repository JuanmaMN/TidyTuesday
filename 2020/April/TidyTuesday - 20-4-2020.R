# Packages ----------------------------------------------------------------

library(tidyverse)
library(maps)
library(ggplot2)
library(viridis)

# Upload the data ---------------------------------------------------------

gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')

View(gdpr_violations)
# Prepare the data --------------------------------------------------------

gdpr_violations2<-gdpr_violations%>%group_by(name)%>% summarise(n=n(),
                                                                fine=sum(price))
names(gdpr_violations2)[1]<-"region"

View(gdpr_violations2)

# World Data --------------------------------------------------------------

world <- map_data("world")

map <- inner_join(world, gdpr_violations2, by = "region")

View(map)
europe <- worldmap + coord_fixed(xlim = c(-9, 42.5),
                                 ylim = c(36, 70.1),
                                 ratio = 1.5)


# Graph -------------------------------------------------------------------

europe2 <- europe + geom_polygon(data = map,
                                 aes(fill = fine,
                                     x = long,
                                     y = lat,
                                     group = group),
                                 color = "grey70") +
  labs(title = "GDPR Fines",
       subtitle = "Total amount paid in fines in euros (???) - Country where violation was enforced",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 10), 
                              color = "#22222b",face = "bold",size = 14,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#22222b", size = 9, family = "Arial",
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10, family = "Arial",
                                 hjust = 0.95),
    axis.text.x  = element_blank(),
    axis.text.y    = element_blank(),
    legend.title = element_text(""),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    text = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()) + 
  scale_fill_gradient(low = "#add8e6", high = "#e13d3d",
                      breaks= c(100000000,
                                75000000,
                                50000000,
                                25000000,
                                0),
                      labels= c("100M ???", "75M ???", "50M ???", "25M ???", "0M ???")) +
  theme(legend.background = element_rect(fill = "#f7f7f7"),
        legend.title = element_blank())


europe2
