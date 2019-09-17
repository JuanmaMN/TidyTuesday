# Upload the data ---------------------------------------------------------

park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")



# Packages to upload ------------------------------------------------------



library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(scales)
library(tidyverse)
library(streamgraph)
library(viridis)
library(hrbrthemes)
library(plotly)



# First graph - Multipoint ------------------------------------------------



park_visits1<-park_visits%>% 
  filter(parkname %in% c("Gateway","George Washington Memorial Parkway", "Golden Gate", "Lake Mead", "Natchez Trace") &
           year %in% c( "1980", "1995", "2015")) %>%
  select(year,parkname,visitors)%>% 
  spread(year,visitors)


View(park_visits1)


park_visits1$parkname <- factor(park_visits1$parkname, levels = c("Natchez Trace",
                                                                  "George Washington Memorial Parkway",
                                                                  "Gateway",
                                                                  "Lake Mead",
                                                                  "Golden Gate"
))



names(park_visits1)[2]<-"Second"
names(park_visits1)[3]<-"Third"
names(park_visits1)[4]<-"Fourth"

ggplot() +
  
  geom_segment(
    data = gather(park_visits1, measure, val, -parkname) %>% 
      group_by(parkname) %>% 
      top_n(-1) %>% 
      slice(1) %>%
      ungroup(),
    aes(x = 4000000, xend = 20000000, y = parkname, yend = parkname),
    linetype = "blank", size = 0.3, color = "gray80"
  ) +
  
  geom_segment(
    data = gather(park_visits1, measure, val, -parkname) %>% 
      group_by(parkname) %>% 
      summarise(start = range(val)[1], end = range(val)[2]) %>% 
      ungroup(),
    aes(x = start, xend = end, y = parkname, yend = parkname),
    color = "gray80", size = 2
  ) +
  # reshape the data frame & plot the points
  geom_point(
    data = gather(park_visits1, measure, value, -parkname),
    aes(value, parkname, color = measure), 
    size = 4
  )  + 
  geom_text(data = filter(park_visits1, parkname== "Lake Mead"),
            aes(x = Second, y = parkname),
            label = "2000", fontface = "bold",
            color = "#F7BC08",
            vjust = -2) +
  geom_text(data = filter(park_visits1, parkname == "Lake Mead"),
            aes(x = Third, y = parkname),
            label = "2005", fontface = "bold",
            color = "#F7BC08",
            vjust = -2)  +
  geom_text(data = filter(park_visits1, parkname == "Lake Mead"),
            aes(x = Fourth, y = parkname),
            label = "2015", fontface = "bold",
            color = "#F7BC08",
            vjust = -2) +
  theme_ft_rc()+
  labs(
    title = "National Park Visits",
    subtitle = "TidyTuesday 17.9.2019 - Top 5 parks by total number of visitors",
    caption = "Visualization: JuanmaMN (Twitter @Juanma_MN)",
    x = "Number of visitors",
    y = "")   + theme(legend.position="") + theme(legend.title = element_blank()) +
  scale_x_continuous(label = unit_format(unit = "m", scale = 1e-6), breaks=c(5000000,10000000,15000000,20000000))







# area --------------------------------------------------------------------

park_visitArea<-park_visits%>% 
  filter(parkname %in% c("Gateway","George Washington Memorial Parkway", "Golden Gate", "Lake Mead", "Natchez Trace") &
           year %in% c(1950:2016)) %>%
  select(year,parkname,visitors)



park_visitArea$year<-as.numeric(park_visitArea$year)
#park_visitArea$visitors<-comma_format()(park_visitArea$visitors)

?comma_format

p2 <- park_visitArea%>% 
  ggplot(aes(x=year, y=visitors, fill=parkname, 
             text =paste("Park name:", parkname))) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE)  +
  theme_ipsum() +
  theme(legend.position="none")  +
  scale_y_continuous(label = unit_format(unit = "m", scale = 1e-6))+
  scale_x_continuous(breaks=c(1950,1970,1990, 2010, 2016))+
  labs(
    title = "National Park - Top 5 by Number of visitors",
    subtitle = "TidyTuesday 17.9.2019 - Top 5 parks by number of visitors",
    caption = "Visualization: JuanmaMN (Twitter @Juanma_MN)",
    x = "",
    y = "") +
  scale_fill_brewer(palette="YlGnBu")




ggplotly(p2, tooltip=c("text","x", "y"))
