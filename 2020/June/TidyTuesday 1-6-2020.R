
# Upload the data ---------------------------------------------------------

marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')



# Upload packages ---------------------------------------------------------

pacman::p_load(dplyr, lubridate, tidyverse, ggplot2, radar, fmsb)



# Prepare the data --------------------------------------------------------

datamarbles <- marbles %>% mutate(time_per_m = avg_time_lap/track_length_m)%>%
  group_by(site) %>%
  summarize(time=mean(time_per_m, na.rm = TRUE)) 



# Add data for radarchart -------------------------------------------------

min <- datamarbles$time %>% min()
max <- datamarbles$time %>% max()
min <- rep(min, times = nrow(datamarbles))
max <- rep(max,times = nrow(datamarbles))
data <- rbind(max,min,datamarbles$time)


rownames(data) <- NULL
names <- datamarbles$site


colnames(data) <- names
data <- data %>% data.frame()




# Radarchart --------------------------------------------------------------

radarmarble2<-radarchart(data, axistype=1,
                         pcol=rgb(0.5,0.2,0.2,0.4), pfcol=rgb(0.3,0.3,0.4,0.4), plwd=1, 
                         vlabels=c("Greenstone \n 2.616", "Hivedrive \n 2.181", 
                                   "Midnight\nBay \n 2.165", "Momotorway\n 	2.017", "O.raceway\n 2.438",
                                   "Razzway\n 2.199","Savage\n Speedway\n 2.445","Short\n Circuit\n 	1.786"),
                         cglcol="grey", cglty=1, axislabcol="grey",
                         caxislabels=c("fastest", "", "", "", "slowest"),
                         calcex=0.8,
                         
                         title=paste("Marble Racing - Average meter time on each site of race"),
                         vlcex=0.6,
                         cex.main = 1.5)