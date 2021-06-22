
# Upload dataset ----------------------------------------------------------

parks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv')

# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,sf,dplyr,
               grid, gridtext,hrbrthemes,scales,ggtext)


library(fmsb)    # for radar
library(colormap)  # for colour
 


# Prepare the data --------------------------------------------------------

## Select Top 6

parks2<-parks %>% filter (year  ==  2020) %>% select (city, basketball_data,dogpark_data,playground_data, rec_sr_data, restroom_data, splashground_data) %>%
  filter (city %in% c("Minneapolis", "Washington, D.C.", "St. Paul", "Arlington, Virginia", "Cincinnati","Portland")) 
  
# Add extra data for Radar ------------------------------------------------

parks22_basksetball<-parks2%>%select(1,2) %>% spread(city,basketball_data)
parks22_basksetball <- rbind(rep(10,6), rep(0,6),parks22_basksetball)   

parks22_dog<-parks2%>%select(1,3) %>% spread(city,dogpark_data)
parks22_dog <- rbind(rep(10,6), rep(0,6),parks22_dog)

parks22_playground <-parks2%>%select(1,4) %>% spread(city,playground_data)
parks22_playground <- rbind(rep(10,6), rep(0,6),parks22_playground)

parks22_rec <-parks2%>%select(1,5) %>% spread(city,rec_sr_data)
parks22_rec <- rbind(rep(10,6), rep(0,6),parks22_rec)



# Graph -------------------------------------------------------------------

par(mar=c(5.1, 4.1, 4.1, 2.1))

par(mfrow = c(2, 2),bg = '#fbfaf6') # Create a 2 x 2 plotting matrix
mtext("Weibull distribution", line=0, side=1, outer=TRUE, cex=2)

colors_border=colormap(colormap=colormaps$viridis, nshades=6, alpha=1)
colors_in=colormap(colormap=colormaps$viridis, nshades=6, alpha=0.3)


radar<-radarchart(parks22_basksetball, axistype=1 ,
                  #custom polygon
                  pcol=colors_border[1] , pfcol=colors_in[1] , plwd=2, plty=1 , 
                  #custom the grid
                  cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,10,2.5), cglwd=0.8,
                  #custom labels
                  vlcex=0.8,
                  title=paste("Basketball hoops per 10,000 residents"),
                  cex.main = 1.2
) 

radar2<-radarchart(parks22_dog, axistype=1 ,
                  #custom polygon
                  pcol=colors_border[2] , pfcol=colors_in[2] , plwd=2, plty=1 , 
                  #custom the grid
                  cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,10,2.5), cglwd=0.8,
                  #custom labels
                  vlcex=0.8,
                  title=paste("Dog parks per 100,000 residents"),
                  cex.main = 1.2
) 

radar3<-radarchart(parks22_playground, axistype=1 ,
                   #custom polygon
                   pcol=colors_border[3] , pfcol=colors_in[3] , plwd=2, plty=1, 
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,10,2.5), cglwd=0.8,
                   #custom labels
                   vlcex=0.8,
                   title=paste("Playgrounds per 10,000 residents"),
                   cex.main = 1.2
) 


radar4<-radarchart(parks22_rec, axistype=1 ,
                   #custom polygon
                   pcol=colors_border[4] , pfcol=colors_in[4] , plwd=2, plty=1, 
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,10,2.5), cglwd=0.8,
                   #custom labels
                   vlcex=0.8,
                   title=paste("Recreation and senior centers per 20,000 residents"),
                   cex.main = 1.2
) 

