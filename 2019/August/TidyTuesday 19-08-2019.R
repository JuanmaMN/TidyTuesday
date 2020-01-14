# Upload the dataset ------------------------------------------------------

nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")


# Upload the necessary packages -------------------------------------------

library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(dplyr)
library(plotly)
library(scales)

#change to decade to make the "group_by" functiom better.

nuclear_explosions$year[nuclear_explosions$year%in% c(1940:1949)] <- "1940-1949"
nuclear_explosions$year[nuclear_explosions$year%in% c(1950:1959)] <- "1950-1959"
nuclear_explosions$year[nuclear_explosions$year%in% c(1960:1969)] <- "1960-1969"
nuclear_explosions$year[nuclear_explosions$year%in% c(1970:1979)] <- "1970-1979"
nuclear_explosions$year[nuclear_explosions$year%in% c(1980:1989)] <- "1980-1989"
nuclear_explosions$year[nuclear_explosions$year%in% c(1990:1999)] <- "1990-1999"
View(nuclear_explosions)



test2<-nuclear_explosions%>%group_by(country,year)%>%
  summarize(total=n())



# Plotly ------------------------------------------------------------------


p <- plot_ly(test2, x = ~year, y = ~total, type = 'bar', color = ~country, name = ~country,
             text =  ~paste('</br> Country: ', country,
                            '</br> Decade: ', year,
                            '</br> Nuclear Explosions: ', comma_format()(total)),
             hoverinfo = "text") %>%
  layout(yaxis = list(title = ''), 
         xaxis = list(title = ''),
         barmode = 'stack')%>%
  layout(title = 
           list(
             text = "Nuclear Explosions by decade", 
             xanchor = "middle",
             font = list(
               family = "times New Roman", 
               color = "#1E86FF", 
               size = 20
             )
           )
  ) 

p%>% layout(annotations = list(
  list(x = 1, xanchor = "right", y = 800, showarrow = F, ax = 0, ay = 1, align = "down",
       text = "TidyTuesday 19.8.2019
       Visualization: JuanmaMN 
       (Twitter @Juanma_MN)")))



