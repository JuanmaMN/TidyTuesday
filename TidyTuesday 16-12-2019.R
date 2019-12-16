# Upload data -------------------------------------------------------------

dog_moves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')


# install and load packages -----------------------------------------------

install.packages("choroplethr")
install.packages("choroplethrMaps")
install_github('arilamstein/choroplethrZip@v1.4.0')
install.packages("mapproj")

library(choroplethr)
library(choroplethrMaps)
library(choroplethrZip)
library(mapproj)
library(ggplot2)
library(tidyverse)
library(usmap)

data<-dog_moves%>%select(1,4) %>% na.omit()


names(data)[1] <- "full"
names(data)[2] <- "value"



# Select the columns needed -----------------------------------------------

data2<- data%>% inner_join(statepop, by= "full") %>% select(1:4)

View(data2)

# Join with statepop for two additional columns (fips and abbr) -----------

plot_usmap(data = data2,, values = "value", labels = TRUE, lines = "white", label_color = "white") + 
  #scale_fill_continuous(name = "data2") +
  scale_fill_gradient(low = "#add8e6", high = "#e13d3d")+
  #theme_void() +
  labs(x = "",y = "",
       title = "Adoptable dogs",
       subtitle = "Total number of adoptable dogs availabe in a given state.",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)",
       x = "",
       y = "") +
  theme(
    plot.title = element_text(margin = margin(b = 10), 
                              color = "#22222b",face = "bold",size = 17,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(b = 25), 
                                 color = "#22222b", size = 12, family = "Arial"),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10, family = "Arial"),
    legend.position = "right",
    legend.title=element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    #plot.background = element_rect(fill = "#f7f7f7")
  ) 


