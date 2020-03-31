
# Upload packages ---------------------------------------------------------

library(choroplethr)
library(choroplethrMaps)
library(choroplethrZip)
library(mapproj)
library(ggplot2)
library(tidyverse)
library(usmap)
library(scales)

# Data --------------------------------------------------------------------


beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

names(beer_states)[1]<-"abbr"
names(beer_states)[3]<-"value"



# Select the columns needed -----------------------------------------------

data2<- beer_states%>%left_join(statepop, by= "abbr")  %>% select(1:6)

data2<-data2%>% filter(type =="On Premises" & year == "2019" & abbr != "total") %>% group_by(abbr, year, type, fips, full) %>% summarize (value = sum(value))



# Graph -------------------------------------------------------------------


p<-plot_usmap(data = data2, values = "value", labels = TRUE, lines = "white", label_color = "white") + 
  scale_fill_gradient(low = "#add8e6", high = "#e13d3d",
                      breaks= c(100000, 200000, 300000, 400000),
                      labels= c("100mil", "200mil", "300mil", "400mil"), name="Number of \nResponses")+
  labs(x = "",y = "",
       title = "Beer Production by State in 2019 ",
       subtitle = "California - State with the highest number of barrels produced in 2019 - Type: On Premises.",
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
    axis.ticks.x=element_blank()
  ) 
