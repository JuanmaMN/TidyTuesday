# Upload packages ---------------------------------------------------------

library(tidyverse)
library(choroplethr)
library(choroplethrMaps)
library(choroplethrZip)
library(mapproj)
library(ggplot2)
library(tidyverse)
library(usmap)


# Upload data -------------------------------------------------------------

measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

View(measles)

# Prepare the data --------------------------------------------------------

data<-measles %>%select(2,9,10, 15,16) %>%filter(mmr != -1) %>% na.omit() %>% mutate (total_vaccination = round(enroll * mmr/100),0) %>% select(1:6)


datab<-data%>%group_by(state)%>%summarize(total_vaccination=sum(total_vaccination),
                                          lat =  mean(c(max(lat), min(lat))),
                                          long = mean(c(max(lng), min(lng))))




names(datab)[1]<-"full"
names(datab)[2]<-"value"


datac<- datab%>% inner_join(statepop, by= "full")  %>% select(1,2,5,6)




# Graph -------------------------------------------------------------------



plot_usmap(data = datac, values = "value", labels = TRUE, lines = "white", label_color = "white") + 
  scale_fill_gradient(low = "#add8e6", high = "#e13d3d") +
  labs(x = "",y = "",
       title = "Total number of vaccinations",
       subtitle = "Total number of vaccinations - School's Measles, Mumps, and Rubella (MMR) -  in a given state.",
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
    plot.background = element_rect(fill = "#f7f7f7")
  ) 
