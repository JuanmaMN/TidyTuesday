
# Load the packages -------------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext,htmltools,reactable,patchwork, choroplethr,choroplethrMaps,
               choroplethrZip,mapproj,hrbrthemes, usmap)

# Upload the datasets -----------------------------------------------------

blackpast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')


# Prepare the data --------------------------------------------------------

names(blackpast)[5]<-"full"

unique(blackpast$subject)

datablackpast<- blackpast%>%left_join(statepop, by= "full")  

datablackpast<-datablackpast%>%  group_by(abbr, fips, full) %>% summarize (n= n())




# US Map ------------------------------------------------------------------


pdatablackpast<-plot_usmap(data = datablackpast, values = "n", labels = TRUE, lines = "white", label_color = "black") + 
  scale_fill_gradient(low = "#ade6d8", high = "#6489d8",
                      breaks= c(50, 100, 150, 200),
                      labels= c( "50", "100", "150", "200"), name="Number of \nEvents")+
  labs(x = "",y = "",
       title = "BlackPast - Number of events per state",
       subtitle = "District of Columbia with 167 events followed by New York with 161 events",
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

