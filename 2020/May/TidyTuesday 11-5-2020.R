
# Upload the packages -----------------------------------------------------

library(tidyverse)
library(ggthemes)
library(countrycode)
library(viridis)
library(rayshader)


# Upload the dataset ------------------------------------------------------

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')


# Prepare the data --------------------------------------------------------

volcano2<-volcano%>% group_by(country) %>%
  summarize (avg_elevation=mean(elevation, na.rm=TRUE),
             total=n())

volcano2$country <- recode(volcano2$country , "United States" = "USA",
                           "United Kingdom" = "UK",
                           "DR Congo" = "Democratic Republic of the Congo",
                           "El Salvador-Guatemala"="El Salvador",
                           "Eritrea-Djibouti"="Eritrea",
                           "Ethiopia-Djibouti"="Ethiopia",
                           "Ethiopia-Kenya"="Ethiopia",
                           "Guatemala-El Salvador"="Guatemala",
                           "Mexico-Guatemala"="Mexico",
                           "North Korea-South Korea"="North Korea",
                           "Saint Kitts and Nevis"="Saint Kitts",
                           "Saint Vincent and the Grenadines"="Saint Vincent",
                           "Syria-Jordan-Saudi Arabia"="Syria",
                           "Uganda-Rwanda"="Uganda",
                           "DR Congo-Rwanda"="Democratic Republic of the Congo",
                           "Burma (Myanmar)"= "Myanmar",
                           "Japan - administered by Russia" = "Japan",
                           "Armenia-Azerbaijan"= "Armenia",
                           "Chile-Argentina"="Chile",
                           "Chile-Bolivia"="Chile",
                           "Chile-Peru"="Chile",
                           "China-North Korea"="China",
                           "Colombia-Ecuador"="Colombia")

world <- map_data("world")



# Join the data -----------------------------------------------------------

databerup <- volcano2 %>% left_join(world, by = c("country"= "region")) 


databerup_DF <- databerup[is.na(databerup$long),]  #see what's mmissing


# Graph -------------------------------------------------------------------

pA <- ggplot() + 
  geom_map(data = world, map = world,
           aes(long, lat, group = group,  map_id = region),
           fill = "#282828", color = "#282828") +
  geom_map(data = databerup, map = world,
           aes(fill = total, map_id = country),
           color = "#282828", size = 0.15, alpha = .8)  +
  
  scale_y_continuous(breaks=c()) +   scale_x_continuous(breaks=c()) +
  labs(x = "",y = "",
       title = "Volcano Eruptions - Number of volcanoes per country",
       subtitle = "",
       caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)")+
  scale_x_discrete(position = "top") +
  guides(fill = NULL) +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#ffffff",face = "bold",size = 10,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#ffffff", size = 8, family = "Arial",
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#ffffff", size = 7, family = "Arial",
                                 hjust = 0.95),
    axis.title.x = element_text(margin = margin(t = 10),
                                color = "#ffffff"),
    axis.title.y = element_text(margin = margin(r = 15), 
                                color = "#ffffff"),
    legend.background = element_rect(fill="#323232",
                                     size=0.5, 
                                     #linetype="solid", 
                                     colour ="#323232"),
    legend.text=element_text(color="#ffffff",size=7),
    legend.title = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#323232"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  ) 


pA  +
  annotate(geom = "text", 
           x = 155, y = 32, size=2.5,
           label = "Japan \nThird highest  with 92", 
           hjust = "center",
           family = "Arial",
           color="white") +
  annotate(geom = "text", 
           x = 85, y = -2, size=2.5,
           label = "Indonesia \nSecond highest  with 95", 
           hjust = "center",
           family = "Arial",
           color="white") +
  annotate(geom = "text", 
           x = -140, y = 34, size=2.5,
           label = "United States	\nHighest with 99", 
           hjust = "center",
           family = "Arial",
           color="white")
