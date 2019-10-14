# Upload the data ---------------------------------------------------------

big_epa_cars <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv")


# Upload packages ---------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggalt)
library(scales)
library(hrbrthemes)


# Prepare the data --------------------------------------------------------


Electric_car<-big_epa_cars %>% filter(fuelType1 == "Electricity") %>% group_by(make)%>% select(make, cityE,highwayE) %>%
  summarise(avg_city_consumption=round(mean(cityE,na.rm=TRUE),1),
            avg_highway_consumption=round(mean(highwayE,na.rm=TRUE),1)) %>% na.omit()




# dumbbell graph ----------------------------------------------------------


ggplot(Electric_car, aes(x = avg_city_consumption, xend = avg_highway_consumption, y=reorder(make,avg_city_consumption))) + 
  geom_dumbbell(colour = "#e5e5e5",
                size = 3,
                colour_x = "#228b34",
                colour_xend = "#1380A1")+
  theme_ipsum_rc()  +
  labs(
    title = "Electric vehicles - Average City VS Highway consumption",
    subtitle = "TidyTuesday 14.10.2019",
    caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)",
    x = "Consumption in kw-hrs/100 miles",
    y = "") + theme(legend.position = "top",
                    legend.box = "horizontal",
                    plot.background=element_rect(fill="#f7f7f7")) +
 geom_text(data = filter(Electric_car, make == "Plymouth"),
            aes(x = avg_highway_consumption, y = make),
            label = "Highway", fontface = "bold",
            color = "#395B74",
            vjust = 4) +
  geom_text(data = filter(Electric_car, make == "Plymouth"),
            aes(x = avg_city_consumption, y = make),
            label = "City", fontface = "bold",
            color = "#228b34",
            vjust = 4)


