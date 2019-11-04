# Upload the data ---------------------------------------------------------

commute_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")

# Upload packages ---------------------------------------------------------

library(readxl)
library(tidyverse)
library(readxl)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggalt)
library(scales)
library(hrbrthemes)


View(commute_mode)

# Prepare the data --------------------------------------------------------


data<-commute_mode %>% group_by(state_region,mode)%>%summarize(total=sum(n)) %>% spread(mode,total) %>%
 mutate (diff=round(Walk-Bike,1),
            label=ifelse(diff>0, paste0("+",comma_format()(diff)), paste0(diff))) %>%
  filter(state_region!="NA")






# Ggplot ------------------------------------------------------------------


g<-ggplot(data, aes(x = Walk, xend = Bike, y=reorder(state_region,Walk))) + 
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = "#228b34",
                colour_xend = "#1380A1")+
  labs(
    title = "Bicycling and Walking to Work in the United States: 2008-2012",
    subtitle = "Number of commuters and difference by Region",
    caption = "\n Source:Tidy Tuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)",
    x = "",
    y = "") + theme(legend.position = "bottom",
                    legend.box = "vertical")  + 
  geom_text(data = filter(data, state_region == "Northeast"),
                                                          aes(x = Walk, y = state_region),
                                                          label = "Walk", fontface = "bold",
                                                          size=3,
                                                          color = "#e13d3d",
                                                          vjust = -1.8) +
  geom_text(data = filter(data, state_region == "Northeast"),
            aes(x = Bike, y = state_region),
            label = "Bike", fontface = "bold",
            size=3,
            color = "#e13d3d",
            vjust = -1.8) +
  geom_text(data = filter(data, state_region == "West"),
            aes(x = Walk, y = state_region),
            label = "Walk", fontface = "bold",
            size=3,
            color = "#e13d3d",
            vjust = -1.8) +
  geom_text(data = filter(data, state_region == "West"),
            aes(x = Bike, y = state_region),
            label = "Bike", fontface = "bold",
            size=3,
            color = "#e13d3d",
            vjust = -1.8)+
  geom_text(data = filter(data, state_region == "North Central"),
            aes(x = Walk, y = state_region),
            label = "Walk", fontface = "bold",
            size=3,
            color = "#e13d3d",
            vjust = -1.8) +
  geom_text(data = filter(data, state_region == "North Central"),
            aes(x = Bike, y = state_region),
            label = "Bike", fontface = "bold",
            size=3,
            color = "#e13d3d",
            vjust = -1.8) +
  geom_text(data = filter(data, state_region == "South"),
            aes(x = Walk, y = state_region),
            label = "Walk", fontface = "bold",
            size=3,
            color = "#e13d3d",
            vjust = -1.8) +
  geom_text(data = filter(data, state_region == "South"),
            aes(x = Bike, y = state_region),
            label = "Bike", fontface = "bold",
            size=3,
            color = "#e13d3d",
            vjust = -1.8)
  

g2<-g + 
  geom_rect(aes(xmin=950000, xmax=1150000, ymin=-Inf, ymax=Inf), fill="#d3d3d3") +
  geom_text(aes(label=label, y=state_region, x=1050000), fontface="bold", size=3.5, color="#008000") +
  geom_text(aes(x=1050000, y=4.03, label="Number of commuters"),
            size=3.5, vjust=-3, fontface="bold") +
  scale_x_continuous(breaks = c(100000, 300000, 500000, 700000), limits = c(-1, 1150000), label=comma_format()) + 
  theme_ipsum_rc(grid="XY")


g2 + geom_label(aes(x = 612611, y = 3.7, label = "Northeast: Highest number of commuters who walked to work"), 
           hjust = 0, 
           vjust = 0.5, 
           lineheight = 0.8,
           colour = "#648aed", 
           fill = "#f7f7f7", 
           label.size = NA, 
           family="Helvetica", 
           size = 3) +theme(legend.position = "top",
                            legend.box = "horizontal",
                            plot.background=element_rect(fill="#f7f7f7"))

