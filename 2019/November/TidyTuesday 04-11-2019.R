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

?comma_format


data<-commute_mode %>% group_by(state_region,mode)%>%summarize(total=sum(n)) %>% spread(mode,total) %>%
 mutate (diff=round(Walk-Bike,1),
            label=ifelse(diff>0, paste0("+",comma_format()(diff)), paste0(diff))) %>%
  filter(state_region!="NA")



%>%
  mutate(Bike = comma_format()(Bike),
         Walk = comma_format()(Walk),
         diff=comma_format()(diff))



View(data)
         
g<-ggplot(data, aes(x = Walk, xend = Bike, y=reorder(state_region,Walk))) + 
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1")+
  labs(x=NULL, y=NULL, title="Modes Less Traveled - Bicycling and Walking to Work in the United States: 2008-2012")  +
  labs(
    title = "Modes Less Traveled - Bicycling and Walking to Work in the United States: 2008-2012",
    subtitle = "Modes Less Traveled - Bicycling and Walking to Work in the United States: 2008-2012",
    caption = "\n Source:Tidy Tuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)",
    x = "",
    y = "") + theme(legend.position = "bottom",
                    legend.box = "vertical")  + geom_text(data = filter(data, state_region == "Northeast"),
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
  scale_x_continuous(label=comma_format())

g2<-g + 
  geom_rect(aes(xmin=900000, xmax=1100000, ymin=-Inf, ymax=Inf), fill="grey80") +
  geom_text(aes(label=label, y=state_region, x=980000), fontface="bold", size=4) +
  geom_text(aes(x=990000, y=4, label="Difference"),
            color="grey20", size=4, vjust=-3, fontface="bold") +
  scale_x_continuous(breaks = c(0:800000), limits = c(-1, 1100000))   + 
  theme_ipsum()


g2

