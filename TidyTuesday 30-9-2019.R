
# Upload the data ---------------------------------------------------------

pizza_jared <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_jared.csv")



# Upload the packages -----------------------------------------------------

library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(lubridate)
library(plotly)
library(scales)
library(tidyverse)
library(viridis)

# Prepare the data for the graph ------------------------------------------

pizza_jared2<-pizza_jared%>%  mutate(date = as_datetime(time),) %>% mutate(Year=format(date,"%Y"),
                                                                           Month=format(date,"%B"))
# geom_area ---------------------------------------------------------------

pizza_jaredarea<-pizza_jared2%>%group_by (Year,  answer)%>% 
  summarise(total=sum(votes))

View(pizza_jaredarea)

str(pizza_jaredarea)

pizza_jaredarea$Year<-as.numeric(pizza_jaredarea$Year)

colnames(pizza_jaredarea)

p2area3 <- pizza_jaredarea%>% 
  ggplot(aes(x=Year, y=total, fill=factor(answer), group=1,
             text =paste("Answer:", answer,
                         "<br>Total Votes:", total))) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE)  +
  theme_ipsum_rc() +
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  scale_y_continuous()+
  scale_x_continuous()+
  labs(
    title = "NY pizza restaurants - TidyTuesday 30.9.2019",
    subtitle = "",
    caption = "Visualization: JuanmaMN (Twitter @Juanma_MN)",
    x = "",
    y = "") +
  scale_fill_brewer(palette="Set3")

ggplotly(p2area3, tooltip=c("text","x"))





 