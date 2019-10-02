
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

pizza_jared2<-pizza_jared%>%  mutate(date = as_datetime(time)) %>% mutate(Year=format(date,"%Y"),
                                                                           Month=format(date,"%B"))
# geom_area ---------------------------------------------------------------

pizza_jaredarea<-pizza_jared2%>%group_by (Year,  answer)%>% 
  summarise(total=sum(votes))

pizza_jaredarea$Year<-as.numeric(pizza_jaredarea$Year)


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







# Ridgeline ---------------------------------------------------------------


# Excelent and Good

pizza_jaredREG<-pizza_jared2%>%group_by (Year, Month, answer)%>% filter (answer == "Excellent") %>%
  summarise(total=sum(votes))


pizza_jaredREG$Year<-as.numeric(pizza_jaredREG$Year)


head(pizza_jaredREG)
ggplot(pizza_jaredREG, aes(x=Year,y = reorder(Month,desc(Month)), fill = Month, group = interaction(Month, answer)),width=800, height=700) +
  geom_density_ridges2(scale =1) + 
  theme_ipsum_rc()+
  labs( 
    title = "NY pizza restaurants - Excelent Answer",
    subtitle = "TidyTuesday 2.10.2019",
    caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)",
    x = "",
    y = "")  +
  theme(legend.position="",
        legend.title = element_blank())



 