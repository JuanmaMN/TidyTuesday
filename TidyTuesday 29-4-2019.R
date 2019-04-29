
# Upload data -------------------------------------------------------------

mp_light <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/mp_light.csv")

View(mp_light)


# Group by month ---------------------------------------------------------


library(dplyr)
library(lubridate)

mp_light_2<-mp_light %>% group_by(month=floor_date(date, "month")) %>%
  summarize(amount=sum(light_score))

View(mp_light_2)
library(xts)



# Prepare the data for dygraphs--------------------------------------------

 
mp_light_3<- as.xts(mp_light_2, order.by=as.Date(mp_light_2$month,format="%Y/%m/%d"))



# Dygraphs ----------------------------------------------------------------


library(dygraphs)


dygraph(mp_light_3$amount, main = "#TidyTuesday", xlab= "", ylab = "Number of windows lit at the McCormick Place, Chicago") %>% dyOptions(fillGraph = TRUE, fillAlpha = 0.4, colors = RColorBrewer::brewer.pal(4, "Paired"), axisLineWidth = 1.5, drawGrid = FALSE)%>%dyRangeSelector(height = 20)

