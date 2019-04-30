# Upload data -------------------------------------------------------------

mp_light <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/mp_light.csv")

View(mp_light)


# Group by month ---------------------------------------------------------


library(dplyr)
library(lubridate)

mp_light_2<-mp_light %>% group_by(month=floor_date(date, "month")) %>%
  summarize(amount=sum(light_score)) 

mp_light_3_plotly <- mp_light_2%>% mutate(month=format(mp_light_2$month,"%Y-%m"))




# Plotly ------------------------------------------------------------------


View(mp_light_3)

library(plotly)

p <- plot_ly(mp_light_3_plotly, x = ~month, y = ~amount, 
             type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 2),
             text =  ~paste('</br> Light score: ', amount,
                            '</br> Month: ', month), 
             marker = list(color = 'rgb(166,206,227)',
                           line = list(color = 'rgb(8,48,107)',
                                       width = 1))) %>%
  layout(xaxis = list(title = "month"),
         yaxis = list(title = "amount"),
         title = 'Number of windows lit at the McCormick Place, Chicago',
         annotations = 
           list(text = "#TidyTuesday @Juanma_MN", 
                showarrow = F, xref='paper', yref='paper', 
                xref = 'paper', x = 1,
                yref = 'paper', y = 0,
                font=list(size=10, color="black"))
  ) 

p
