# Upload the necessary packages -------------------------------------------

library(readr)
library(plotly)
library(scales)



# Upload and prepare the data ---------------------------------------------------------


corbyn <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/corbyn.csv")

corbyn
corbyn_2<-corbyn%>%mutate(percentage=avg_facebook_likes/sum(avg_facebook_likes))


# Plotly graph ------------------------------------------------------------


p <- plot_ly(corbyn_2, labels = ~political_group, values = ~avg_facebook_likes, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+value') %>%
  layout(title = 'Political identity or group - Average number of facebook likes per Facebook post in 2016',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         annotations = 
           list(text = "#TidyTuesday.  ", 
                showarrow = F, xref='paper', yref='paper', 
                xref = 'paper', x = 0,
                yref = 'paper', y = 1,
                font=list(size=10, color="black")))

p