
# Upload the data ---------------------------------------------------------

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
View(nobel_winners)


# Upload the necessary packages -------------------------------------------

library(dplyr)
library(plotly)


# Prepare the data for plotly ---------------------------------------------

test<-nobel_winners%>%mutate(birth_year= format(birth_date,'%Y'))
View(test)

test_2<-test %>% mutate(winning_age=(as.numeric(prize_year)-as.numeric(birth_year))) %>% select(full_name,winning_age)

View(test_2)
test_3<-test_2 %>%   mutate(
    decade=case_when(
      test_2$winning_age %in% 10:20 ~ "10-20",
      test_2$winning_age %in% 21:30 ~ "21-30",
      test_2$winning_age %in% 31:40 ~ "31-40",
      test_2$winning_age %in% 41:50 ~ "41-50",
      test_2$winning_age %in% 51:60 ~ "51-60",
      test_2$winning_age %in% 61:70 ~ "61-70",
      test_2$winning_age  %in% 71:80 ~ "71-80",
      test_2$winning_age  %in% 81:90 ~ "81-90",
      test_2$winning_age %in% 91:100 ~ "91-100",
      TRUE ~ as.character(test_2$winning_age)
    )
  ) %>% group_by(decade) %>%
  summarize(n=n(), na.rm=TRUE)%>%  select(decade, n)

test_4<-test_3%>%filter(decade != "NA")
View(test_4)


# Plotly ------------------------------------------------------------------


t <- list(
  family = "sans serif",
  size = 16,
  color = 'black')
m <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)
p_3 <- plot_ly(test_4,
             y = ~n,
             x = ~decade,
             type = "bar",   
             text =  ~paste('</br> Age range: ', decade,
                            '</br> Total number of winners: ', round(n,2)),
             hoverinfo = "text",
             marker = list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  layout(title = "Nobel prize winners - Age Range", font=t, autosize = F, width = 800, height = 600, margin = m)%>%
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = ""),
    annotations = 
      list(text = "#TidyTuesday 14.05.2019<br>@Juanma_MN", 
           showarrow = F, xref='paper', yref='paper', 
           xref = 'paper', x = 0,
           yref = 'paper', y = 1.2,
           font=list(size=8, color="black"))) %>%
  layout(
    
    annotations = 
      list(text = "There are 31 winners with no date of birth", 
           showarrow = F, xref='paper', yref='paper', 
           xref = 'paper', x = 1,
           yref = 'paper', y = -0.2,
           font=list(size=10, color="black")))
    
p_3



