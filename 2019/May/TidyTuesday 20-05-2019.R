
# Upload the data ---------------------------------------------------------

waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

View(waste_vs_gdp)

colnames(waste_vs_gdp)

## name
names(waste_vs_gdp)[1]<- "Country"
names(waste_vs_gdp)[4]<- "Per_capita_plastic_waste"
names(waste_vs_gdp)[5]<- "GDP_per_capita"
names(waste_vs_gdp)[6]<- "Population"

View(waste_vs_gdp)


#############   omit na rows

data_plotly<-na.omit(waste_vs_gdp)  # Data for 2010 because for the rest of years we don't have all variables.


View(data_plotly)
colnames(data_plotly_Health_Education)

colnames(data_plotly)

library(plotly)
library(scales)
p_Tidy_Tuesday <-   plot_ly(data_plotly, 
                                x = ~GDP_per_capita, 
                                y = ~Per_capita_plastic_waste, 
                                color = ~Country, 
                                size = ~Population,
                                text =  ~paste('</br> Country: ', Country,
                                               '</br> Year: ', Year,
                                               '</br> Total population (Gapminder): ', comma_format()(Population),
                                               '</br> Per capita plastic waste (kilograms per person per day): ', Per_capita_plastic_waste,
                                               '</br> GDP per capita, PPP: ', round(GDP_per_capita,2)), 
                                
                                
                                hoverinfo = "text",
                                type = 'scatter',
                                mode = 'markers'
) %>%
  
  layout(xaxis = list(range = c(0, 130000), title = 'GDP per capita, PPP'),
         yaxis = list(range = c(0,5), title = 'Per capita plastic waste (kilograms per person per day)'),
         title = 'Per capita plastic waste VS GDP per capita by country',
         annotations = 
           list(text = "#TidyTuesday", 
                showarrow = F, xref='paper', yref='paper', 
                xref = 'paper', x = 0,
                yref = 'paper', y = 1,
                font=list(size=12, color="black"))
)


p_Tidy_Tuesday


