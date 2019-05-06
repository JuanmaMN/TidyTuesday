
# Upload the data ---------------------------------------------------------

student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")


# Create average student ratio ---------------------------------------------

library(dplyr)
student_ratio<-student_ratio%>%group_by(country)%>%mutate(mean = mean(student_ratio, na.rm = TRUE))


# Prepare the colour palette ----------------------------------------------


library(plotly)

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)


# Plotly graph ------------------------------------------------------------



p <- plot_geo(student_ratio) %>%
  add_trace(
    z = ~student_ratio, color = ~student_ratio, colors = 'Blues',
    frame = ~year, 
    text =  ~paste('</br> Country: ', country,
                     '</br> Year: ', year,
                     '</br> Global Student to Teacher Ratios(%): ', round(student_ratio,2)), 
    
    
    hoverinfo = "text"
  , locations = ~country_code, marker = list(line = l)
  ) %>%
  colorbar(title = 'Global Student to Teacher Ratios') %>%
  layout(
    title = 'Global Student to Teacher Ratios<br><a href="http://data.uis.unesco.org/index.aspx?queryid=180">UNESCO Institute of Statistics</a>',
    annotations = 
      list(text = "#TidyTuesday 07.05.2019<br>@Juanma_MN", 
           showarrow = F, xref='paper', yref='paper', 
           xref = 'paper', x = 0,
           yref = 'paper', y = 1,
           font=list(size=10, color="black")),
    geo = g
  )

p


