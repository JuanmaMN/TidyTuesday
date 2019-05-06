
# Upload the data ---------------------------------------------------------

student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

View(student_ratio)

colnames(student_ratio)


# Prepare the colour palette ----------------------------------------------


# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

library(plotly)

p <- plot_geo(student_ratio) %>%
  add_trace(
    z = ~student_ratio, color = ~student_ratio, colors = 'Blues',
    frame = ~year, 
    text =  ~paste('</br> Country: ', country,
                     '</br> Year: ', year,
                     '</br> Student Ratio(%): ', round(student_ratio,2)), 
    
    
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
           font=list(size=14, color="black")),
    geo = g
  )

