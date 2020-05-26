
# Upload the data ---------------------------------------------------------

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')


# Upload the packages -----------------------------------------------------

pacman::p_load(dplyr, lubridate, tidyverse, ggplot2)



# Prepare the data --------------------------------------------------------

cocktails$alcoholic[cocktails$alcoholic == "Non Alcoholic"] <- "Non alcoholic" 

monthtile2<-cocktails %>%
  mutate(year=format(as.Date(cocktails$date_modified, format="%Y-%m-%d %H:%M:%S"),"%Y"),
         monthA=format(as.Date(cocktails$date_modified, format="%Y-%m-%d %H:%M:%S"),"%m"),
         category = str_wrap(category, 6),  # wrap it here. If I wrap in the graph, it changes the order
         month = recode(monthA, "01" = "January",
                        "02" = "February",
                        "03" = "March",
                        "04" = "April",
                        "05" = "May",
                        "06" = "June",
                        "07" = "July",
                        "08" = "August",
                        "09" = "September",
                        "10" = "October",
                        "11" = "November",
                        "12" = "December",
         )) %>% 
  group_by(month,category) %>%
  summarise(total=n())  %>% na.omit()


# Level for months

monthtile2$month =  fct_relevel(monthtile2$month , 
                                c("December",
                                  "November",
                                  "October",
                                  "September",
                                  "August",
                                  "July",
                                  "April",
                                  "February",
                                  "January")
)


# geom_tile ---------------------------------------------------------------

tile2heatmap2 <- monthtile2 %>%  
  ggplot(aes(x = category, y = month)) +
  geom_tile(aes(fill = total), color = "#2b2b2b") +
  geom_text(aes(label = total), color = "#22292F") +
  scale_fill_gradient(low = "#ffef9a", high = "#e13d3d")+
  scale_x_discrete(position = "top") +
  guides(fill = NULL) +
  labs(x = "",y = "",
       title = "Cocktail consumption analysis",
       subtitle = "What drink is consumed the most in every month?",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 14,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#22222b", size = 9, family = "Arial",
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 9, family = "Arial",
                                 hjust = 0.95),
    axis.title.x = element_text(margin = margin(t = 10),
                                color = "#22222b"),
    axis.title.y = element_text(margin = margin(r = 15), 
                                color = "#22222b"),
    legend.position = "none",
    axis.text.x    = element_text(color = "#22222b"),
    axis.text.y    = element_text(color = "#22222b"),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7"),
    plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank()
  ) 

tile2heatmap2 +
  annotate(geom = "text", 
           x = 0.5, y = -1, 
           label = "No data for March, May, June\n", hjust = "left", 
           color="#808080",
           size = 2.5) 



