
# Upload packages ---------------------------------------------------------

pacman::p_load(readxl,tidyverse,lubridate, tidyverse, ggplot2, hrbrthemes,patchwork,ggimage,ggdraw,patchwork)



# Raw data ----------------------------------------------------------------

wind<- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')



# Prepare the data --------------------------------------------------------

wind2<-wind%>%
  mutate(
    year	= substr(commissioning_date,1,4)
  ) %>% group_by(province_territory, year) %>%
  summarize(n=n())


wind2$province_territory = fct_relevel(wind2$province_territory, c("Yukon","Northwest Territories","Newfoundland and Labrador",
                                                                   "Prince Edward Island", "New Brunswick",  "Manitoba",
                                                                   "Saskatchewan", "British Columbia","Nova Scotia",
                                                                   "Alberta","Quebec",
                                                                   "Ontario"))



# First graph -------------------------------------------------------------

wind_first_graph <-wind2 %>% 
  ggplot(aes(x = year, y = province_territory)) +
  geom_tile(aes(fill = n), color = "#2b2b2b") +
  geom_text(aes(label = n), color = "#22292F",size = 3.5)+
  scale_fill_gradient(low = "#ececc2", high = "#20b2aa")+
  scale_x_discrete(position = "bottom") +
  guides(fill = NULL) +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x    = element_text(color = "#22222b", margin = margin(t = 15)),
    axis.text.y    = element_text(color = "#22222b"),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  ) 



# Second graph ------------------------------------------------------------


wind3<-wind2%>%
  group_by(province_territory)%>%
  summarize(n=sum(n))


wind_second_graph <- wind3 %>% 
  ggplot(aes(x = fct_reorder(province_territory,n), y = n, fill = n)) +
  geom_bar(stat="identity", width=0.8) +
  geom_text(aes(label = n), color = "#22292F",hjust=-0.2, size = 3.5) +
  coord_flip()+
  scale_fill_gradient(low = "#ececc2", high = "#20b2aa") +
  scale_y_continuous(limit = c(0, 3500), expand=c(0,1)) +
  guides(fill = NULL) +
  theme(
    axis.title.x =  element_blank(),
    axis.title.y =  element_blank(),
    legend.position = "none",
    axis.text.x    =  element_blank(),
    axis.text.y    =  element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),
    plot.margin = unit(c(0,0, 0, 0), "cm"),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) 



# Patchwork ---------------------------------------------------------------

patch<-wind_first_graph | wind_second_graph


Ppatch <- patch + plot_annotation(title = "Canadian Wind Turbines",
                                  subtitle = "Total number of wind turbines by Province/territory",
                                  caption = "\n Source: TidyTuesday 
                                  Visualization: JuanmaMN (Twitter @Juanma_MN)",
                                  theme = theme(plot.title = element_text(margin = margin(t=15,b = 8), 
                                                                          color = "#000000",face = "bold",size = 14,
                                                                          hjust = 0.5,
                                                                          family = "Arial"),
                                                plot.subtitle = element_text(margin = margin(t=10, b = 25), 
                                                                             color = "#000000", size = 10, family = "Arial",
                                                                             hjust = 0.5),
                                                plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                                                             color = "#000000", size = 8, family = "Arial",
                                                                             hjust = 0.95),
                                                plot.background = element_rect(fill = "#f7f7f7"),
                                                panel.border = element_blank()))


Ppatch