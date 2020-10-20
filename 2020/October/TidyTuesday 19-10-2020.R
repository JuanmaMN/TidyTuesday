

# Upload the packages -----------------------------------------------------

pacman::p_load(readxl,tidyverse,lubridate, tidyverse, ggplot2, hrbrthemes,patchwork,ggimage,ggdraw, maps, viridis,
               biscale, cowplot, grid, gridtext,hrbrthemes,scales,ggtext, ggpubr,rgeos,mapproj,usmap, geojsonio)


# Upload the data ---------------------------------------------------------

beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')


# Bring US states ---------------------------------------------------------

US_States <- read_excel("US States.xlsx")
names(US_States)[2]<-"state"


# Prepare the data --------------------------------------------------------

beer_awards<-beer_awards%>%left_join(US_States, by = "state") 

beer_awards<-beer_awards%>%filter(medal == "Gold")%>%group_by(medal, State)%>% summarize(n=n())


# Build the graph ---------------------------------------------------------


map_data <- geojson_read("us_states_hexgrid.geojson",  what = "sp")

map_data@data = map_data@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))


map_data_fortified <- broom::tidy(map_data, region = "google_name")


join_data <- map_data_fortified %>%
  left_join(. , beer_awards, by=c("id"="State"))


centers <- cbind.data.frame(data.frame(gCentroid(map_data, byid=TRUE), id=map_data@data$iso3166_2))



# Font and grobtext -------------------------------------------------------

font_text <- "Nova Mono"
font_title <- "Work Sans"
font_subtitle <- "Work Sans"


grob_comment  <- grobTree(richtext_grob(
  sprintf("<b style='color:%s'>California</b><br><br> is the state <br> with the highest Gold awards <br>","#e3d18f"),  
  x=-140,y=35, hjust=0.5, gp=gpar(col = "grey50", fontsize=3), vjust = 1))


# Graph -------------------------------------------------------------------



graph2<-join_data %>% 
  ggplot() +
  geom_polygon(aes(fill = n, x = long, y = lat, group = group), 
               color = "white") +
  geom_text(data=centers, aes(x=x, y=y, label=id), color = "grey40", size = 3) +
  scale_fill_gradient(low = "#fbfbd2", high = "#dcc469", na.value = "#ffffff")+
  theme_void() +
  coord_map() +
  labs(
    title = "Beer Awards - Gold",
    subtitle = "Total number of gold awards by state since 1987",
    fill = "Gold awards \nby state",
    caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)")+
  theme(
    plot.title = element_text(margin = margin(t=0.5, b = 5), 
                              color = "#e3d18f",face = "bold",size = 12,
                              hjust = 0.5,
                              family = font_title),
    plot.subtitle = element_text(margin = margin(t=2.5,b = 10), 
                                 color = "grey50", size = 9, family = font_text,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 10), 
                                 color = "grey50", size = 8, family = font_subtitle,
                                 hjust = 0.95),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(color = "grey50", size = 8, family = font_text),
    legend.title.align=0.5,
    legend.margin = margin(1.22, 0.2, 0.5, 0.2, "cm"),
    legend.text = element_text(color = "grey50",family = font_text),
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(2.5, 1, 2.5, 1), "cm"),
    axis.ticks = element_blank(),
    strip.text.x = element_blank()
  ) +annotate(geom = "text", x = -133, y = 35, size = 3, label = "California is the state \nwith the highest number of \nGold awards", 
              family = font_text,    hjust = "center", lineheight = 0.9, color = "#e3d18f") 




