
# Upload the packages -----------------------------------------------------

library(tidyverse)

library(hrbrthemes)

devtools::install_github("thebioengineer/tidytuesdayR")


tuesdata <- tidytuesdayR::tt_load('2020-01-21') 
tuesdata <- tidytuesdayR::tt_load(2020, week = 4)
spotify_songs <- tuesdata$spotify_songs


#spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')


# Prepare the data --------------------------------------------------------

data<-spotify_songs%>%mutate(year= substr(track_album_release_date, start=1, stop=4)) %>% group_by(playlist_genre, year) %>% summarize(n=n())


data2<-data%>% filter(year %in% c("2000", "2010", "2020")) %>% spread(year,n)  # this step wouldn't be necessary to do as we're using gather in the graph

names(data2)[2]<- "total_2000"
names(data2)[3]<- "total_2010"
names(data2)[4]<- "total_2020"

data2$playlist_genre <- gsub("&", "", data2$playlist_genre)

data2$playlist_genre <- recode(data2$playlist_genre, rb = "rhythm and blues", 
                               edm = "electronic dance music")

data3<-data%>% filter(year %in% c("2000", "2010", "2020"))
data3$playlist_genre <- gsub("&", "", data3$playlist_genre)

unique(data3$playlist_genre)
data3$playlist_genre <- recode(data3$playlist_genre, rb = "rhythm and blues", 
                               edm = "electronic dance music")

data3$playlist_genre<- factor(data3$playlist_genre, levels = c("rock",
                                                               "rhythm and blues",
                                                               "latin",
                                                               "rap",
                                                               "pop","electronic dance music"))



# Graph -------------------------------------------------------------------


graph<-ggplot() +
  
  geom_segment(data3 %>%
                 group_by(playlist_genre) %>% 
                 top_n(-1) %>% 
                 slice(1) %>%
                 ungroup(),
               mapping = aes(x = 0, xend = n, y = playlist_genre, yend = playlist_genre),
               linetype = "blank", size = 0.3, color = "gray80"
  ) +
  
  geom_segment(
    data3 %>% 
      group_by(playlist_genre) %>% 
      summarise(start = range(n)[1], end = range(n)[2]) %>% 
      ungroup(),
    mapping = aes(x = start, xend = end, y = playlist_genre, yend = playlist_genre),
    color = "gray80", size = 2
  ) +
  # reshape the data frame & plot the points
  geom_point(
    data3,
    mapping = aes(n, playlist_genre, color = year), 
    size = 4
  ) 


# Include the text

graph1<-graph+
  geom_text(data = filter(data2, playlist_genre == "electronic dance music"),
            aes(x = total_2000, y = playlist_genre),
            label = "2000", fontface = "bold",
            color = "#F7BC08",
            vjust = -2) +
  geom_text(data = filter(data2, playlist_genre == "electronic dance music"),
            aes(x = total_2010, y = playlist_genre),
            label = "2010", fontface = "bold",
            color = "#F7BC08",
            vjust = -2)  +
  geom_text(data = filter(data2, playlist_genre == "electronic dance music"),
            aes(x = total_2020, y = playlist_genre),
            label = "2020", fontface = "bold",
            color = "#F7BC08",
            vjust = -2)


# Include the theme

graph1+
  theme_minimal() +
  labs(
    title = "Number of songs by genre - 2000 to 2020",
    subtitle = "What genre saw the highest increase in songs since 2000 on Spotify?",
    caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)",
    x = "Total songs",
    y = "") + theme(legend.position = "none",
                    legend.box = "horizontal",
                    plot.background=element_rect(fill="#f7f7f7"),
                    plot.title = element_text(margin = margin(b = 8), 
                                              color = "#22222b",face = "bold",size = 14,
                                              hjust = 0.5,
                                              family = "Calibri"),
                    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                                 color = "#22222b", size = 9, family = "Arial",
                                                 hjust = 0.5),
                    plot.caption =  element_text(margin = margin(t = 20), 
                                                 color = "#22222b", size = 10, family = "Arial",
                                                 hjust = 0.95))