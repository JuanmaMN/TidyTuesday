
# Upload the datasets -----------------------------------------------------

beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')

taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')


# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext,htmltools,reactable,patchwork,hrbrthemes,
               scales,ggtext, ggpubr, tidytext, showtext,ggwordcloud, grid, gridtext, cowplot)



# First plot --------------------------------------------------------------


unique_sentiments_taylor <- taylor_swift_lyrics %>%
  unnest_tokens(word, Lyrics) %>%
  anti_join(get_stopwords()) %>%
  group_by(Artist, word) %>%
  count() %>%
  inner_join(get_sentiments()) %>%
  group_by(word) %>% 
  filter(n > 10)



# Upload the image


grob_TS_sales <- grobTree(richtext_grob(
  sprintf("<br><br><b style='color:%s'>$52.60 million</b><br>in sales","#e3d18f"),  
  x=0.5,y=0.2, hjust=0.5, gp=gpar(col = "#a1a1a1", fontsize=15), vjust = 1))



first<-ggplot(unique_sentiments_taylor, aes(label = word,size = n,color = sentiment)) +
  geom_text_wordcloud_area(mask = img, rm_outside = TRUE) +
  scale_size_area(max_size = 35) +
  scale_color_manual(values = c("#a0c4a9", "#e6d492"), guide = F) +
  guides(fill = NULL) +
  labs(x = NULL, y = NULL, title = "Taylor Swift") +
  theme(
    plot.title = element_text(margin = margin(t=30, b = 5), 
                              color = "#808080",face = "bold",size = 12,
                              hjust = 0.5,
                              family = "Arial"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.ticks = element_blank(),
    strip.text.x = element_blank()
  )  +
  annotation_custom(grob_TS_sales)



# Second plot -------------------------------------------------------------


unique_sentiments_beyonce <- beyonce_lyrics %>%
  unnest_tokens(word, line) %>%
  anti_join(get_stopwords()) %>%
  group_by(artist_name, word) %>%
  count() %>%
  inner_join(get_sentiments()) %>%
  group_by(word) %>%
  filter(n > 10)

# Upload the image


grob_BY_sales <- grobTree(richtext_grob(
  sprintf("<br><br><b style='color:%s'>$32 million</b><br>in sales","#e3d18f"),  
  x=0.5,y=0.2, hjust=0.5, gp=gpar(col = "#a1a1a1", fontsize=15), vjust = 1))

second<-ggplot(unique_sentiments_beyonce,
               aes(label = word,size = n,color = sentiment)) +
  geom_text_wordcloud_area(mask = img,rm_outside = TRUE) +
  scale_color_manual(values = c("#a0c4a9", "#e6d492"), guide = F) +
  guides(fill = NULL) +
  scale_size_area(max_size = 35) +
  labs(x = NULL, y = NULL, title = "Beyoncé") +
  theme(
    plot.title = element_text(margin = margin(t=30,b = 5), 
                              color = "#808080",face = "bold",size = 12,
                              hjust = 0.5,
                              family = "Arial"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.ticks = element_blank(),
    strip.text.x = element_blank()
  )  +
  annotation_custom(grob_BY_sales)



# Both plots --------------------------------------------------------------



TB_plot<-(first| second) + plot_annotation(title = "Taylor Swift and Beyoncé Lyrics",
                                      caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)",
                                      theme = theme(plot.title = element_text(margin = margin(t=5, b = 5), 
                                                                              color = "#808080",face = "bold",size = 15,
                                                                              hjust = 0.5,
                                                                              family = "Arial"),
                                                    plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                                                                 color = "#808080", size = 8, family = "Arial",
                                                                                 hjust = 0.95),
                                                    plot.background = element_rect(fill = "#f7f7f7"),
                                                    panel.border = element_blank()))

TB_plot




