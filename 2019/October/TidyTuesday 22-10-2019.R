# Packages ----------------------------------------------------------------

library(tidyverse)
library(anytime)
library(ggplot2)
library(waffle)


# Upload data -------------------------------------------------------------

horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")
colnames(horror_movies)



# Prepare the data for geom_waffle ----------------------------------------

horror_movies_review<-horror_movies%>% select(3,6)%>%na.omit()


horror_movies_review_2<-horror_movies_review%>%  
  mutate(release_year_3=anydate(horror_movies_review$release_date),
         release_year_2=dmy(horror_movies_review$release_date)) %>%
  mutate(mycol = coalesce(release_year_2,release_year_3)) %>%
  mutate(Year=format(mycol,"%Y")) %>% select(6,2)


# View(horror_movies_review_2)

horror_movies_review_2$review_rating<-as.numeric(horror_movies_review_2$review_rating)



horror_movies_review_3<-horror_movies_review_2%>%
  mutate(
    Review_Rating=case_when(
      horror_movies_review_2$review_rating >= 1 & horror_movies_review_2$review_rating < 5 ~ "Less than 5",
      horror_movies_review_2$review_rating >= 5 & horror_movies_review_2$review_rating < 7 ~ "Between 5 & 6.9",
      horror_movies_review_2$review_rating >= 7  ~ "Higher than 7",
      TRUE ~ as.character(horror_movies_review_2$review_rating)
    )
  ) %>% select(1,3)



horror_movies_review_3 %>%
  count(Year, Review_Rating) -> waffle

#View(waffle)



# waffle ------------------------------------------------------------------


ggplot(waffle, aes(fill = Review_Rating, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = T) +
  facet_wrap(~Year, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  scale_fill_manual(values = c("#E7B800","#00AFBB","#FC4E07")) +
  coord_equal() +
  labs(
    title = "Horror movie metadata - Number of rating reviews by Year",
    subtitle = "What year received higher reviews?\n",
    x = "",
    y = "Number of Reviews\n",
    caption ="\n Source: TidyTuesday 22.10.2019
      Visualization: JuanmaMN (Twitter @Juanma_MN)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size=9, face = "italic", hjust = 0.5),
        plot.caption = element_text(size = 8, face = "italic", color = "black"),
        axis.title=element_text(size=8),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.ticks.y = element_line(),
        legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) 





