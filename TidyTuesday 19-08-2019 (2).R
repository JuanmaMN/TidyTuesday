
# First ggridges ----------------------------------------------------------

nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")


# Upload the necessary packages -------------------------------------------

library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(dplyr)


test4<-nuclear_explosions%>%group_by(country,year)%>% filter(country %in% c("CHINA", "FRANCE", "UK", "USA", "USSR")) %>%
  summarize(total=n()) 

View(test4)


# ggplot ------------------------------------------------------------------


ggplot(test4, aes(x=year,y= reorder(country,desc(country)), fill = country, group = country)) +
  geom_density_ridges2(scale = 0.8)  + 
  scale_color_ipsum() +
  theme_ipsum_rc()+
  labs(
    title = "Nuclear Explosions",
    subtitle = "TidyTuesday 19.8.2019",
    caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)",
    x = "",
    y = "") +
  scale_fill_brewer(palette = "Spectral") + theme(legend.position = "",
                                                  legend.box = "") +
  scale_x_continuous(
    limits = c(1940, 2005),
    expand = c(0, 0)
  )








