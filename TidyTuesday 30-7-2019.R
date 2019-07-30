
# Upload the data ---------------------------------------------------------

video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

View(video_games)
str(video_games)
colnames(video_games)
# unique(video_games$owners)


# Upload the necessary packages -------------------------------------------

library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(ggridges)


# Prepare the data --------------------------------------------------------

video_games$release_date<-mdy(video_games$release_date)

test2<-video_games%>%
  mutate(Year=year(release_date))%>%
  group_by(owners, Year) %>% filter(str_detect(owners, "000,000")) %>%
  filter(!str_detect(owners, "200,000,000"))%>%
  summarize (mean=round(mean(average_playtime,na.rm=TRUE),2))

View(test2)



# ggridges ----------------------------------------------------------------


ggplot(test2, aes(x=Year,y = reorder(owners,desc(owners)), fill = owners, group = owners)) +
  geom_density_ridges2(scale =1) + 
  theme_ft_rc(grid="X")+
  labs(
    title = "Video Games Dataset 2004-2018",
    subtitle = "TidyTuesday 30.7.2019",
    caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)",
    x = "",
    y = "") +
  scale_fill_brewer(palette = "Spectral") + theme(legend.position = "",
                                                  legend.box = "") +
  scale_x_continuous(
    breaks = c(2004:2018), limits = c(2000, 2025),
    expand = c(0, 0)
  )
