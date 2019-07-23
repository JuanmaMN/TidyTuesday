wildlife_impacts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-23/wildlife_impacts.csv")

View(wildlife_impacts)

# Understand the data -----------------------------------------------------

unique(wildlife_impacts$operator)

unique (wildlife_impacts$damage)

unique (wildlife_impacts$time_of_day)

unique(wildlife_impacts$damage)




# Tidyverse ---------------------------------------------------------------

library (tidyverse)
library(hrbrthemes)
library (ggridges)
library(dplyr)
library(ggplot2)


# Prepare the data --------------------------------------------------------

data_damage<-wildlife_impacts%>% group_by(incident_month,incident_year,time_of_day,operator, damage)%>% filter(!is.na(time_of_day) & damage %in% c("N", "M", "S")) %>%summarize(n=n())


# Graph -------------------------------------------------------------------


ggplot(data_damage, aes(x=incident_year,y = reorder(time_of_day,desc(time_of_day)), fill = operator, group = interaction(operator,time_of_day))) +
  geom_density_ridges2(scale = 0.9) + 
  theme_ft_rc(grid="X")+
  labs(
    title = "Wildlife strikes 1990-2018",
    subtitle = "TidyTuesday 23.7.2019",
    caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)",
    x = "",
    y = "") +
  scale_fill_brewer(palette = "Spectral") + scale_x_continuous(breaks=seq(1990,2018,4))





#scale=1 to avoid overlap

