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



# geom_dumbbell -----------------------------------------------------------------


library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggalt)
library(scales)
library(hrbrthemes)


data_damage_2<-wildlife_impacts%>% group_by(operator,incident_year)%>%  filter (incident_year %in% c("1990", "2018")) %>%summarize(n=n())


spread<-spread(data_damage_2,incident_year,n)

View(spread)



ggplot(spread, aes(x = `1990`, xend = `2018`, y=operator)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1")+
  labs(x=NULL, y=NULL, title="ggplot2 geom_dumbbell with dot guide") +
  theme_ft_rc(grid="X")  +
  labs(
    title = "Wildlife strikes 1990-2018",
    subtitle = "TidyTuesday 23.7.2019",
    caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)",
    x = "",
    y = "") + theme(legend.position = "bottom",
                    legend.box = "vertical")  + geom_text(data = filter(spread, operator == "UNITED AIRLINES"),
                                                          aes(x = `2018`, y = operator),
                                                          label = "2018", fontface = "bold",
                                                          color = "#395B74",
                                                          vjust = -2) +
                                                geom_text(data = filter(spread, operator == "UNITED AIRLINES"),
                                                          aes(x = `1990`, y = operator),
                                                          label = "1990", fontface = "bold",
                                                          color = "#F7BC08",
                                                          vjust = -2)
