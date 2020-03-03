# Upload packages ---------------------------------------------------------

library(readr)
library(tidyverse)
library(hrbrthemes)
library(readxl)
library(scales)
library(ggthemes)


# Raw Data ----------------------------------------------------------------

season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

# Prepare data ------------------------------------------------------------

data_lift<-season_goals%>% group_by(season) %>%
  filter (league == "NHL")%>% 
  summarize (total_point=sum(points))

View(data_lift)

data_lift2<-data_lift%>%mutate(lag1 = lag(total_point),
                               increase = (total_point/ lag1) - 1,
                               label=percent(increase))%>%
  filter (season >"1990-91")



data_lift2$color <- factor(ifelse(data_lift2$increase < 0, "low", "high"),   levels = c("low", "high"))

data_lift3<-data_lift2%>% select(1,4,5,6)

data_lift3$increase<-as.numeric(data_lift3$increase)


# Graph -------------------------------------------------------------------


graph<-ggplot(data_lift3,aes(season, increase, fill=color)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#f08080","#20b2aa"), name = NULL) +
  labs(x = "",y = "",
       title = "NHL Total Points",
       subtitle = "Season on Season change in overall points in NHL games",
       caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)")+
  guides(fill = NULL) +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 14,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#22222b", size = 9, family = "Arial",
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10, family = "Arial",
                                 hjust = 0.95),
    axis.title.x = element_text(margin = margin(t = 10),
                                color = "#22222b"),
    legend.position = "none",
    axis.text.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7"),
    plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank()
  ) + 
  #scale_x_continuous(breaks = c(2001:2019)) + 
  geom_text(position = position_dodge(0.9), 
            vjust = -0.9,
            color = "black", size = 3, aes(label=label))


graph +
  annotate("text", x = 25, y =0.3	,fontface ="bold",
           hjust = 0.5, color = "#e13d3d",
           size = 3, label = paste0("2014-2020 \nSix consecutive seasons \nwith decrease in points")) +
  annotate("text", x = 18, y =0.3	,fontface ="bold",
           hjust = 0.5, color = "#e13d3d",
           size = 3, label = paste0("2006-2013 \nSeven consecutive seasons \nwith decrease in points")) +
  annotate("text", x = 7, y =0.9,fontface ="bold",
           hjust = 0.5, color = "#20b2aa",
           size = 3, label = paste0("1995-96 \nSeason with the biggest \nincrease in points\ncompared \nwith previous season"))




