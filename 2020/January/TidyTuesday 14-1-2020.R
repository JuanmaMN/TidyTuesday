
# Upload data -------------------------------------------------------------

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')
#View(passwords)


# Load the packages -------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(hrbrthemes)


# Prepare the data --------------------------------------------------------

passwords2<- passwords%>%filter (category!= "NA") %>% group_by(category) %>%
  summarize(avg_value=round(mean(value,na.rm = TRUE),2),
            avg_strength=round(mean(strength,na.rm=TRUE),2))


passwords3<-gather(passwords2,measure, value,2:3)        
View(passwords3)

passwords3$measure <- recode(passwords3$measure, avg_value = "Average Time to crack by online guessing", 
                             avg_strength = "Strength = quality of password")

# Graph -------------------------------------------------------------------




g13<-passwords3%>% ggplot( aes(x=category, y=value)) +
  geom_bar(stat="identity", fill="#69b3a2", width=0.6) +
  coord_flip() +
  labs(x = "",y = "",
       title = "Password quality & strength analysis ",
       subtitle = "What passwords are easiest to guess and the strongest?",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
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
    axis.title.y = element_text(margin = margin(r = 15), 
                                color = "#22222b"),
    legend.position = "none",
    axis.text.x    = element_blank(),
    axis.text.y    = element_text(color = "#22222b"),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7"),
    plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank()
  ) +
  
  ylim(0,15) +
  
  ylab("") +
  
  xlab("") +
  
  facet_wrap(~measure, ncol=4) + geom_text(aes(label=value),hjust=-0.5) 





