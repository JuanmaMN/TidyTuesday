# Upload the data ---------------------------------------------------------

diseases <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-10/diseases.csv")
View(diseases)


# Upload packages ---------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(scales)


# Prepare the data --------------------------------------------------------


data<-diseases%>%
  mutate(
    time=case_when(
      diseases$year %in% 1921:1950 ~ "1921-1950",
      diseases$year %in% 1951:1980 ~ "1951-1980",
      diseases$year %in% 1981:2020 ~ "1981-2020",
      TRUE ~ as.character(diseases$year)
    )
  ) %>% filter(state %in% c("California", "New York","Michigan", "Pennsylvania","Texas","Wisconsin","Ohio")) %>% group_by(disease, time, state)%>%summarize(total=sum(count))



# Graphs ------------------------------------------------------------------


gA<- ggplot(data, aes(x=reorder(state,total)),width=600, height=600) +
  geom_bar(aes(y = total, fill = time),stat="identity") +
  scale_y_continuous(labels = comma_format())+
  coord_flip() +
  theme_ipsum() +
  labs(x = "",y = "",
       title = "Diseases - Top 7 states",
       subtitle = "Which state has the highest number of diseases?",
       caption = "You can make it in R\nVisualization: JuanmaMN (Twitter @Juanma_MN)")+
  scale_fill_manual(values=c('#f08080','#20b2aa','#87cefa','#f8d568'))+  
  theme(
    plot.title = element_text(margin = margin(b = 10), 
                              color = "#22222b",face = "bold",size = 17,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(b = 25), 
                                 color = "#22222b", size = 12, family = "Arial"),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10, family = "Arial"),
    #panel.grid.major = element_line(color = "#DAE1E7"),
    panel.grid.major.y = element_line(color = "#dbdbdb"),
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.title = element_text (size = 15),
    axis.title.y = element_text(margin = margin(r = 10), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10), hjust = 0.5),
    plot.margin = unit(c(1, 2, 1, 1), "cm"),
    legend.position=""
  ) 




gB<- ggplot(data, aes(x=reorder(disease,total)),width=600, height=600) +
  geom_bar(aes(y = total, fill = time),stat="identity") +
  scale_y_continuous(labels = comma_format())+
  coord_flip() +
  theme_ipsum()  +
  labs(x = "",y = "",
       title = "Diseases - Total number by type",
       subtitle = "Which disease has the highest number of cases?",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")+
  scale_fill_manual(values=c('#f08080','#20b2aa','#87cefa','#f8d568'))+  
  theme(
    plot.title = element_text(margin = margin(b = 10), 
                              color = "#22222b",face = "bold",size = 17,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(b = 25), 
                                 color = "#22222b", size = 12, family = "Arial"),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10, family = "Arial"),
    #panel.grid.major = element_line(color = "#DAE1E7"),
    panel.grid.major.y = element_line(color = "#dbdbdb"),
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.title = element_text (size = 15),
    axis.title.y = element_text(margin = margin(r = 10), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10), hjust = 0.5),
    plot.margin = unit(c(1, 2, 1, 1), "cm"),
    legend.position="right",
    legend.title = element_blank()
  ) 


library(gridExtra)

grid.arrange(gA,gB, ncol=2)





