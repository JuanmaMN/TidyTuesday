
# Upload the data ---------------------------------------------------------

library(ggplot2)
library(hrbrthemes)

library(gridExtra)

emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

View(emperors)

colnames(emperors)
str(emperors)

library(dplyr)
library(lubridate)


# Calculate the reign length ----------------------------------------------

reign_start <- as.POSIXct(emperors$reign_start, format = "%Y-%m-%d")
reign_end<- as.POSIXct(emperors$reign_end, format = "%Y-%m-%d")
View(emperors)


empeorers2<-emperors %>% mutate(elapsed_time = (reign_start %--% reign_end)/ddays(1)) %>% select("name", "reign_start",
                                                                                                 "reign_end", "elapsed_time", "rise","cause","killer","dynasty","era")
View(emperors)
View(empeorers2)


empeorers3<-empeorers2%>%group_by(dynasty)%>%summarize(average=round(sum(elapsed_time),0))    # for first graph




empeorers4<-empeorers2%>%group_by(dynasty,rise)%>%summarize(average=round(sum(elapsed_time),0)) %>%
  arrange(-average)   # for second graph





# First graph -------------------------------------------------------------



g<- ggplot(empeorers3, aes(x=reorder(dynasty,average), y=average)) +
  geom_bar(stat="identity", fill="#69b3a2", width=0.6) +
  coord_flip() +
  
  theme_ipsum() +
  
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text( size=48 )
  ) +
  
  ylim(0,60000) +
  
  ylab("Total time in days") +
  
  xlab("") +
  
  labs(
    title = "Roman Emperors Dataset",
    subtitle = "Total length of reign by dynasty (in days)",
    caption = "\n Source: TidyTuesday 13.8.2019
      Visualization: JuanmaMN (Twitter @Juanma_MN)")

g1<-g + geom_text(aes(label=average),hjust=-0.5) 



# Second graph --------------------------------------------------------------




g2<- ggplot(empeorers4, aes(reorder(dynasty,average))) +
  geom_bar(aes(y = average, fill = rise),stat="identity") +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  theme_ipsum()   + 
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="bottom",
    axis.text = element_text( size=48 )
  ) +
  ylim(0,60000) +
  ylab("Total time in days") +
  xlab("") +
  
  labs(
    title = "Roman Emperors Dataset",
    subtitle = "Total length of reign by dynasty (in days)",
    caption = "\n Source: TidyTuesday 13.8.2019
      Visualization: JuanmaMN (Twitter @Juanma_MN)")




grid.arrange(g1,g2, ncol=2)

