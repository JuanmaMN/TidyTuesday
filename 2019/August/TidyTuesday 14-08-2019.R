# Upload the data ---------------------------------------------------------

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(lubridate)

emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")




# Calculate the reign length ----------------------------------------------

reign_start <- as.POSIXct(emperors$reign_start, format = "%Y-%m-%d")
reign_end<- as.POSIXct(emperors$reign_end, format = "%Y-%m-%d")



empeorers2<-emperors %>% mutate(elapsed_time = (reign_start %--% reign_end)/ddays(1)) %>% select("name", "reign_start",
                                                                                                 "reign_end", "elapsed_time", "rise","cause","killer","dynasty","era")


empeorers4<-empeorers2%>%group_by(dynasty,rise)%>%summarize(total=round(sum(elapsed_time),0)) %>%
  arrange(-total)   # for second graph



# Order the column by total -----------------------------------------------


empeorers4$dynasty <- factor(empeorers4$dynasty, levels = c("Theodosian","Flavian","Julio-Claudian","Severan","Gordian", "Valentinian", 
                                                            "Nerva-Antonine","Constantinian"))




g2<- ggplot(empeorers4, aes(dynasty)) +
  geom_bar(aes(y = total, fill = rise),stat="identity") +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  theme_ipsum_tw()  + 
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
    caption = "\n Source: TidyTuesday 14.8.2019
      Visualization: JuanmaMN (Twitter @Juanma_MN)")
g2
