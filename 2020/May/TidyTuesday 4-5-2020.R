# Upload packages ---------------------------------------------------------

library(tidyverse)
library(scales)
library(ggplot2)
library(ggtext)


# Upload data -------------------------------------------------------------


items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')



# Prepare the data --------------------------------------------------------

datafa<-items%>%group_by(category) %>% filter (buy_currency == "bells",
  sell_currency == "bells")%>% 
  summarise (buy_value=sum(buy_value,na.rm=TRUE), 
             sell_value=sum(sell_value,na.rm=TRUE),
             n=n()) %>%
  mutate(diff=buy_value-sell_value,
         percent=diff/n) %>% select(1,4,5,6)

datafa$color <- factor(ifelse(datafa$diff < 0, "low", "high"),   levels = c("low", "high"))


# Percentage --------------------------------------------------------------




graph2<-ggplot(datafa,aes(reorder(category,percent),percent, fill=color)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#8ab2b3","#bd9a99"), name = NULL) +
  labs(x = "",y = "",
       title = "Animal Crossing - New Horizons",
       subtitle = "Biggest difference between Buy and Sell per item value among categories",
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
    plot.background = element_rect(fill = "#ffffff"),
    plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank()
  ) + 
  geom_text(position = position_dodge(0.9), 
            vjust = -0.9,
            color = "black", size = 3, aes(label=comma_format()(percent)))

graph2 +
 annotate("text", x = 5, y =10000	,fontface ="bold",
         hjust = 0.5, color = "#e13d3d",
        size = 3, label = paste0("Furniture - Biggest difference: 9,802,983 
                                \n Furniture - Highest difference per item: 12,780.9
                                 \n Photos - Highest number of items: 840")) 







