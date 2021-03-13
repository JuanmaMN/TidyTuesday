
# Upload the data ---------------------------------------------------------

shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

# Upload the packages -----------------------------------------------------


pacman::p_load(readxl, lubridate, tidyverse, ggplot2, ggtext, hrbrthemes, gridExtra,patchwork, systemfonts, sysfonts,showtext, ggpubr)
               


# Font --------------------------------------------------------------------

font_add_google("Lora")

font_labels <- "Lora"

showtext_auto()



# Prepare the data --------------------------------------------------------

total_shelters_city_A<-shelters%>%filter(sector == "Youth" & occupancy_date == "2019-12-31") %>%
  group_by(occupancy_date, shelter_city)%>%summarize(total=sum(occupancy)) %>% ungroup() %>%
  mutate(percentage=total/sum(total),
         pcnt=round(percentage*100, 2),
         yaxismax=cumsum(percentage), 
         yaxismin = c(0, head(yaxismax, n=-1)),
         label_position = (yaxismax+yaxismin)/2,
         label = if_else(pcnt >= 50, 
                         paste0(pcnt, "%"),
                         NA_character_),
         label2 = paste0(total," ", "in total"),
         label3= paste0(label, "\n",label2)
  )

total_shelters_city_B<-shelters%>%filter(sector == "Youth" & occupancy_date == "2017-01-01") %>%
  group_by(occupancy_date, shelter_city)%>%summarize(total=sum(occupancy)) %>% ungroup() %>%
  mutate(percentage=total/sum(total),
         pcnt=round(percentage*100, 2),
         yaxismax=cumsum(percentage), 
         yaxismin = c(0, head(yaxismax, n=-1)),
         label_position = (yaxismax+yaxismin)/2,
         label = if_else(pcnt >= 50, 
                         paste0(pcnt, "%"),
                         NA_character_),
         label2 = paste0(total," ", "in total"),
         label3= paste0(label, "\n",label2)
  )



# Graphs ------------------------------------------------------------------


total_shelters_city_graph<-ggplot(total_shelters_city_A, aes(ymax=yaxismax, ymin=yaxismin, xmax=4, xmin=3, 
                                                             fill=shelter_city)) +
  geom_rect(alpha=0.5) + 
  geom_richtext(aes(label=" <span style='color:#cdaa25'>96.82%   <br><br> 6,526 people</span>",
                    x=1, y=0),
                fill=NA, label.color=NA,
                family = font_labels,
                size=3.5)+
  coord_polar(theta="y") +
  xlim(c(1, 5))+ 
  theme_void() + 
  scale_fill_manual(breaks=c("Etobicoke","North York","Toronto"),values=c("#92aae5", "#43a07e","#CDAA25")) +
  geom_text(aes(label="December 31st 2019", x=5, y=0), color="#767676", family = font_labels, size=4)+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.margin=margin(b = 2, unit='cm'),
        legend.text=element_text(size=8),
        strip.background = element_rect(fill = "#f4efe1"),
        strip.text.x = element_blank(),
        legend.key.size = unit(0.3, "cm"))+ 
  guides(fill = guide_legend(
    label.position = "bottom",
    family = font_labels, 
    color = "#808080",
    keywidth = 3, keyheight = 0.5)) 

total_shelters_city_graph2<-ggplot(total_shelters_city_B, aes(ymax=yaxismax, ymin=yaxismin, xmax=4, xmin=3, 
                                                              fill=shelter_city)) +
  geom_rect(alpha=0.5) + 
  geom_richtext(aes(label=" <span style='color:#cdaa25'>74.95%  <br><br> 374 people</span>",
                    x=1, y=0),
                fill=NA, label.color=NA,
                family = font_labels,
                size=3.5)+
  coord_polar(theta="y") +
  xlim(c(1, 5))+ 
  theme_void() + 
  scale_fill_manual(breaks=c("Etobicoke","North York","Toronto","Scarborough"),values=c("#92aae5", "#43a07e","#CDAA25","grey")) +
  geom_text(aes(label="January 1st 2017", x=5, y=0), color="#767676", family = font_labels, size=4) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.margin=margin(b = 2, unit='cm'),
        legend.text=element_text(size=8),
        strip.background = element_rect(fill = "#f4efe1"),
        strip.text.x = element_blank(),
        legend.key.size = unit(0.3, "cm")) + 
  guides(fill = guide_legend(
    label.position = "bottom",
    family = font_labels, 
    color = "#808080",
    keywidth = 3, keyheight = 0.5)) 



ggarrange(total_shelters_city_graph2,total_shelters_city_graph, ncol=2, nrow=1, common.legend = TRUE, legend="bottom") +
  theme_ipsum() +
  labs(x = "",y = "",
       title = "Toronto Shelters - Youth ",
       subtitle = "Percentage by city of the total youth occupancy",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#343434",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10, b = 25), 
                                 color = "#000000", size = 10, family = font_labels,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                 color = "#808080", size = 8, family = font_labels,
                                 hjust = 0.95),
    plot.background = element_rect(fill = "#f7f7f7", color = NA))  