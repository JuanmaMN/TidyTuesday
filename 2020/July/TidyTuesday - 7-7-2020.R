
# Upload data -------------------------------------------------------------

coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')




# Upload packages ---------------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext,htmltools,reactable,patchwork, choroplethr,choroplethrMaps,
               choroplethrZip,mapproj,hrbrthemes, usmap,scales,ggtext, ggpubr)






# Prepare the data for first graph ----------------------------------------



data_coffe_donut<-coffee_ratings%>%group_by(species)%>% summarize(total=n()) %>%
  mutate(ftotal=total/sum(total), 
         pcnt=round(ftotal*100, 1), 
         yaxismax=cumsum(ftotal), 
         yaxismin = c(0, head(yaxismax, n=-1)),
         label_position = (yaxismax+yaxismin)/2) 



# First graph -------------------------------------------------------------


gender_coffeeMdonut<-ggplot(data_coffe_donut, aes(ymax=yaxismax, ymin=yaxismin, xmax=4, xmin=3, fill=species)) +
  geom_rect(show.legend=F, alpha=0.6) + 
  coord_polar(theta="y") +
  xlim(c(1, 5))+ 
  theme_void() + 
  scale_fill_manual(values=c("#CDAA25", "#43a07e")) +
  geom_text(x=3.5, aes(y=label_position, label=paste(pcnt,"%",sep="")), size=4, col=c("black", "black"))  + 
  geom_richtext(aes(label="<span style='color:#000000'><br></span>
                      <span style='color:#CDAA25'>Arabica	<br></span>
                      <span style='color:#43a07e'>Robusta	<br></span>",
                    x=1, y=0),
                fill=NA, label.color=NA,
                family="Arial",
                size=3)   +
  theme(
    plot.background = element_rect(fill = "#f4efe1", color = NA),    # color removes the border
    plot.margin = unit(c(0,1,0, 0), "cm")
  )





# Prepare the data for second graph ---------------------------------------


data_coffe_both<-coffee_ratings%>%	group_by(country_of_origin)%>% summarize(total=n()) %>% top_n(5, total) 


# Second graph ------------------------------------------------------------


gdata_coffee_both<-data_coffe_both%>% ggplot(aes(x=reorder(country_of_origin,total), y=total), fill=country_of_origin) +
  geom_bar(stat="identity", fill=c("#CDAA25","#CDAA25","#CDAA25","#CDAA25","#CDAA25"), width=0.6,
           alpha=0.5)  +
  coord_flip() +
  geom_text(aes(label=total),hjust=-0.5,
            color = c("#CDAA25","#CDAA25","#CDAA25","#CDAA25","#CDAA25"))     +
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
                                color = "#CDAA25"),
    axis.title.y = element_text(margin = margin(r = 15), 
                                color = "#CDAA25"),
    legend.position ="none",
    legend.title = element_blank(),
    legend.key.size = unit(0.3, "cm"),
    legend.text=element_text(size=8),
    legend.box = "horizontal",
    legend.background = element_rect(fill = "#f4efe1", color = NA),
    legend.margin=margin(b = 0.2, unit='cm'),
    axis.text.x    = element_blank(),
    axis.text.y    = element_text(color = "#CDAA25"),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    strip.background = element_rect(fill = "#f4efe1"),
    strip.text.x = element_blank(),
    plot.background = element_rect(fill = "#f4efe1", color = NA),    # color removes the border
    #plot.margin = unit(c(0,0,0, 2), "cm"),
    plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank()
    
  )+
  
  ylim(0,300) +
  
  ylab("") +
  
  xlab("") 




#patchwork_coffee1<- gender_coffeeMdonut | gdata_coffee_both 


# First arrange -----------------------------------------------------------


ggarrange(gender_coffeeMdonut,gdata_coffee_both, ncol=2, nrow=1, common.legend = TRUE, legend="bottom") +
  theme_ipsum() +
  labs(x = "",y = "",
       title = "Coffee beans by species and country",
       subtitle = "",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 14,
                              hjust = 0.5,
                              family = "Arial"),
    plot.background = element_rect(fill = "#f4efe1", color = NA))  



# Patchwork ---------------------------------------------------------------

patchwork_coffee2<- gender_coffeeMdonut  / gdata_coffee_both + plot_annotation(title = "Coffee beans by species and country",
                                                                               subtitle = "",
                                                                               caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)",
                                                                               theme = theme(plot.background = element_rect(fill = "#f4efe1",
                                                                                                                            size = 10),
                                                                                             plot.margin = unit(c(1, 2, 2, 1), "cm"),
                                                                                             plot.title = element_text(margin = margin(t=5, b = 5), 
                                                                                                                       color = "#22222b",face = "bold",size = 15,
                                                                                                                       hjust = 0.5,
                                                                                                                       family = "Arial"),
                                                                                             plot.caption =  element_text(margin = margin(t = 5, b=10), 
                                                                                                                          color = "#000000", size = 8, family = "Arial",
                                                                                                                          hjust = 0.95)))  

