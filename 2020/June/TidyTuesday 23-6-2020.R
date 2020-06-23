# Upload packages ---------------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext,htmltools,reactable,patchwork, choroplethr,choroplethrMaps,
               choroplethrZip,mapproj,hrbrthemes, usmap,scales,ggtext, ggpubr)



# Upload data -------------------------------------------------------------

individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')



# First graph -------------------------------------------------------------

individuals_deploy_off_type<-individuals %>% group_by(deploy_off_type)%>% summarize(total=n()) %>%
  mutate(ftotal=total/sum(total),
         pcnt=round(ftotal*100, 1), 
         yaxismax=cumsum(ftotal), 
         yaxismin = c(0, head(yaxismax, n=-1)),
         label_position = (yaxismax+yaxismin)/2)



color_mine <- c("#a0c4a9", "#c27c7c","#734b5e", "#aaa4b0")

gender_donutfirst<-ggplot(individuals_deploy_off_type, aes(ymax=yaxismax, ymin=yaxismin, xmax=4, xmin=3, fill=deploy_off_type)) +
  geom_rect(show.legend=T, alpha=0.5) + 
  coord_polar(theta="y") +
  xlim(c(1, 5))+ 
  theme_void()+
  expand_limits(x = 0, y = 0) +
  scale_fill_manual(values = color_mine) +
  geom_text(x=3.5, aes(y=label_position, label=paste(pcnt,"%",sep="")), size=3, col="black")  + 
  geom_richtext(aes(label=" <span style='color:#525252'>Percentage of tag type classifications<br></span>",
                    x=1, y=0),
                fill=NA, label.color=NA,
                family="Arial",
                size=3.5)  +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.margin=margin(b = 2, unit='cm'),
        legend.text=element_text(size=8),
        strip.background = element_rect(fill = "#f4efe1"),
        strip.text.x = element_blank(),
        legend.key.size = unit(0.3, "cm")) +
  geom_text(aes(label="Classification of tag deployment end", x=5, y=0), color="#000000", family="Calibri", size=4)




# Second graph ------------------------------------------------------------

individuals_study_site2<-individuals  %>% mutate(study_site2= case_when(
  study_site == "Hart Ranges"  ~ "Hart Ranges",
  study_site == "Graham"  ~ "Graham",
  study_site == "Kennedy"  ~ "Kennedy",
  study_site == "Quintette"  ~ "Quintette",
  study_site == "Moberly"  ~ "Moberly",
  TRUE                      ~ "Rest")) %>%
  group_by(study_site2)%>% summarize(total=n()) %>%
  mutate(ftotal=total/sum(total),
         pcnt=round(ftotal*100, 1), 
         yaxismax=cumsum(ftotal), 
         yaxismin = c(0, head(yaxismax, n=-1)),
         label_position = (yaxismax+yaxismin)/2)


color_mine2 <- c("#a0c4a9", "#c27c7c", "#734b5e", "#aaa4b0", "#c8a774","#e6d492")

gender_donut_second<-ggplot(individuals_study_site2, aes(ymax=yaxismax, ymin=yaxismin, xmax=4, xmin=3, fill=study_site2)) +
  geom_rect(show.legend=T, alpha=0.5) + 
  coord_polar(theta="y")+
  xlim(c(1, 5))+ 
  theme_void()+
  expand_limits(x = 0, y = 0) +
  scale_fill_manual(values = color_mine2) +
  geom_text(x=3.5, aes(y=label_position, label=paste(pcnt,"%",sep="")), size=3, col="black")  + 
  geom_richtext(aes(label="<span style='color:#525252'>Percentage of woodland caribou <br>by Deployment site or colony<br></span>",
                    x=1, y=0),
                fill=NA, label.color=NA,
                family="Arial",
                size=3.5)  +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.margin=margin(b = 2, unit='cm'),
        legend.box = "horizontal",
        legend.text=element_text(size=8),
        strip.background = element_rect(fill = "#f4efe1"),
        strip.text.x = element_blank(),
        legend.key.size = unit(0.3, "cm")) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  geom_text(aes(label="Deployment site or colony analysis of woodland caribou", x=5, y=0), color="#000000", family="Calibri", size=4)




# Cowplot -----------------------------------------------------------------


ggarrange(gender_donutfirst,gender_donut_second, ncol=2, nrow=1) +
  theme_ipsum() +
  theme(plot.background = element_rect(fill = "#f4efe1", color = NA))  +
  labs(x = "",y = "",
       title = "Caribou Location Tracking",
       subtitle = "",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)",
       x = "",
       y = "") + theme(plot.title = element_text(hjust = 0.5,family="Calibri"),
                       legend.title =element_text(hjust = 0.5,family="Calibri") )



