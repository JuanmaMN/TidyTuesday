
# Upload the data ---------------------------------------------------------

firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')



# Load the packages -------------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext,htmltools,reactable,patchwork, markdown)



# Prepare the data --------------------------------------------------------

datafirsts<-firsts%>% group_by(category, gender)%>%
  summarize(total=n()) 

datafirsts2<-datafirsts %>%
  mutate(percentage=total/sum(total),
         pcnt=round(percentage*100, 2),
         yaxismax=cumsum(percentage), 
         yaxismin = c(0, head(yaxismax, n=-1)),
         label_position = (yaxismax+yaxismin)/2,
         label = if_else(pcnt >= 50, 
                paste0(pcnt, "%"),
                NA_character_)
                )

 

# Graph -------------------------------------------------------------------


gender_datafirsts2<-ggplot(datafirsts2, aes(ymax=yaxismax, ymin=yaxismin, xmax=4, xmin=3, fill=gender)) +
  geom_rect(alpha=0.5) + 
  geom_text(aes(label = label, x = 1, y = 0), 
            color = "#cdaa25", fontface = "bold", family = "Arial",
            size=2.8) +
  coord_polar(theta="y") +
  xlim(c(1, 5))+ 
  theme_void() + 
  scale_fill_manual(breaks=c("African-American Firsts","Female African American Firsts"),values=c("#CDAA25", "#43a07e")) +
  facet_wrap(~ category, 
             nrow = 2)  +
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
    axis.title.x =element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position="bottom",
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.ticks = element_blank()
  ) 

gender_datafirsts2



# Bar charts --------------------------------------------------------------


datafirsts4<-firsts%>% group_by(category, gender)%>%
  summarize(total=n()) 

gdatafirsts4<-datafirsts4%>% ggplot(aes(x=reorder(category,total), y=total)) +
  geom_bar(stat="identity", fill=c("#CDAA25", "#CDAA25","#CDAA25","#CDAA25","#CDAA25","#CDAA25","#CDAA25","#CDAA25",
                                   "#43a07e","#43a07e","#43a07e","#43a07e","#43a07e","#43a07e","#43a07e","#43a07e"), width=0.6,
           alpha=0.5) +
  coord_flip() +
  facet_wrap(~gender, ncol=4) + 
  geom_text(aes(label=total),hjust=-0.5,
           color = c("#CDAA25","#43a07e","#CDAA25", "#43a07e","#CDAA25","#43a07e","#CDAA25",
                     "#43a07e","#CDAA25","#43a07e","#CDAA25","#43a07e","#CDAA25","#43a07e","#CDAA25","#43a07e")) +
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
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.text.x    = element_blank(),
    axis.text.y    = element_text(color = "#22222b"),
    legend.title = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.ticks = element_blank()
  ) +
  
  ylim(0,100) +
  
  ylab("") +
  
  xlab("") 

gdatafirsts4




# Patchwork ---------------------------------------------------------------

patchworkgdatafirsts5<-cowplot::plot_grid(gdatafirsts4, gender_datafirsts2, ncol = 2,align = "v")

PW962020 <- patchworkgdatafirsts5 + plot_annotation(title = "African American Achievements",
                                       subtitle = "Total number of achievements and percentange by gender in each category",
                                       caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)",
                                       theme = theme(plot.title = element_text(margin = margin(t=15,b = 8), 
                                                                               color = "#000000",face = "bold",size = 12,
                                                                               hjust = 0.5,
                                                                               family = "Arial"),
                                                     plot.subtitle = element_text(margin = margin(t=10, b = 25), 
                                                                                  color = "#000000", size = 10, family = "Arial",
                                                                                  hjust = 0.5),
                                                     plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                                                                  color = "#000000", size = 8, family = "Arial",
                                                                                  hjust = 0.95)))



PW962020

