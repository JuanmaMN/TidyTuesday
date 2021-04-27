
# Upload packages ---------------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)



# Upload raw data ---------------------------------------------------------

departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')



# Prepare the data --------------------------------------------------------


data<-departures%>%mutate(month= substr(leftofc, start = 6, stop = 7))%>%group_by(month)%>%summarize(n=n()) %>% na.omit() %>%
  ungroup()%>% mutate(month = recode(month, "01" = "Jan", 
                      "02" = "Feb", 
                      "03" = "Mar", 
                      "04" = "Apr", 
                      "05" = "May", 
                      "06" = "Jun", 
                      "07" = "Jul", 
                      "08" = "Aug", 
                      "09" = "Sep", 
                      "10" = "Oct", 
                      "11" = "Nov", 
                      "12" = "Dec"
       ),
       month = fct_relevel(month,
                           c("Jan", "Feb", "Mar","Apr", "May","Jun",
                             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))


# Font --------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Quicksand")

font_add_google("Work Sans")

font_labels <- "Quicksand"

font_labels2 <- "Work Sans"

showtext_auto()

# Graph -------------------------------------------------------------------


grob_comment <- grobTree(richtext_grob(
  sprintf("<b style='color:%s'>January, December and May</b> <br> <br> Months with the highest CEO departures","#f7a188"),  
  x=.8,y=.6, hjust=0.5,  gp=gpar(col = "#a1a1a1", fontsize=16), vjust = 1))





g2<- ggplot(data, aes(x=reorder(month,n), y=n, fill = month)) +
  geom_bar(stat="identity",width=0.4) +
  coord_flip(ylim=c(350,1000)) +
  scale_fill_manual(values = c( "Jan" = "#f7a188",
                                "Feb" = "#c3d6d0", 
                                "Mar" = "#c3d6d0",
                                "Apr" = "#c3d6d0",
                                "May" = "#f7a188",
                                "Jun" = "#c3d6d0",
                                "Jul" = "#c3d6d0", 
                                "Aug" = "#c3d6d0", 
                                "Sep"  = "#c3d6d0",
                                "Oct" = "#c3d6d0", 
                                "Nov" = "#c3d6d0",
                                "Dec" = "#f7a188")) +
  theme_ipsum() +
  labs(y = "",
       x = "",
       title = "Which months have the highest CEO departures?",
       subtitle =  "CEO turnover and dismissal in S&P 1500 firms between 2000 and 2018",
       caption =  "Source: TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")+
  guides(fill = NULL) +
  theme(plot.title = element_text(color = "#22222b",face = "bold",size = 16, hjust = 0,family = font_labels),
    plot.subtitle = element_text(margin = margin(t=5, b= 30),color = "#22222b", size = 10, family = font_labels,hjust = 0),
    plot.caption =  element_text(margin = margin(t = 50),  color = "#22222b", size = 10,hjust = 0.95,family = font_labels),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        axis.text.x    = element_blank(),
        #axis.text.y    = element_text(face = "italic",size = 8, color = "#22222b"),
        axis.text.y    = element_text(family = font_labels,size = 12, color = "#808080",margin = margin(t = 0, r = 5, b = 0, l = 5)),        
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
        plot.margin = unit(c(1, 1, 1, 2), "cm"),
        axis.ticks = element_blank()
  ) + geom_text(aes(label=n),hjust=-0.5,color = "#22222b", size = 3.5, fontface = "italic",family = font_labels) +  annotation_custom(grob_comment)  
