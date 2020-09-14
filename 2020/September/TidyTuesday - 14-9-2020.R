
# Upload packages ---------------------------------------------------------


pacman::p_load(tidyverse,lubridate, tidyverse, ggplot2, hrbrthemes,patchwork,ggimage,ggdraw, maps, viridis,
               biscale, cowplot, grid, gridtext,hrbrthemes,scales,ggtext, ggpubr,choroplethr,choroplethrMaps,choroplethrZip,mapproj,usmap)



# Upload data -------------------------------------------------------------

kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')


# Prepare the data --------------------------------------------------------

names(kids)[1]<-"region"

kids$region <- tolower(kids$region)

kids <- kids %>% filter(
  variable %in% c("edsubs", "edservs") &  year == "2016")

kids<-kids%>% select(1,2,6) %>%  
  pivot_wider(names_from = variable, values_from = inf_adj_perchild)%>%
  group_by(region)%>%
  summarize(
    edsubs = median(edsubs),
    edservs = median(edservs)
  ) %>% 
  bi_class(x = edsubs, y = edservs, style = "quantile", dim = 3)



# Join with States data set -----------------------------------------------


names(kids)[1]<-"full"

statepop$full <- tolower(statepop$full)

kids_map_join<- kids%>%left_join(statepop, by= "full") 



# Plot --------------------------------------------------------------------

p2<-plot_usmap(data = kids_map_join, values = "bi_class", labels = TRUE, label_color = "white")   +
  bi_scale_fill(pal = "DkCyan", dim = 3, guide = F) +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#ffffff",face = "bold",size = 9,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#ffffff", size = 6, family = "Arial",
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#ffffff", size = 5, family = "Arial",
                                 hjust = 0.95),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f3f3f3", color = NA),
    panel.border = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  ) 



legend_US<- 
  bi_legend(pal = "DkCyan",
            dim = 3,
            xlab = "Spend subsidies",
            ylab = "Spend Special Services",
            size = 5) +
  theme(rect = element_rect(fill = "grey10"),
        panel.border = element_blank(),
        axis.text = element_blank(),
        plot.background = element_rect(fill = "#f3f3f3", color = NA),
        axis.title.x = element_text(size = 10,
                                    color = "#a1a1a1",
                                    face = "bold"),
        axis.title.y = element_text(size = 10,
                                    color = "#a1a1a1",
                                    face = "bold"),
        legend.text = element_text(size = 5),
        legend.text.align = 0)




grob_comment  <- grobTree(richtext_grob(
  sprintf("<b style='color:%s'>States with the highest public spending per child in 2016 on</b><br><br> 1.Education subsidies, including tuition and scholarships. <br><br>  2. Education special services.<br><br>
          Delaware<br>Louisiana<br> Massachusetts<br>New Jersey<br>Rhode Island<br> Vermont<br> Washington <br> West Virginia<br>","#2a5a5b"),  
  x=.87,y=.6, hjust=0.5, gp=gpar(col = "#a1a1a1", fontsize=10), vjust = 1))


map_legend_US_p2<- ggdraw() +
  draw_plot(p2, 0, 0, 1, 1) +
  draw_plot(legend_US, 0, 0.1, 0.2, 0.2) +
  draw_label("Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)", 
             color = "#a1a1a1", size = 7.5, angle = 0, x = 0.9, y = 0.05) +
  draw_label("What states spent the most on education per child in 2016?", 
             color = "#000000", size = 14, angle = 0, x =0.5, y = 0.93, fontface = "bold") +
  theme(plot.background = element_rect(fill = "#f3f3f3", color = NA)) +
  annotation_custom(grob_comment)




