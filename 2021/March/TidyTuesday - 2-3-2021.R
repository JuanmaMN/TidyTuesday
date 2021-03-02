
# Upload data -------------------------------------------------------------

youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')


# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext)



# Prepare the data --------------------------------------------------------


youtube_total_count_per_brand_scatter_plot2<-youtube%>%group_by(brand,year)%>%  mutate_at(vars(view_count,like_count,dislike_count), replace_na, 0) %>%
  summarize(total_views=sum(view_count),
            total_likes=sum(like_count),
            total_dislikes=sum(dislike_count)) %>% 
  mutate(
    total_views_2=total_views/1000000,
    total_views_2=paste0(round(total_views_2,1), " ","m"),
    color = case_when(
      total_views < 2500000   ~ "Less_than_5M",
      total_views > 2500000 & total_views < 5000000   ~ "Between_2.5M_more_than_5M",
      total_views > 5000000 & total_views < 10000000   ~ "Between_5M_more_than_10M",
      total_views >= 10000000    ~ "Higher_10M",
      TRUE ~ "Others"
    ),
    label4 = case_when(
      color == "Between_2.5M_more_than_5M" ~ total_views_2,
      color == "Between_5M_more_than_10M" ~ total_views_2,
      color == "Higher_10M"  ~ total_views_2,
      TRUE ~ ""))


youtube_total_count_per_brand_scatter_plot2$label5<-youtube_total_count_per_brand_scatter_plot2$label4
youtube_total_count_per_brand_scatter_plot2$label4 <- recode(youtube_total_count_per_brand_scatter_plot2$label4, "176.5 m" = "")


youtube_total_count_per_brand_scatter_plot2$label5<-recode(youtube_total_count_per_brand_scatter_plot2$label5, "8 m" = "")
youtube_total_count_per_brand_scatter_plot2$label5<-recode(youtube_total_count_per_brand_scatter_plot2$label5, "7.7 m" = "")
youtube_total_count_per_brand_scatter_plot2$label5<-recode(youtube_total_count_per_brand_scatter_plot2$label5, "6.4 m" = "")
youtube_total_count_per_brand_scatter_plot2$label5<-recode(youtube_total_count_per_brand_scatter_plot2$label5, "4.9 m" = "")
youtube_total_count_per_brand_scatter_plot2$label5<-recode(youtube_total_count_per_brand_scatter_plot2$label5, "3.7 m" = "")
youtube_total_count_per_brand_scatter_plot2$label5<-recode(youtube_total_count_per_brand_scatter_plot2$label5, "3.6 m" = "")
youtube_total_count_per_brand_scatter_plot2$label5<-recode(youtube_total_count_per_brand_scatter_plot2$label5, "3.5 m" = "")
youtube_total_count_per_brand_scatter_plot2$label5<-recode(youtube_total_count_per_brand_scatter_plot2$label5, "3.1 m" = "")
youtube_total_count_per_brand_scatter_plot2$label5<-recode(youtube_total_count_per_brand_scatter_plot2$label5, "28.8 m" = "")
youtube_total_count_per_brand_scatter_plot2$label5<-recode(youtube_total_count_per_brand_scatter_plot2$label5, "26.7 m" = "")
youtube_total_count_per_brand_scatter_plot2$label5<-recode(youtube_total_count_per_brand_scatter_plot2$label5, "22.9 m" = "")



youtube_total_count_per_brand_scatter_plot2<-youtube_total_count_per_brand_scatter_plot2 %>% group_by(brand) %>% 
  mutate(total_views_brand=sum(total_views),
         total_views_brand=total_views_brand/1000000,
         total_views_brand=paste0(round(total_views_brand,1)," ", "millions"),
         brand = paste0(brand, " ", "-", " ", total_views_brand))




# Graph -------------------------------------------------------------------

first_graph <-youtube_total_count_per_brand_scatter_plot2 %>% 
  ggplot(aes(year, brand, size = total_views,fill=color)) +
  geom_point(aes(fill=color),  shape = 21,stroke = .005) +
  scale_y_discrete(expand = c(-0.5, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(2000, 2020,1)) +
  scale_size(range = c(2,15),guide="none") +
  scale_fill_manual(values = c( "Less_than_5M" = "#fcfcfc",
                                "Between_2.5M_more_than_5M" = "#3b9ab2",
                                "Between_5M_more_than_10M" = "#a1cfbe",
                                "Higher_10M" = "#e3d18f")) +  
  coord_cartesian(clip = "off") +
  labs(x = "",y = "",
       title = "",
       subtitle = "",
       caption = "") + 
  theme( axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.x    = element_text(size = 12, color = "#828282",margin = margin(t = 30, r = 0, b = 0, l = 0),family = font_labels),
    axis.text.y    = element_text(size = 12, color = "#828282",margin = margin(t = 0, r = 30, b = 0, l = 0),family = font_labels),
    panel.grid.major.y = element_line(color = "#f1f1f1"),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_line(color = "#f1f1f1"),
    panel.grid.minor.x = element_blank(), 
    panel.background = element_rect(fill = "#f7f7f7", color = NA), 
    panel.grid.major = element_line(color = "#f1f1f1"),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(4, 4, 4, 4), "cm"),
    legend.position = "none",
    axis.ticks = element_blank())  +
  geom_text(aes(year,brand, label = label4, color = color), youtube_total_count_per_brand_scatter_plot2, 
            size = 4, vjust = -1.1,family = font_labels, fontface = "bold") +
  geom_text(aes(year,brand, label = label5, color = color), youtube_total_count_per_brand_scatter_plot2, 
            size = 4, vjust = -2.1,family = font_labels, fontface = "bold") +
  scale_color_manual(values = c( "Less_than_5M" = "#fcfcfc",
                                 "Between_2.5M_more_than_5M" = "#3b9ab2",
                                 "Between_5M_more_than_10M" = "#a1cfbe",
                                 "Higher_10M" = "#e3d18f")) 



super_bowl <- ggdraw() +
  draw_plot(first_graph, 0, 0, 1, 1) +
  draw_label("Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)", 
             color = "#b3b3b3", size = 10, hjust = 1, angle = 0, x = 0.93, y = 0.05, fontfamily = font_labels) +
  draw_label("Superbowl commercials", hjust = 0.5,
             color = "#22222b", size = 20, angle = 0, x =0.5, y = 0.96,fontfamily = font_labels) +
  draw_label("Total Youtube view counts by brand ",hjust = 0.5,
             color = "#22222b", size = 12, angle = 0, x =0.5, y = 0.92,fontfamily = font_labels) 




