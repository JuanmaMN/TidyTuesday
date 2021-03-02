

# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext)


# Upload the data ---------------------------------------------------------

earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')



# Prepare the data --------------------------------------------------------


earn2<-earn %>% filter(sex == "Women" & race == "All Races" & ethnic_origin == "All Origins" &
                  age %in% c("20 to 24 years","25 to 34 years",
                             "35 to 44 years","45 to 54 years","55 to 64 years","65 years and over"),
                  year == "2020") %>% select(age,quarter, median_weekly_earn)
                  
earn2$quarter<-as.character(earn2$quarter)


earn3<-earn2%>%pivot_wider(names_from = quarter, values_from = median_weekly_earn) 
names(earn3)[2]<-"Q1"
names(earn3)[3]<-"Q2"
names(earn3)[4]<-"Q3"
names(earn3)[5]<-"Q4"


# Additional data for points (comments) -----------------------------------

df<-data.frame(point = c(1,2,3),
               x2=c(600,600,600),
               y2=c(5.5,5,4.5))

# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Lora")

font_labels <- "Lora"

showtext_auto()



# Graph -------------------------------------------------------------------

ggplot() +
  
  geom_segment(
    data = earn2 %>% 
      group_by(age) %>% 
      top_n(-1) %>% 
      slice(1) %>%
      ungroup(),
    aes(x = 575, xend = median_weekly_earn, y = age, yend = age),
    linetype = "blank", size = 0.3, color = "gray80"
  )+
  
  geom_segment(
    data =  earn2 %>% 
      group_by(age) %>% 
      summarise(start = range(median_weekly_earn)[1], end = range(median_weekly_earn)[2]) %>% 
      ungroup(),
    aes(x = start, xend = end, y = age, yend = age),
    color = "gray80", size = 1.2
  ) +
  # reshape the data frame & plot the points
  geom_point(
    data = earn2,
    aes(median_weekly_earn, age, color = quarter), 
    size = 4
  ) +
  
  scale_color_manual(values = c( "1" = "#1A73E8",
                                  "2" = "#F9AB00",
                                  "3"= "#34A853",
                                  "4"= "#D93025")) +
  geom_text(data = filter(earn3, age== "65 years and over"),
            aes(x = Q1, y = age),
            label = "Q1", fontface = "bold",
            color = "#1A73E8",
            vjust = -2) +
  geom_text(data = filter(earn3, age== "65 years and over"),
            aes(x = Q2, y = age),
            label = "Q2", fontface = "bold",
            color = "#F9AB00",
            vjust = -2)  +
  geom_text(data = filter(earn3, age== "65 years and over"),
            aes(x = Q3, y = age),
            label = "Q3", fontface = "bold",
            color = "#34A853",
            vjust = -2) +
  geom_text(data = filter(earn3, age== "65 years and over"),
            aes(x = Q4, y = age),
            label = "Q4", fontface = "bold",
            color = "#D93025",
            vjust = -2)  + 
  scale_x_continuous(labels = scales::dollar_format(prefix="$"))+
  labs(x = "",y = "",
       title = "Employed Status",
       subtitle = "Median weekly earning in current dollars - Quarterly analysis in 2020 - Women",
       caption = "\nSource: U.S. Bureau of Labor Statistics - Tidy Tuesday\n\nVisualization: JuanmaMN (Twitter @Juanma_MN)") + 
  theme( legend.position = "none",
         plot.title = element_text(margin = margin(t=5, b = 10), 
                                   color = "#22222b",face = "bold",size = 24,
                                   hjust = 0.5,
                                   family = font_labels),
         plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                      color = "#22222b", size = 16, family = font_labels,
                                      hjust = 0.5),
         plot.caption =  element_text(margin = margin(t = 20, b = 5), 
                                      color = "#808080", size = 10, family = font_labels,
                                      hjust = 0.95),
         axis.title.x = element_blank(), 
         axis.title.y = element_blank(), 
         axis.text.x    = element_text(face = "bold",size = 12, color = "#808080",family = font_labels),
         axis.text.y    = element_text(face = "bold",size = 12, color = "#808080",margin = margin(t = 0, r = 20, b = 0, l = 0),family = font_labels),
         #panel.grid.major.y = element_blank(), 
         panel.grid.minor.y = element_blank(), 
         #panel.grid.major.x = element_blank(), 
         panel.grid.minor.x = element_blank(), 
         axis.line = element_blank(),
         panel.background = element_blank(), 
         panel.grid.major = element_line(colour = "#ebebeb"),
         #panel.grid.minor = element_blank(),
         axis.ticks = element_blank(),
         plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
         plot.margin = unit(c(1, 2, 2, 1), "cm")) +
  annotate("text", family = font_labels, x = 602, y = 5.5,  hjust = 0, size = 3.5, color = "#808080",
           label = str_wrap("Filters applied: Women, all races, all origins.")) +
  annotate("text", family = font_labels, x = 602, y = 5,  hjust = 0, size = 3.5, color = "#808080",
           label = "65 years and over: Age group with the biggest difference in earning among quarters.") +
  annotate("text", family = font_labels, x = 602, y = 4.5,  hjust = 0, size = 3.5, color = "#808080",
           label = "20 to 24 years: Age group with the smallest difference in earning among quarters.") +
  geom_point(df,mapping =  aes(x = x2, y = y2), hjust = -1, size = 2.5, alpha=0.5, colour="#808080") +
  annotate("rect", xmin = 595 , xmax = 755, ymin = 4.3, ymax = 5.6,
           alpha = .2, fill = "#d1d1d1")





