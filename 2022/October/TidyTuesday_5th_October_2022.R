# Upload data -------------------------------------------------------------

product_hunt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-04/product_hunt.csv')



# Upload packages ---------------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)


# Prepare the data --------------------------------------------------------

product_hunt_2<- product_hunt %>%select(upvotes, category_tags) %>%
  separate_rows(category_tags, sep = ", ") %>%
  mutate(name_2 = gsub("[^[:alnum:]]", " ",category_tags)) %>%
  mutate(name_2 = trimws(name_2))%>%
  group_by(name_2) %>%
  summarize(total=n()) %>%
  mutate(percentage = total/sum(total),
         percentage=percent(percentage,accuracy = 0.01)) %>%
  top_n(10) %>%
  arrange(desc(total))

product_hunt_2_column <- product_hunt_2 %>% arrange(desc(total)) %>% select(2,1,3) %>%
  mutate(x_axis = rep(5, each = 10, length = n()),
         x_axis_2 = x_axis + 1.2,
         x_axis_3 = x_axis + 2.4,
         y_axis = rep(-1:-10, length = n()))

product_hunt_2_column$name_2<- fct_relevel(product_hunt_2_column$name_2, c("ARTIFICIAL INTELLIGENCE",
                                                                           "USER EXPERIENCE",
                                                                           "DESIGN TOOLS",
                                                                           "MARKETING",
                                                                           "ANDROID",
                                                                           "DEVELOPER TOOLS",
                                                                           "WEB APP",
                                                                           "IPHONE",
                                                                           "PRODUCTIVITY",
                                                                           "TECH"))


# Font --------------------------------------------------------------------

extrafont::loadfonts(device = "all", quiet = TRUE)
font_add_google("Montserrat")
font_labels <- "Montserrat"
showtext_auto()



# Graph -------------------------------------------------------------------

p_graph_columns<-product_hunt_2_column %>%ggplot() +
  geom_text(aes(x = x_axis, y = y_axis, label = total), hjust = 0.5, fontface = "bold", color = "#000000",  size = 3.5, 
            family = font_labels) +
  geom_text(aes(x = x_axis_2, y = y_axis, label = name_2), hjust = 0.5, fontface = "bold", color = "#525252",  size = 3, 
            family = font_labels) +
  geom_text(aes(x = x_axis_3, y = y_axis, label = percentage), hjust = 0.5, fontface = "bold", color = "#ff0000",  size = 3.5, 
            family = font_labels) +
  geom_text(aes(x = 5, y = 0.2, label = "Total"), hjust = 0.5, fontface = "plain", color = "#000000",  size = 3.5, 
            family = font_labels) +
  geom_text(aes(x = 6.2, y = 0.2, label = "Category"), hjust = 0.5, fontface = "plain", color = "#525252",  size = 3.5, 
            family = font_labels) +
  geom_text(aes(x = 7.4, y = 0.2, label = "% Products \nwith tag"), hjust = 0.5, fontface = "plain", color = "#ff0000",  size = 3.5, 
            family = font_labels) +
  scale_x_continuous(limits = c(4.5,7.9)) +
  scale_y_continuous(limits = c(-10.55,0.5)) +
  geom_segment(aes(x = 4.7, xend = 4.7, y = -10.5, yend = -0.5), color="#000000") +
  geom_segment(aes(x = 4.7, xend = 5.3, y = -10.5, yend = -10.5), color="#000000") +
  geom_segment(aes(x = 4.7, xend = 5.3, y = -9.5, yend = -9.5), color="#000000") +
  geom_segment(aes(x = 4.7, xend = 5.3, y = -8.5, yend = -8.5), color="#000000") +
  geom_segment(aes(x = 4.7, xend = 5.3, y = -7.5, yend = -7.5), color="#000000") +
  geom_segment(aes(x = 4.7, xend = 5.3, y = -6.5, yend = -6.5), color="#000000") +
  geom_segment(aes(x = 4.7, xend = 5.3, y = -5.5, yend = -5.5), color="#000000") +
  geom_segment(aes(x = 4.7, xend = 5.3, y = -4.5, yend = -4.5), color="#000000") +
  geom_segment(aes(x = 4.7, xend = 5.3, y = -3.5, yend = -3.5), color="#000000") +
  geom_segment(aes(x = 4.7, xend = 5.3, y = -2.5, yend = -2.5), color="#000000") +
  geom_segment(aes(x = 4.7, xend = 5.3, y = -1.5, yend = -1.5), color="#000000") +
  geom_segment(aes(x = 4.7, xend = 5.3, y = -0.5, yend = -0.5), color="#000000") +
  
  # Right-hand side  
  geom_segment(aes(x = 7.7, xend = 7.7, y = -10.5, yend = -0.5), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 7.1, y = -10.5, yend = -10.5), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 7.7, y = -10.5, yend = -10.5), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 7.7, y = -9.5, yend = -9.5), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 7.7, y = -8.5, yend = -8.5), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 7.7, y = -7.5, yend = -7.5), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 7.7, y = -6.5, yend = -6.5), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 7.7, y = -5.5, yend = -5.5), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 7.7, y = -4.5, yend = -4.5), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 7.7, y = -3.5, yend = -3.5), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 7.7, y = -2.5, yend = -2.5), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 7.7, y = -1.5, yend = -1.5), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 7.7, y = -0.5, yend = -0.5), color="#ff0000") +
  

# Arrows left

  geom_segment(aes(x = 5.3, xend = 5.5, y = -10.5, yend = -10), color="#000000") +
  geom_segment(aes(x = 5.3, xend = 5.5, y = -9.5, yend = -10), color="#000000") +
  geom_segment(aes(x = 5.5, xend = 5.8, y = -10, yend = -10), color="#000000") +



  geom_segment(aes(x = 5.3, xend = 5.5, y = -9.5, yend = -9), color="#000000") +
  geom_segment(aes(x = 5.3, xend = 5.5, y = -8.5, yend = -9), color="#000000") +
  geom_segment(aes(x = 5.5, xend = 5.8, y = -9, yend = -9), color="#000000") +

  
  geom_segment(aes(x = 5.3, xend = 5.5, y = -8.5, yend = -8), color="#000000") +
  geom_segment(aes(x = 5.3, xend = 5.5, y = -7.5, yend = -8), color="#000000") +
  geom_segment(aes(x = 5.5, xend = 5.8, y = -8, yend = -8), color="#000000") +
  
  
  geom_segment(aes(x = 5.3, xend = 5.5, y = -7.5, yend = -7), color="#000000") +
  geom_segment(aes(x = 5.3, xend = 5.5, y = -6.5, yend = -7), color="#000000") +
  geom_segment(aes(x = 5.5, xend = 5.8, y = -7, yend = -7), color="#000000") +
  
  geom_segment(aes(x = 5.3, xend = 5.5, y = -6.5, yend = -6), color="#000000") +
  geom_segment(aes(x = 5.3, xend = 5.5, y = -5.5, yend = -6), color="#000000") +
  geom_segment(aes(x = 5.5, xend = 5.8, y = -6, yend = -6), color="#000000") +
  
  
  geom_segment(aes(x = 5.3, xend = 5.5, y = -5.5, yend = -5), color="#000000") +
  geom_segment(aes(x = 5.3, xend = 5.5, y = -4.5, yend = -5), color="#000000") +
  geom_segment(aes(x = 5.5, xend = 5.8, y = -5, yend = -5), color="#000000") +
  
  
  geom_segment(aes(x = 5.3, xend = 5.5, y = -4.5, yend = -4), color="#000000") +
  geom_segment(aes(x = 5.3, xend = 5.5, y = -3.5, yend = -4), color="#000000") +
  geom_segment(aes(x = 5.5, xend = 5.8, y = -4, yend = -4), color="#000000") +
  
  
  geom_segment(aes(x = 5.3, xend = 5.5, y = -3.5, yend = -3), color="#000000") +
  geom_segment(aes(x = 5.3, xend = 5.5, y = -2.5, yend = -3), color="#000000") +
  geom_segment(aes(x = 5.5, xend = 5.8, y = -3, yend = -3), color="#000000") +
  
  
  geom_segment(aes(x = 5.3, xend = 5.5, y = -2.5, yend = -2), color="#000000") +
  geom_segment(aes(x = 5.3, xend = 5.5, y = -1.5, yend = -2), color="#000000") +
  geom_segment(aes(x = 5.5, xend = 5.8, y = -2, yend = -2), color="#000000") +
  
  
  geom_segment(aes(x = 5.3, xend = 5.5, y = -1.5, yend = -1), color="#000000") +
  geom_segment(aes(x = 5.3, xend = 5.5, y = -0.5, yend = -1), color="#000000") +
  geom_segment(aes(x = 5.5, xend = 5.8, y = -1, yend = -1), color="#000000") +

  
  
  geom_segment(aes(x = 7.1, xend = 6.9, y = -10.5, yend = -10), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 6.9, y = -9.5, yend = -10), color="#ff0000") +
  geom_segment(aes(x = 6.6, xend = 6.9, y = -10, yend = -10), color="#ff0000") +
  
  geom_segment(aes(x = 7.1, xend = 6.9, y = -9.5, yend = -9), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 6.9, y = -8.5, yend = -9), color="#ff0000") +
  geom_segment(aes(x = 6.6, xend = 6.9, y = -9, yend = -9), color="#ff0000") +
  
  geom_segment(aes(x = 7.1, xend = 6.9, y = -8.5, yend = -8), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 6.9, y = -7.5, yend = -8), color="#ff0000") +
  geom_segment(aes(x = 6.6, xend = 6.9, y = -8, yend = -8), color="#ff0000") +
  
  geom_segment(aes(x = 7.1, xend = 6.9, y = -7.5, yend = -7), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 6.9, y = -6.5, yend = -7), color="#ff0000") +
  geom_segment(aes(x = 6.6, xend = 6.9, y = -7, yend = -7), color="#ff0000") +
  
  geom_segment(aes(x = 7.1, xend = 6.9, y = -6.5, yend = -6), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 6.9, y = -5.5, yend = -6), color="#ff0000") +
  geom_segment(aes(x = 6.6, xend = 6.9, y = -6, yend = -6), color="#ff0000") +
  
  geom_segment(aes(x = 7.1, xend = 6.9, y = -5.5, yend = -5), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 6.9, y = -4.5, yend = -5), color="#ff0000") +
  geom_segment(aes(x = 6.6, xend = 6.9, y = -5, yend = -5), color="#ff0000") +
  
  geom_segment(aes(x = 7.1, xend = 6.9, y = -4.5, yend = -4), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 6.9, y = -3.5, yend = -4), color="#ff0000") +
  geom_segment(aes(x = 6.6, xend = 6.9, y = -4, yend = -4), color="#ff0000") +
  
  
  geom_segment(aes(x = 7.1, xend = 6.9, y = -3.5, yend = -3), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 6.9, y = -2.5, yend = -3), color="#ff0000") +
  geom_segment(aes(x = 6.6, xend = 6.9, y = -3, yend = -3), color="#ff0000") +
  
  geom_segment(aes(x = 7.1, xend = 6.9, y = -2.5, yend = -2), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 6.9, y = -1.5, yend = -2), color="#ff0000") +
  geom_segment(aes(x = 6.6, xend = 6.9, y = -2, yend = -2), color="#ff0000") +
  
  geom_segment(aes(x = 7.1, xend = 6.9, y = -1.5, yend = -1), color="#ff0000") +
  geom_segment(aes(x = 7.1, xend = 6.9, y = -0.5, yend = -1), color="#ff0000") +
  geom_segment(aes(x = 6.6, xend = 6.9, y = -1, yend = -1), color="#ff0000") +
  labs(y = "",
       x = "",
       title = "10 Most Common Category Tags",
       caption =  "Source: Components.one for #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(      plot.title = element_text(#margin = margin(b = 10), 
    color = "#000000",face = "bold",size = 20,
    hjust = 0.45,
    family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#808080", size = 12, family = font_labels,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 50), 
                                 color = "#22222b", size = 10,
                                 hjust = 0.45,
                                 family = font_labels),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
    axis.ticks = element_blank(),
    legend.position = "none")

    
    
    
p_graph_columns



