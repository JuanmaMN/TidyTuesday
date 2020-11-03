# Upload the packages -----------------------------------------------------

pacman::p_load(readxl,tidyverse,lubridate, tidyverse, ggplot2,hrbrthemes,scales,ggtext, ggpubr)


# Upload the data ---------------------------------------------------------

ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

ikea$sellable_online<-as.character(ikea$sellable_online)


# Prepare the data --------------------------------------------------------

ikea_category<-ikea%>%group_by(category,sellable_online)%>% summarize(avg=mean(price,na.rm=TRUE)) %>%
  mutate(sellable_online = recode(sellable_online, "TRUE" = "online_price", "FALSE"="offline_price")) %>%
  pivot_wider(names_from = sellable_online, values_from = avg) %>% na.omit() %>% 
  mutate(quadrant = case_when(
    offline_price >= 600 & online_price >= 1050   ~  "High-High",
    offline_price >= 600 & online_price<= 1050   ~  "High-Low",
    offline_price <= 600 & online_price >= 1050    ~  "Low-High",
    offline_price <= 600 & online_price <= 1050   ~  "Low-Low",
    T ~ as.character(category))
  )


# Font google -------------------------------------------------------------

font_text <- "Nova Mono"
font_title <- "Work Sans"
font_subtitle <- "Work Sans"


# Graph -------------------------------------------------------------------

p_ikea <- ggplot(ikea_category, aes(offline_price, online_price))  +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1200),breaks = c(300, 600, 900, 1200), name = "Average offline price") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2100),breaks = c(500, 1000, 1500, 2000),name = "Average online price") +
  geom_hline(yintercept=1050, color = "lightgrey", size=0.5) +
  geom_vline(xintercept=600, color = "lightgrey", size=0.5) +
  annotate("rect", xmin = 600, xmax = 1200, ymin = 1050, ymax = 2100, fill= "#e6d492", alpha =0.8)  + 
  annotate("rect", xmin = 0, xmax = 600, ymin = 0, ymax = 1050 , fill= "#e6d492", alpha =0.2) + 
  annotate("rect", xmin = 600, xmax = 1200, ymin = 0, ymax = 1050, fill= "#e6d492", alpha =0.4) + 
  annotate("rect", xmin = 0, xmax = 600, ymin = 1050, ymax = 2100, fill= "#e6d492", alpha = 0.4) + 
  geom_point(aes(colour = factor(quadrant)), size = 5) +
  scale_colour_manual(values = c( "High-High" = "#76B7B2",
                                  "High-Low"      = "#4e79a7",
                                  "Low-High"     = "#4e79a7",
                                  "Low-Low" = "#c27c7c")) +
  geom_text(aes(label = category), hjust = -0.2, color = "#000000",size = 3)  +
  guides(fill = NULL) +
  labs(x = "",y = "",
       title = "IKEA Furniture",
       subtitle = "Category analysis - Average online and offline price across all items",
       caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)")+
  theme(
    plot.title = element_text(margin = margin(t=0.5, b = 10), 
                              color = "#e3d18f",face = "bold",size = 18,
                              hjust = 0.5,
                              family = font_title),
    plot.subtitle = element_text(margin = margin(t=2.5,b = 20), 
                                 color = "grey50", size = 9, family = font_text,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 10), 
                                 color = "grey50", size = 8, family = font_subtitle,
                                 hjust = 0.95),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 20, l = 0), color = "grey50", size = 9, family = font_text,hjust = 0.5),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), color = "grey50", size = 9, family = font_text,hjust = 0.5),
    legend.position = "none",
    axis.text.x    = element_text(color = "grey50", size = 9, family = font_text,hjust = 0.5),
    axis.text.y    = element_text(color = "grey50", size = 9, family = font_text,hjust = 0.5),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(2, 4, 2, 4), "cm"),
    axis.ticks = element_blank()
  )



p_ikea  +  annotate(geom = "text", x = 900, y = 2000, label = "Higher price online & offline",     hjust = "middle", colour = "#8690a5", size = 5, fontface = 2) 

