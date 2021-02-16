
# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,tidytext,extrafont,systemfonts, 
               showtext, add2ggplot)



# Upload data -------------------------------------------------------------

freed_slaves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/freed_slaves.csv')


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Lora")

font_labels <- "Lora"

showtext_auto()



# First graph -------------------------------------------------------------


freed_slaves_2<-freed_slaves%>% pivot_longer(cols = 2:3, names_to="Metric", values_to = "value") %>%
  mutate(
    label = ifelse(Metric == "Free", paste0(round(value,0), "%"), " ")
  )


g_first_graph<- ggplot(freed_slaves_2, aes(x= Year, y=value, fill = Metric, label=label)) +
  geom_bar(stat="identity", width=5) +
  geom_text(position = position_stack(vjust = 0.5), size=4,family = font_labels, color = "#ffffff") +
  scale_x_reverse(breaks = c(seq(1790, 1870, 10))) +
  coord_flip() +
  scale_fill_manual(values = c( "Free" = "#2e8b57",
                                "Slave"      = "#262626")) +
  theme_du_bois() +
  labs(x = "",y = "",
       title = "Proportion of freemen vs slaves in US by year",
       subtitle = "",
       caption = "") +
  theme(plot.title = element_text(margin = margin(t=5, b = 10), 
                                  color = "#808080",face = "bold",size = 12,
                                  hjust = 0.5,
                                  family = font_labels),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x    = element_blank(), 
        axis.text.y    = element_text(face = "bold",size = 10, color= "#808080",margin = margin(t=-15,l=10,r =0),family = font_labels),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.background=element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin=margin(b = 0.1, unit='cm'),
        legend.text=element_text(size=8,color = "#808080"),
        strip.text.x = element_blank(),
        legend.key.size = unit(0.3, "cm"))+ 
  guides(fill = guide_legend(
    label.position = "bottom",
    family = font_labels, 
    color = "#808080",
    keywidth = 3, keyheight = 0.5))



# Second graph ------------------------------------------------------------

city_rural <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/city_rural.csv')

pie_city_rural<-city_rural%>% mutate (ftotal=Population/sum(Population),
                                             pcnt=round(ftotal*100, 1), 
                                             yaxismax=cumsum(ftotal), 
                                             yaxismin = c(0, head(yaxismax, n=-1)),
                                             label_position = (yaxismax+yaxismin)/2,
                                              labs= paste0(Category, "\n(", pcnt, "%)")) 

g_second_graph <-ggplot(pie_city_rural, aes(ymax=yaxismax, ymin=yaxismin, xmax=4, xmin=3, fill= factor(Category))) +
  geom_rect(show.legend=T) + 
  coord_polar(theta="y") +
  xlim(c(1, 4))+ 
  theme_void() +
  theme_du_bois() +
  labs(x = "",y = "",
       title = "Black population split between city and rural areas in 1890",
       subtitle = "",
       caption = "") +
  expand_limits(x = 0, y = 0) +
  geom_text(x=3.5, aes(y=label_position, label=paste(pcnt,"%",sep="")), size=4.5, col="#ffffff",family = font_labels) +
  scale_fill_manual(values = c( "Cities Over 10000" = "#4d695a",
                                "Cities 5000-10000" = "#5371ab",
                                "Cities 2500-5000"= "#f2b226",
                                "Country and Villages"= "#d8273d")) +
  
  theme( plot.title = element_text(margin = margin(t=5, b = 10), 
                                   color = "#808080",face = "bold",size = 12,
                                   hjust = 0.5,
                                   family = font_labels),
         axis.title.x = element_blank(), 
         axis.title.y = element_blank(), 
         axis.text.x    = element_blank(), 
         axis.text.y    = element_blank(), 
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        legend.text=element_text(size=8, color = "#808080"),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        legend.key.size = unit(0.2, "cm"),
        legend.key = element_blank(),
        legend.background=element_blank(),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        legend.margin=margin(b = 0.1, unit='cm'),
        strip.text.x = element_blank())+ 
  guides(fill = guide_legend(
    override.aes = list(size = 2),
    label.position = "bottom",
    family = font_labels, 
    color = "#808080",
    keywidth = 2, keyheight = 0.5)) 




# Patchwork ---------------------------------------------------------------

ggarrange(g_first_graph,g_second_graph, ncol=2, nrow=1) +
  theme_du_bois()  +
  labs(x = "",y = "",
       title = "Dubois Challenge - #TidyTuesday ",
       subtitle = "",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(t = 10, b = 5), 
                              color = "#343434",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                 color = "#808080", size = 8, family = font_labels,
                                 hjust = 0.95))
