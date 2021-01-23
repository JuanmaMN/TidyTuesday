
# Packages ----------------------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, ggtext, hrbrthemes, gridExtra,patchwork, systemfonts, sysfonts,showtext, ggpubr,
               extrafont, systemfonts,circlepackeR,packcircles,viridis, ggflags, janitor,readr)


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Lora")

font_labels <- "Lora"

showtext_auto()



# Data sources ------------------------------------------------------------


# Kenya is a joined of the three data sets (gender,crops)
Kenya <- read_csv("Kenya.csv")


View(Kenya_1)
# First graph -------------------------------------------------------------

Kenya_1 <- Kenya %>% clean_names() %>% select(2,11:19) %>% filter (county == "Kenya")%>%
  pivot_longer(-county,
               names_to = c("Metric"),
               values_to = "total"
  )  %>%
  mutate (Metric = case_when(
    Metric %in% c("tea", "coffee")  ~ "Tea & Coffee",
    Metric %in% c("cashew_nut", "coconut")  ~ "Cashew & Coconut",
    Metric %in% c("khat_miraa", "macadamia","citrus")  ~ "Khat & Macadamia & Citrus",
    TRUE ~ Metric)) %>% group_by(Metric) %>% summarize(total=sum(total)) %>%  mutate (ftotal=total/sum(total),
                                                                                      pcnt=round(ftotal*100, 1), 
                                                                                      yaxismax=cumsum(ftotal), 
                                                                                      yaxismin = c(0, head(yaxismax, n=-1)),
                                                                                      label_position = (yaxismax+yaxismin)/2) 


graph_Kenya_1<-ggplot(Kenya_1, aes(ymax=yaxismax, ymin=yaxismin, xmax=4, xmin=3, fill= factor(Metric))) +
  geom_rect(show.legend=T, alpha=0.5) + 
  coord_polar(theta="y") +
  xlim(c(1, 5))+ 
  theme_void()+
  expand_limits(x = 0, y = 0) +
  scale_fill_manual(values = c( "Tea & Coffee" = "#38818c",
                                "farming" = "#c8a774",
                                "mango"= "#aaa4b0",
                                "avocado"= "#a0c4a9",
                                "Cashew & Coconut"= "#c27c7c",
                                "Khat & Macadamia & Citrus"  = "#734b5e")) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        legend.margin=margin(b = 2, unit='cm'),
        legend.text=element_text(size=8),
        strip.background = element_rect(fill = "#f4efe1"),
        strip.text.x = element_blank(),
        legend.key.size = unit(0.3, "cm"))  



# Second graph ------------------------------------------------------------

View(test_nationality)
test_nationality <- Kenya %>% clean_names() %>% select(2,11:19) %>% filter (county == "Kenya")%>%
  pivot_longer(-county,
               names_to = c("Metric"),
               values_to = "total") %>%
  mutate(Metric = recode(Metric,  
                         "avocado" = "Avocado", 
                         "mango" = "Mango", 
                         "tea" = "Tea", 
                         "coffee" = "Coffee", 
                         "macadamia" = "Macadamia", 
                         "citrus" = "Citrus", 
                         "khat_miraa" = "Khat (Miraa)", 
                         "coconut" = "Coconut", 
                         "cashew_nut" = "Cashew Nut"),
         ftotal=total/sum(total),
         pcnt=round(ftotal*100, 1))%>%  arrange(desc(pcnt)) %>%
  mutate(x_axis = rep(5, each = 9, length = n()),
         y_axis = c(-7,-8,-9,-7,-8,-9,-7,-8,-9),
         x_axis_2 = rep(4.95, each = 9, length = n()), 
         y_axis_2 = y_axis,
         facet  = c(1,1,1,2,2,2,3,3,3))


graph_Kenya_2<-test_nationality %>%ggplot() +
  geom_text(aes(x = x_axis, y = y_axis, label = str_wrap(paste0(Metric," ", "-", " ",pcnt,"%"))), hjust = 0, 
            color = "#525252",  size = 4, 
            family = font_labels) +
  #geom_point(aes(x = x_axis_2, y = y_axis_2, colour= Metric, size = pcnt), hjust = -2, alpha=0.5) +
  geom_point(aes(x = x_axis_2, y = y_axis_2, colour= Metric), hjust = -2, size = 10, alpha=0.5) +
  scale_colour_manual(values = c( "Tea" = "#38818c",
                                  "Coffee" = "#38818c",
                                  "Mango"= "#aaa4b0",
                                  "Avocado"= "#a0c4a9",
                                  "Cashew Nut"= "#c27c7c",
                                  "Coconut"= "#c27c7c",
                                  "Khat (Miraa)"  = "#734b5e",
                                  "Macadamia"  = "#734b5e",
                                  "Citrus"  = "#734b5e")) +
  scale_x_continuous(limits = c(4.85,5.5)) +
  scale_y_continuous(limits = c(-11,-5)) +
  facet_wrap(vars(facet)) +
  theme(plot.title = element_text(margin = margin(t=10, b = 10), 
                                  color = "#525252", size = 10, family = font_labels,
                                  face = "bold",
                                  hjust = 0.6),
        legend.position = "none",
        axis.text.x    = element_blank(),
        axis.text.y    = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.major.y =  element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
        plot.margin = unit(c(1, 2, 2, 1), "cm"),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank()
  ) 



# Both graphs -------------------------------------------------------------


ggarrange(graph_Kenya_1,graph_Kenya_2, ncol=2, nrow=1, common.legend = FALSE)+
  theme_ipsum() +
  labs(x = "",y = "",
       title = "Distribution of Households Growing Permanent Crops",
       subtitle = "",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(t=20,b = 8), 
                              color = "#e6d492",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10, b = 25), 
                                 color = "#525252", size = 10, family = font_labels,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                 color = "#808080", size = 8, family = font_labels,
                                 hjust = 0.95),
    plot.background = element_rect(fill = "#f7f7f7", color = NA))  

