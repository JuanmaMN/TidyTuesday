
# Upload packages ---------------------------------------------------------


pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts, showtext,ggflags,usmap)

# Upload the data ---------------------------------------------------------

Freedom_percentage <- read_excel("Freedom_percentage.xlsx") # data downloaded from the site - https://freedomhouse.org/

# Prepare the data --------------------------------------------------------

Freedom_percentage_2 <- Freedom_percentage %>% pivot_longer (2:4, names_to = "type", values_to = "percentage")

names(Freedom_percentage_2)[1]<- "year"
Freedom_percentage_2$type <- recode(Freedom_percentage_2$type,"Percent_F_Countries" = "Free")
Freedom_percentage_2$type <- recode(Freedom_percentage_2$type,"Percent_PF_Countries" = "Partly Free")
Freedom_percentage_2$type <- recode(Freedom_percentage_2$type, "Percent_of_NF_Countries" = "Not Free")


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("DM Serif Display")

font_labels <- "DM Serif Display"

showtext_auto()

font_add_google("Lora")

font_labels2 <- "Lora"

showtext_auto()


# Donut chart -------------------------------------------------------------


## 2022

Freedom_percentage_3_2022<-Freedom_percentage_2 %>% filter (year == "2022") %>%
  mutate(pcnt=round(percentage*100, 1), 
         yaxismax=cumsum(pcnt), 
         yaxismin = c(0, head(yaxismax, n=-1)),
         label_position = (yaxismax+yaxismin)/2)


gender_donut_freedom_2022<-ggplot(Freedom_percentage_3_2022, 
                                  aes(ymax=yaxismax, ymin=yaxismin, xmax=4, xmin=3, fill=type)) +
  geom_rect(show.legend=T, alpha=0.5) + 
  scale_fill_manual(values = c(  
    "Not Free" = "#6a71a7",
    "Free" = "#00aa83",
    "Partly Free" = "#e7b80a"
  )) +
  coord_polar(theta="y") +
  xlim(c(1, 5))+ 
  theme_void()+
  expand_limits(x = 0, y = 0) +
  geom_text(x=3.5, aes(y=label_position, label=paste(pcnt,"%",sep="")), size=5, col="#343434")  + 
  theme(
    plot.title = element_text(margin = margin(b = 15, t=10), 
                              color = "#22222b",face = "bold",size = 12,
                              hjust = 0.5,
                              family = font_labels),
    legend.position = c(0, 1), 
    legend.justification = c(0, 0),
    legend.direction = "horizontal",
    legend.title=element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    legend.margin=margin(b = 0.5, unit='cm'),
    legend.text=element_text(size=8,family = font_labels),
    strip.background = element_rect(fill = "#f4efe1"),
    strip.text.x = element_blank(),
    legend.key.size = unit(0.3, "cm"),
    legend.background =  element_rect(fill = "#fbfaf6", color = NA),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    panel.background = element_rect(color=NA, fill = "#fbfaf6")
  ) +  
  geom_text(aes(label="Status in 2022", x=5, y=0), color="#000000", family=font_labels, size=6) +
  guides(fill = guide_legend(
    label.position = "bottom",
    family = font_labels, 
    color = "#808080",
    keywidth = 3, keyheight = 0.5))


## 2003

Freedom_percentage_3_2003<-Freedom_percentage_2 %>% filter (year == "2003") %>%
  mutate(pcnt=round(percentage*100, 1), 
         yaxismax=cumsum(pcnt), 
         yaxismin = c(0, head(yaxismax, n=-1)),
         label_position = (yaxismax+yaxismin)/2)


gender_donut_freedom_2003<-ggplot(Freedom_percentage_3_2003, 
                                  aes(ymax=yaxismax, ymin=yaxismin, xmax=4, xmin=3, fill=type)) +
  geom_rect(show.legend=T, alpha=0.5) + 
  scale_fill_manual(values = c(  
    "Not Free" = "#6a71a7",
    "Free" = "#00aa83",
    "Partly Free" = "#e7b80a"
  )) +
  coord_polar(theta="y") +
  xlim(c(1, 5))+ 
  theme_void()+
  expand_limits(x = 0, y = 0) +
  geom_text(x=3.5, aes(y=label_position, label=paste(pcnt,"%",sep="")), size=5, col="#343434")  + 
  theme(
    plot.title = element_text(margin = margin(b = 15, t=10), 
                              color = "#22222b",face = "bold",size = 12,
                              hjust = 0.5,
                              family = font_labels),
    legend.title=element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    legend.position = c(0, 1), 
    legend.justification = c(0, 0),
    legend.direction = "horizontal",
    legend.margin=margin(b = 0.5, unit='cm'),
    legend.text=element_text(size=8,family = font_labels),
    strip.background = element_rect(fill = "#f4efe1"),
    strip.text.x = element_blank(),
    legend.key.size = unit(0.3, "cm"),
    legend.background =  element_rect(fill = "#fbfaf6", color = NA),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    panel.background = element_rect(color=NA, fill = "#fbfaf6")
  ) +  
  geom_text(aes(label="Status in 2003", x=5, y=0), color="#000000", family=font_labels, size=6) +
  guides(fill = guide_legend(
    label.position = "bottom",
    family = font_labels, 
    color = "#808080",
    keywidth = 3, keyheight = 0.5))

# Cowplot -----------------------------------------------------------------


ggarrange(gender_donut_freedom_2003,gender_donut_freedom_2022, ncol=2, nrow=1, common.legend = TRUE, legend="top") +
  theme_ipsum()   +
  labs(x = "",y = "",
       title = "Decline in number of free countries ",
       subtitle = "Analysis of freedom in the World. Number of countries under each category. Comparison between 2003 and 2022.",
       caption = "Source: https://freedomhouse.org\nVisualization: JuanmaMN (Twitter @Juanma_MN)",
       x = "",
       y = "") + theme(plot.title = element_text(margin = margin(b = 20), 
                                                 color = "#343434",face = "bold",size = 24,
                                                 hjust = 0,
                                                 family = font_labels),
                       plot.subtitle = element_text(margin = margin(b = 20), 
                                                    color = "#343434",size = 16,
                                                    hjust = 0,
                                                    family = font_labels),
                       plot.caption =  element_text(margin = margin(t = 30, b = 10), 
                                                    color = "#343434", size = 10, family = font_labels2,
                                                    hjust = 0.5),
                       plot.background = element_rect(fill = "#fbfaf6", color = NA),
                       legend.background =  element_rect(fill = "#fbfaf6", color = NA)) 

