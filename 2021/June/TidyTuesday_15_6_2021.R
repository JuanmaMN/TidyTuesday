# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,sf,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm, 
               ggalt)


# Upload data -------------------------------------------------------------

tweets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-15/tweets.csv')

# Prepare the data --------------------------------------------------------

tweets<-tweets%>%mutate(Day=weekdays(as.Date(datetime))) %>%
  group_by(Day)%>% summarize (retweets = sum(retweet_count),
                              likes = sum(like_count)) %>%
  pivot_longer(2:3,names_to = "type", values_to = "amount") %>% na.omit()

tweets$Day<- fct_relevel(tweets$Day, c( "Monday", "Tuesday","Wednesday",  "Thursday",  "Friday",  "Saturday",  "Sunday"))

# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Quicksand")

font_labels <- "Quicksand"

font_add_google("Crimson Text")

font_labels2 <- "Crimson Text"


showtext_auto()

# Graph -------------------------------------------------------------------

gDaytweets<- ggplot(tweets, aes(x=Day, y=amount, fill = type)) +
  geom_bar(stat="identity",  width=0.4, position=position_dodge(width = 0.5)) +
  scale_y_continuous(breaks = seq(0, 3000,by =500),limits=c(0, 3000), labels = c(" ","500","1,000","1,500","2,000", "2,500", ""),
                     expand = c(.007, .007))+
  scale_fill_manual(values = c("#8fb9c9", "#c0d8c5")) +
  theme_ipsum_rc() +
  labs(y = "",
       title = "#DuBoisChallenge tweets' analysis for #TidyTuesday",
       subtitle =  "Total retweets and likes by day of the week when the tweet was published",
       caption =  "Source: Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")  +
  theme(
    plot.title = element_text(margin = margin(b = 5), 
                              color = "#000000",face = "bold",size = 18,
                              hjust = 0,
                              family = font_labels2),
    plot.subtitle = element_text(margin = margin(t=10, b = 5), 
                                 color = "#000000", size = 16, family = font_labels2,
                                 hjust = 0),
    plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                 color = "#000000", size = 10, family = font_labels,
                                 hjust = 0.95),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.y    = element_text(color = "#525252", size = 10, family = font_labels,margin = margin(r = 0)),
    axis.text.x = element_text(color = "#525252", size = 10, family = font_labels,margin = margin(r = 0)),  
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(2, 3, 2, 3), "cm"),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border,
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.text=element_text(size=8, color = "#525252",family = font_labels),
    legend.key.size = unit(0.2, "cm"),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.background=element_blank(),
    legend.margin=margin(t=1, b = 0.1, unit='cm')) +
  guides(fill = guide_legend(
    label.position = "bottom",
    nrow = 1,
    family = font_labels, 
    color = "#525252",
    keywidth = 3, keyheight = 0.5)) +
  geom_segment(aes(x=0.5,xend=7.23,y=0,yend=0),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=0.5,xend=7.5,y=500,yend=500),linetype="dotted",colour = "#525252")+
  geom_segment(aes(x=0.5,xend=7.5,y=1000,yend=1000),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=0.5,xend=7.5,y=1500,yend=1500),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=0.5,xend=7.5,y=2000,yend=2000),linetype="dotted",colour = "#525252")  +
  geom_segment(aes(x=0.5,xend=7.5,y=2500,yend=2500),linetype="dotted",colour = "#525252") 



gDaytweets