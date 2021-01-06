
# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, ggtext, hrbrthemes,systemfonts, sysfonts,showtext, ggpubr,ggflags,ggfittext,patchwork)


# Upload data -------------------------------------------------------------


transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

data_transit<-transit_cost



# Prepare the data --------------------------------------------------------

data_transit$real_cost<-as.numeric(data_transit$real_cost)

data_transit_wrangle<-data_transit %>% group_by(country,rr)%>% 
  summarize(total_length=sum(length,na.rm=TRUE), total_cost=sum(real_cost,na.rm=TRUE),
            avg_cost_length=total_cost/total_length) %>%
  select(country, rr, avg_cost_length) %>%
  pivot_wider(names_from = rr, values_from = avg_cost_length)


names(data_transit_wrangle)[2]<-"NonRailroad"
names(data_transit_wrangle)[3]<-"Railroad"



# Font --------------------------------------------------------------------

font_add_google("Lora")

font_labels <- "Lora"

showtext_auto()


# Non-Railroad ------------------------------------------------------------


data_transit_wrangle_non_railroad<-data_transit_wrangle %>% select(1,2) %>%ungroup()%>%top_n(10) %>%
  mutate(country=tolower(country))


ggplot_non_rail<-data_transit_wrangle_non_railroad %>% ggplot(aes(x=fct_reorder(country,NonRailroad), y=NonRailroad,group = country,
                                   fill= factor(country))) +
  #geom_bar(stat="identity", fill="#69b3a2", width=0.6) +
  geom_flag(y = -50, aes(country = country), size = 8, hjust = -2) +
  #geom_text(nudge_y = 0.2, color = "darkred", size = 5) +
  scale_fill_manual(values = c( "us" = "#4e79a7",
                                "qa" = "#76B7B2",    
                                "sg"      = "#76B7B2",
                                "bd"     = "#76B7B2",
                                "hu" = "#76B7B2",
                                "eg" = "#76B7B2",
                                "ph"      = "#76B7B2",
                                "nl" = "#76B7B2",
                                "vn"= "#76B7B2",
                                "ca" = "#76B7B2"))+
  coord_flip()+
  #coord_flip(clip = "off", expand = FALSE)  +
  geom_col(width = 0.6) +
  geom_bar_text(place = "right", contrast = TRUE, size=8,
                aes(label=paste0("$",round(NonRailroad,2)," M")),
                family = font_labels) + 
  labs(x = "",y = "", title = "Non-railroad") +
  #scale_x_discrete(position = "top") +
  guides(fill = NULL) +
  ylim(-1, 2600)+
  theme(
    plot.title = element_text(margin = margin(b = 10), 
                              color = "#22222b",size = 8,
                              hjust = 0.5,
                              family = font_labels),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank()
  ) 




# Railroad ----------------------------------------------------------------

data_transit_wrangle_railroad<-data_transit_wrangle %>% select(1,3) %>%ungroup()%>%top_n(10) %>%
  mutate(country=tolower(country),
           country = gsub("uk", "gb", country))


ggplot_rail<-data_transit_wrangle_railroad %>% ggplot(aes(x=fct_reorder(country,Railroad), y=Railroad,group = country,
                                                                  fill= factor(country))) +
  #geom_bar(stat="identity", fill="#69b3a2", width=0.6) +
  geom_flag(y = -50, aes(country = country), size = 8, hjust = -2) +
  #geom_text(nudge_y = 0.2, color = "darkred", size = 5) +
  scale_fill_manual(values = c( "us" = "#4e79a7",
                                "au" = "#76B7B2",    
                                "ch"      = "#76B7B2",
                                "de"     = "#76B7B2",
                                "fr" = "#76B7B2",
                                "jp" = "#76B7B2",
                                "nz"      = "#76B7B2",
                                "tw" = "#76B7B2",
                                "gb"= "#76B7B2",
                                "ar" = "#76B7B2"))+
  coord_flip()+
  #coord_flip(clip = "off", expand = FALSE)  +
  geom_col(width = 0.6) +
  geom_bar_text(place = "right", contrast = TRUE, size=8,
                aes(label=paste0("$",round(Railroad,2)," M")))  + 
  labs(x = "",y = "", title = "Railroad") +
  #scale_x_discrete(position = "top") +
  guides(fill = NULL) +
  ylim(-1, 2600)+
  theme(
    plot.title = element_text(margin = margin(b = 10), 
                              color = "#22222b",size = 8,
                              hjust = 0.5,
                              family = font_labels),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank()
  ) 

  
  
# Patchwork ---------------------------------------------------------------
  

ggarrange(ggplot_non_rail,ggplot_rail, ncol=2, nrow=1) +
  theme_ipsum() +
  labs(x = "",y = "",
       title = "Transit Costs Project",
       subtitle = "Cost of each project km in millions of USD - Top 10 countries",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#343434",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10, b = 25), 
                                 color = "#000000", size = 10, family = font_labels,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                 color = "#808080", size = 8, family = font_labels,
                                 hjust = 0.95),
    panel.background = element_blank())


  

