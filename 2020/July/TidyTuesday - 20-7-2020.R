
# Upload packages ---------------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes,patchwork,sf,ozmaps,
               hrbrthemes,scales,ggtext, ggpubr)



# Raw data ----------------------------------------------------------------

animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')

names(animal_outcomes)[4]<-"Australian Capital Territory"
names(animal_outcomes)[5]<-"New South Wales"
names(animal_outcomes)[6]<-"Northern Territory"
names(animal_outcomes)[7]<-"Queensland"
names(animal_outcomes)[8]<-"South Australia"
names(animal_outcomes)[9]<-"Tasmania"
names(animal_outcomes)[10]<-"Victoria"
names(animal_outcomes)[11]<-"Western Australia"


# Prepare the data --------------------------------------------------------


animal_outcomes2<-animal_outcomes%>% 
  pivot_longer(cols = 4:11, names_to = "Region", values_to = "value") %>%group_by(Region)%>% summarize(value=sum(value,na.rm=TRUE))  


sf_oz <- ozmap_data("states")

tibble::as_tibble(sf_oz)

data_join222<-animal_outcomes2 %>% left_join(sf_oz, by= c("Region" = "NAME"))

data_join222<-st_as_sf(data_join222) 


# Graph -------------------------------------------------------------------

ggplotaus<-ggplot() + 
  geom_sf(data=data_join222,mapping = aes(fill = value), show.legend = TRUE) + 
  coord_sf() +
  guides(fill = NULL) +
  scale_fill_gradient(low = "#ececc2", high = "#20b2aa") +
  theme(
    axis.title.x =  element_blank(),
    axis.title.y =  element_blank(),
    legend.position = "none",
    axis.text.x    =  element_blank(),
    axis.text.y    =  element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),
    plot.margin = unit(c(0, 2.8, 0, 0), "cm"),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) + geom_sf_text(data=data_join222,aes(label = Region))



# Bar chart ---------------------------------------------------------------

data_join223<-data_join222%>%select(1,2)

View(data_join223)
data_join223_second_graph <- data_join223 %>% 
  ggplot(aes(x = fct_reorder(Region,value), y = value,fill = value)) +
  geom_bar(stat="identity", width=0.8) +
  geom_text(aes(label = comma_format()(value)), color = "#22292F",hjust=-0.2, size = 3.5) +
  coord_flip()+
  scale_fill_gradient(low = "#ececc2", high = "#20b2aa")+
  scale_y_continuous(limit = c(0, 1000000), expand=c(0,1)) +
  guides(fill = NULL) +
  theme(
    axis.title.x =  element_blank(),
    axis.title.y =  element_blank(),
    legend.position = "none",
    axis.text.x    =  element_blank(),
    #axis.text.y    =  element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),
    plot.margin = unit(c(2, 2.8, 1, 1), "cm"),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) 




# Patchwork ---------------------------------------------------------------


ggarrange(ggplotaus,data_join223_second_graph, ncol=2, nrow=1, common.legend = TRUE, legend="none") +
  theme_ipsum() +
  labs(x = "",y = "",
       title = "Australian Pets",
       subtitle = "Total number by region considering all animal types",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 14,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=10, b = 25), 
                                 color = "#000000", size = 10, family = "Arial",
                                 hjust = 0.5),
    plot.background = element_rect(fill = "#f7f7f7", color = NA))  
