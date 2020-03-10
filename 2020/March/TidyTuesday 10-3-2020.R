

# Upload the data ---------------------------------------------------------

salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')



# Upload the packages -----------------------------------------------------

library(ggrepel)
library(tidyverse)
library(gghighlight)



# Prepare the data --------------------------------------------------------

data<-salary_potential%>%filter(!is.na(make_world_better_percent) & !is.na(stem_percent) & stem_percent>0)  

g1 <- subset(data, name == "California Institute of Technology")
g2 <- subset(data, name == "Rose-Hulman Institute of Technology")
g3 <- subset(data, name == "Colorado School of Mines")
g4 <- subset(data, name == "South Dakota School of Mines and Technology")	
g5 <- subset(data, name == "New Mexico Institute of Mining and Technology")	

f1<- subset(data, name == "Baptist Memorial College of Health Sciences")	
f2<- subset(data, name == "Rush University")	
f3<- subset(data, name == "Medical University of South Carolina")	
f4<- subset(data, name == "Adventist University of Health Sciences")	



# Graph -------------------------------------------------------------------

ggplot(data, aes(x=stem_percent, y=make_world_better_percent,label = state_name)) + 
  geom_point(color="black", fill="#dbdbdb", shape=21, alpha=0.65, size=3, stroke = 1) + 
  geom_point(data=g1, color="black", fill="#e13d3d", shape=21, alpha=0.65, size=3, stroke = 1) +
  geom_point(data=g2, color="black", fill="#e13d3d", shape=21, alpha=0.65, size=3, stroke = 1) +
  geom_point(data=g3, color="black", fill="#e13d3d", shape=21, alpha=0.65, size=3, stroke = 1)  +
  geom_point(data=g4, color="black", fill="#e13d3d", shape=21, alpha=0.65, size=3, stroke = 1)  +
  geom_point(data=g5, color="black", fill="#e13d3d", shape=21, alpha=0.65, size=3, stroke = 1)  +
  geom_point(data=f1, color="black", fill="#20b2aa", shape=21, alpha=0.65, size=3, stroke = 1)  +
  geom_point(data=f2, color="black", fill="#20b2aa", shape=21, alpha=0.65, size=3, stroke = 1)  +
  geom_point(data=f3, color="black", fill="#20b2aa", shape=21, alpha=0.65, size=3, stroke = 1)  +
  geom_point(data=f4, color="black", fill="#20b2aa", shape=21, alpha=0.65, size=3, stroke = 1)  +
  geom_text_repel(
    data=g1,
    color = "#e13d3d",
    size= 3,
    direction     = "x"
  ) +
  geom_text_repel(
    data=g2,
    color = "#e13d3d",
    size= 3,
    direction     = "x"
  )+
  geom_text_repel(
    data=g3,
    color = "#e13d3d",
    size= 3,
    direction     = "x"
  ) +
  geom_text_repel(
    data=g4,
    color = "#e13d3d",
    size= 3,
    direction     = "x"
  )+
  geom_text_repel(
    data=g5,
    color = "#e13d3d",
    size= 3,
    direction     = "x"
  )  +
  geom_text_repel(
    data=f1,
    color = "#20b2aa",
    size= 3,
    direction     = "x"
  )+
  geom_text_repel(
    data=f2,
    color = "#20b2aa",
    size= 3,
    direction     = "y",
    nudge_y=0.05
  ) +
  geom_text_repel(
    data=f3,
    color = "#20b2aa",
    size= 3,
    direction     = "x",
    hjust        = 1
  )+
  geom_text_repel(
    data=f4,
    color = "#20b2aa",
    size= 3,
    vjust        = 0
    #direction     = "both"
  ) +
  labs(x = "Percent of student body in STEM",y = "Percent of alumni who think they are making the world a better place",
       title = "College tuition, diversity, and pay",
       subtitle = "Schools and states with the highest percentage of students in STEM and that think they are making the world a better place",
       caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)")+
  guides(fill = NULL) +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 12,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#22222b", size = 9, family = "Arial",
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 8), 
                                 color = "#22222b", size = 8, family = "Arial",
                                 hjust = 0.95),
    axis.title.x = element_text(element_text(vjust=-2),
                                color = "#3d3947"),
    axis.title.y = element_text(element_text(vjust=0.5),
                                color = "#3d3947"),
    legend.position = "none",
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    panel.background = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7"),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) 






