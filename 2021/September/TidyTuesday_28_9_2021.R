
# Data sources ------------------------------------------------------------

papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, scales, ggtext, ggpubr,ggbump,wesanderson,
               grid, gridtext, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm,stringi)


# Upload name data --------------------------------------------------------

library(genderdata)
#data(package = "genderdata")

ssa_national_join<- ssa_national %>% filter (year >= 1940 & year <= 1995) %>%
  mutate(gender = case_when (	
  female >= 1 ~ "Female",
  male >=1 ~ "Male",
  TRUE ~ "Others")) %>%
  select(name, gender)%>%
  distinct(name, gender)

ssa_national_join$name = as.character(ssa_national_join$name)

# Data preparation --------------------------------------------------------

authors2 <- authors%>%  mutate(first_name = stri_extract_first(name, regex="\\w+"),
                               first_name = str_to_lower(first_name))
                               
authors_join <- authors2%>% left_join(ssa_national_join, by= c("first_name" = "name")) %>%select(author, first_name, gender) %>% filter (!is.na(gender))


authors_join2<- authors_join%>% left_join(paper_authors, by = "author")%>%
  left_join(paper_programs, by = "paper")%>%
  left_join(programs, by = "program") %>%
  na.omit()

authors_join2_female <- authors_join2 %>% filter(gender == "Female")

authors_join2_female_group<-authors_join2_female %>% group_by(program_desc) %>%  summarize(n=n())

authors_join2_female_2<- authors_join2_female_group%>% left_join(programs, by = "program_desc")


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Quicksand")

font_add_google("Work Sans")

font_labels <- "Quicksand"

font_labels2 <- "Work Sans"

showtext_auto()



# Graph -------------------------------------------------------------------

Author_graph <- authors_join2_female_2 %>% ggplot(aes(x= fct_reorder(program_desc,n), y = n, fill = program_category)) +
  geom_bar(stat = "identity", position = "identity", width = 0.4) + 
  scale_fill_manual(values = c("Finance" = "#df5252",
                               "Micro" = "#81B29A",
                               "Macro/International" = "#41658A")) +
  coord_flip() +
  labs(y = "",
       x = "",
       caption ="Source: #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")  +
  theme(plot.caption =  element_text(margin = margin(t = 50), 
                                     color = "#22222b", size = 10,
                                     hjust = 0.5,
                                     family = font_labels),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x    = element_blank(), 
        axis.text.y    = element_text(margin = margin(t = 10),family = font_labels,  color = "#525252"),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border,
        axis.ticks = element_blank(),
        legend.position = "none") +
  geom_segment(aes(x=0.8,xend=20.2,y=3000,yend=3000),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=0.8,xend=20.2,y=6000,yend=6000),linetype="dotted",colour = "#525252") +   
  geom_segment(aes(x=0.8,xend=5,y=9000,yend=9000),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=13,xend=20.2,y=9000,yend=9000),linetype="dotted",colour = "#525252") +
  annotate("text",x = 0.5,  y = 3000, label = "3000", colour = "#525252",  vjust = 0.5, size =2.5, family = font_labels) +
  annotate("text",x = 0.5,  y = 6000, label = "6000", colour = "#525252",  vjust = 0.5, size =2.5, family = font_labels) +
  annotate("text",x = 0.5,  y = 9000, label = "9000", colour = "#525252",  vjust = 0.5, size =2.5, family = font_labels) +
  annotate("text",x = 10,  y = 8700, label = "NBER working papers \nauthored by females \nby research program", colour = "#525252",  
           vjust = 0.5, size =10, family = font_labels,fontface = "bold") +
  annotate("text",x = 6,  y = 6800, label = "Finance", colour = "#df5252",  vjust = 0.5, size =8, family = font_labels, fontface = "bold") +
  annotate("text",x = 6,  y =8000, label = "Micro", colour = "#81B29A",  vjust = 0.5, size =8, family = font_labels,fontface = "bold") +
  annotate("text",x = 6,  y = 10000, label = "Macro/International", colour = "#41658A",  vjust = 0.5, size =8, family = font_labels,fontface = "bold") 


Author_graph

