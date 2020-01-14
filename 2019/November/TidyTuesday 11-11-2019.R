# Upload dataset ----------------------------------------------------------

cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

# Upload necessary packages -----------------------------------------------

library(tidyverse)
library(scales)
library(ggplot2)
library(ggrepel)

Top_n<-cran_code%>%group_by(language)%>%summarize(
  total_code=sum(code),
  total_packages=n(),
  total_file=sum(file)
  )%>%top_n(50,total_file)



# Graph -------------------------------------------------------------------

ggplot(as.data.frame(Top_n), aes(x=total_packages, y=total_file)) +
  geom_point(aes(size = total_code),show.legend = FALSE, color=ifelse(Top_n$language == "R", "#e13d3d", "#2096b2")) +
  scale_x_continuous(label=comma_format()) +
  scale_y_continuous(label=comma_format()) +
  theme_minimal()+
  labs(
    title = "Analysis of different languages in all of the R packages",
    subtitle = "What are the most used programming languages used in R packages?",
    caption = "\n Source:Tidy Tuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)",
    x = "Total number of packages",
    y = "Total number of files") +
  theme(plot.margin = unit(c(1, 2, 1, 1), "cm"),
    text = element_text(family = "Arial"),
    plot.title = element_text(family = "Arial",size = 15,  face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(family = "Arial",size = 12, margin = margin(b = 25)),
    plot.caption = element_text(size = 10, margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(t = 10, b = 10,r = 20), color = "#515151", hjust = 1,size = 10, family = "Arial"),
    axis.title.x = element_text(margin = margin(t = 15, b = 15), color = "#515151", hjust = 1,size = 10, family = "Arial"),
    plot.background=element_rect(fill="#f7f7f7")) +
  annotate("rect", fill = "#f08080", alpha = .1, 
           xmin = 10200, xmax = 13000, 
           ymin = 175000, ymax = 245000) +
  annotate("text", x = 10500, y = 210000,
           hjust = 0, color = "#451225",
           size = 3.5, label = paste0("R, as expected, is the \n    first language with a total of \n      22,822,548 lines of R code \n      14,689 packages and \n      267,967 files")) +  
  geom_segment(aes(x = 13000, y = 250000, xend = 14500, yend = 267967), 
               colour = "#f08080", 
               size=0.5,
               arrow = arrow(length = unit(0.02, "npc"))) +
  geom_text_repel(family = "Arial", aes(label = language), fontface = "italic", 
                  data = subset(Top_n, language %in% c("HTML","C/C++ Header","C++","C","Markdown")),
                  point.padding = 0.15,
                  color = "#170404")

 


                                