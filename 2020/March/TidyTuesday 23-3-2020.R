
# Upload the data ---------------------------------------------------------

tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')


# Upload the packages -----------------------------------------------------

library(tidyverse)
library(ggchicklet)
library(scales)
library(ggthemes)

# Prepare the data --------------------------------------------------------

line_graph_year<-tbi_year%>% filter (injury_mechanism != "Total" & type =="Deaths") %>% 
  mutate (injury_mechanism = case_when(injury_mechanism %in% c("Other unintentional injury, mechanism unspecified",
                                                               "Other or no mechanism specified",
                                                               "Unintentionally struck by or against an object") ~ "Other",
                                       TRUE ~ injury_mechanism))%>%
  group_by(year,injury_mechanism)%>% summarize(total=sum(number_est))





# Graph -------------------------------------------------------------------

injury_mechanism_g<-ggplot(line_graph_year, aes(x = year, y = total, fill = injury_mechanism)) +
  geom_chicklet() +
  coord_flip() +
  scale_x_continuous(
    breaks = c(2006:2014), 
    #limits = c(2000, 2025),
    expand = c(0, 0)) + 
 scale_y_continuous(breaks=c(0,10000,20000,30000, 40000, 50000))  +
  scale_fill_manual(values = c("#f08080","#add8e6","#90ee90","#eee8aa","#d3d3d3"))  +
  #scale_color_tableau(palette = "Tableau 10", type = "regular")+
  labs(x = "",y = "",
       title = "Traumatic Brain Injury (TBI)",
       subtitle = "Estimated observed cases in each year  - Deaths	 by Injury mechanism",
       caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)")+
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 14,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#22222b", size = 9, family = "Arial",
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10, family = "Arial",
                                 hjust = 0.95),
    axis.title.x = element_text(margin = margin(t = 10),
                                color = "#22222b"),
    axis.title.y = element_text(margin = margin(r = 15), 
                                color = "#22222b"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(fill="#f7f7f7"),
    #axis.text.x    = element_text(color = "#22222b"),
    #axis.text.y    = element_text(color = "#22222b"),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7"),
    plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank()
  )  +
  annotate("text", x = 2014, y = 57800,hjust = 0, label = "56,800",size = 3.5,
           color = "#22222b")+
  annotate("text", x = 2013, y = 57800,hjust = 0, label = "55,921",size = 3.5,
           color = "#22222b")+
  annotate("text", x = 2012, y = 57800,hjust = 0, label = "55,382",size = 3.5,
           color = "#22222b")+
  annotate("text", x = 2011, y = 57800,hjust = 0, label = "53,837",size = 3.5,
           color = "#22222b")+
  annotate("text", x = 2010, y = 57800,hjust = 0, label = "52,837",size = 3.5,
           color = "#22222b")+
  annotate("text", x = 2009, y = 57800,hjust = 0, label = "52,667",size = 3.5,
           color = "#22222b")+
  annotate("text", x = 2008, y = 57800,hjust = 0, label = "53,825",size = 3.5,
           color = "#22222b")+
  annotate("text", x = 2007, y = 57800,hjust = 0, label = "54,699",size = 3.5,
           color = "#22222b")+
  annotate("text", x = 2006, y = 57800,hjust = 0, label = "54,433",size = 3.5,
           color = "#22222b")

injury_mechanism_g



