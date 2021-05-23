
# Upload the data ---------------------------------------------------------

survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,sf,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)



survey<-survey%>%mutate(industry = recode(industry, "Biomedical research" = "Biomedical Research", 
                  "Biotech/pharma" = "Biotech/Pharma",
                  "Biotech/pharmaceuticals" = "Biotech/Pharma",
                  "Biotech/Pharmaceuticals" = "Biotech/Pharma",
                  "Pharma/biotechnology" = "Biotech/Pharma",
                  "Museum" = "Museums",
                  "Pharmaceuticals / Biotech" = "Biotech/Pharma",
                  "Research & Development" = "Research and Development",
                  "Research & Development (Physical Sciences)" = "Research and Development",
                  "Pharma/biotech"  = "Biotech/Pharma",
                  "Pharma/biotech" = "Biotech/Pharma",
                  "Biotech" = "Biotech/Pharma",
                  "Pharmaceuticals / Biotech" = "Biotech/Pharma"))
                  
survey_US<-survey%>% filter(highest_level_of_education_completed == "PhD"  &  currency == "USD") %>% 
  filter (!country %in% c("England", "Mexico", "Russia", "United Kingdom","Canada","Japan", "Denmark","Singapore",
                          "\U0001f1fa\U0001f1f8","Australia","Vietnam")) %>%
  filter(gender %in% c("Woman","Man")) %>%
  filter (industry %in% c(  
    "Accounting, Banking & Finance",
    "Biomedical Research",
    "Business or Consulting",
    "Computing or Tech",
    "Education (Higher Education)",
    "Engineering or Manufacturing",
    "Government and Public Administration",
    "Health care",
    "Insurance",
    "Law",
    "Marketing, Advertising & PR",
    "Museums",
    "Nonprofits",
    "Education (Primary/Secondary)",
    "Sales")) %>%
  group_by(industry,gender) %>% summarize (avg= mean(annual_salary, na.rm=T)) %>% ungroup()%>%
  pivot_wider(names_from=gender, values_from=avg)




# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Quicksand")

font_add_google("Work Sans")

font_labels <- "Quicksand"

font_labels2 <- "Work Sans"

showtext_auto()


# Graph -------------------------------------------------------------------


ggplot(survey_US, aes(x = Man, xend = Woman, y=reorder(industry,Man))) + 
  geom_dumbbell(colour = "#e5e5e5",
                size = 3,
                colour_x = "#228b34",
                colour_xend = "#1380A1")+
  scale_x_continuous(breaks = seq(50000,250000, by = 50000), limits=c(50000, 250000), labels = c("$50k", "$100k", "$150k", "$200k", " "))+
  theme_ipsum_rc()  +
  labs(y = "",
       x = "",
       title = "Ask a Manager Survey",
       subtitle =  "Average Gender Pay gap in the US for people with a PhD",
       caption =  "Source: TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(
      color = "#22222b",face = "bold",size = 14,
      hjust = -0.1,
      family = font_labels),
    plot.subtitle = element_text(margin = margin(t=0, b= 30),
                                 color = "#22222b", size = 12, family = font_labels,
                                 hjust = -0.12),
    plot.caption =  element_text(margin = margin(t = 50), 
                                 color = "#22222b", size = 10,
                                 hjust = 0.99,
                                 family = font_labels),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.x    = element_text(color = "#525252", family = font_labels, size = 10), 
    axis.text.y    = element_text(color = "#525252",family = font_labels, size = 10),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
    #axis.ticks = element_blank(),
    legend.position = "none"
  ) +
  geom_segment(data= data, mapping=aes(x=50000,xend=225000,y=1,yend=1),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=50000,xend=225000,y=2,yend=2),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=50000,xend=225000,y=3,yend=3),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=50000,xend=225000,y=4,yend=4),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=50000,xend=225000,y=5,yend=5),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=50000,xend=225000,y=6,yend=6),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=50000,xend=225000,y=7,yend=7),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=50000,xend=225000,y=8,yend=8),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=50000,xend=225000,y=9,yend=9),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=50000,xend=225000,y=10,yend=10),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=50000,xend=225000,y=11,yend=11),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=50000,xend=225000,y=12,yend=12),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=50000,xend=225000,y=13,yend=13),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=50000,xend=225000,y=14,yend=14),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=50000,xend=225000,y=15,yend=15),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=50000,xend=50000,y=1,yend=15),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=100000,xend=100000,y=1,yend=15),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=150000,xend=150000,y=1,yend=15),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=200000,xend=200000,y=1,yend=15),linetype="dotted",colour = "#525252") +
  geom_segment(aes(x=225000,xend=225000,y=1,yend=15),linetype="dotted",colour = "#525252")  +
  geom_text(data = filter(survey_US, industry == "Accounting, Banking & Finance"),
            aes(x = Man, y = industry),
            label = "Male", fontface = "bold",
            color = "#228b34",
            vjust = -1) +
  geom_text(data = filter(survey_US, industry == "Accounting, Banking & Finance"),
            aes(x = Woman, y = industry),
            label = "Female", fontface = "bold",
            color = "#395B74",
            vjust = -1)





   



