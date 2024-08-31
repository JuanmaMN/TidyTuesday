
# Upload the data ---------------------------------------------------------

tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')


# Upload the packages -----------------------------------------------------


library(lubridate)
library(ggplot2)
library(tidyverse)
library(ggfittext)


# Prepare the data --------------------------------------------------------

tdf_winners2<-tdf_winners %>%
  mutate(
    decade=case_when(
      tdf_winners$start_date < "2000-12-31" ~ "20th century",
      tdf_winners$start_date > "2001-01-01" ~ "21st century",
      TRUE ~ as.character(tdf_winners$start_date))
  ) %>% 
  mutate(
    continent=case_when(
      tdf_winners$birth_country == "Australia" ~ "Oceania",
      tdf_winners$birth_country == "Belgium" ~ "Europe",
      tdf_winners$birth_country == "Columbia" ~ "South America",
      tdf_winners$birth_country == "Denmark" ~ "Europe",
      tdf_winners$birth_country == "France" ~ "Europe",
      tdf_winners$birth_country == "Germany" ~ "Europe",
      tdf_winners$birth_country == "Ireland" ~ "Europe",
      tdf_winners$birth_country == "Italy" ~ "Europe",
      tdf_winners$birth_country == "Kenya" ~ "Africa",
      tdf_winners$birth_country == "Luxembourg" ~ "Europe",
      tdf_winners$birth_country == "Netherlands" ~ "Europe",
      tdf_winners$birth_country == "Spain" ~ "Europe",
      tdf_winners$birth_country == "Switzerland" ~ "Europe",
      tdf_winners$birth_country == "USA" ~ "North America",
      tdf_winners$birth_country == "Wales" ~ "Europe",
      TRUE ~ as.character(tdf_winners$birth_country))
  )%>%
  group_by(decade,continent)%>% summarize(n=n())




# Graph -------------------------------------------------------------------

RWlogo <- png::readPNG("TF.png")
raster <- as.raster(RWlogo)



p4<-ggplot(tdf_winners2,aes(reorder(continent,n), n,group=decade,label=n, fill=decade)) +
  geom_bar(stat="identity", width=0.6, position = "dodge") +
  geom_text(
    aes(x = continent, y = n, label = n, group = decade),
    position = position_dodge(width = 0.5),
    vjust = -0.3, hjust=0.5,size = 2) +
  scale_fill_manual(values = c("#add8e6", "#20b2aa"))+  labs(x = "",y = "",
       title = "Tour de France",
       subtitle = "Winners by continent - 20th century VS 21st century",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +  
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
  axis.text.y    = element_text(color = "#22222b"),
  legend.background = element_rect(fill = "#f7f7f7"),
  panel.background = element_blank(),
  plot.background = element_rect(fill = "#f7f7f7"),
  plot.margin = unit(c(1, 2, 2, 1), "cm"),
  axis.ticks = element_blank()
) + annotation_raster(raster, xmin=0.5,xmax=1, ymin=75, ymax=85)



p4
