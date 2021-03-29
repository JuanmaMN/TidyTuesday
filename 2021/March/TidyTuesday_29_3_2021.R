
# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, scales, ggtext, 
               ggpubr, ggthemes, ggbump,wesanderson,countrycode,viridis,rayshader, grid, gridtext, 
               biscale, cowplot, sysfonts, ggimage,extrafont,systemfonts, showtext, ggbeeswarm)

# Upload the data ---------------------------------------------------------

allShades <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allShades.csv')

# Prepare the data --------------------------------------------------------

allShades_natural<-allShades%>% filter(str_detect(name, "natural|Natural")) %>%   
  mutate(type= case_when( 
    lightness  < 0.5 ~ "Dark",
    lightness >= 0.5  & lightness < 0.75 ~ "Medium",
    lightness >= 0.75  ~ "Light",
    TRUE ~ "Others"),
  type = fct_relevel(allShades_natural$type, 
                     c("Dark","Medium","Light"))) %>% 
  arrange(brand, desc(type)) %>% 
  mutate(n = row_number())
  
allShades_natural_total<-allShades_natural%>%group_by(brand)%>%summarize(total=n())

allShades_natural_2<-allShades_natural%>%left_join(allShades_natural_total, by = "brand")

# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Lora")

font_labels <- "Lora"

showtext_auto()


# Graph -------------------------------------------------------------------

p_lightness <-
  allShades_natural_2 %>% 
  ggplot() +
  geom_bar(aes(x = fct_reorder(brand, total), group = n, fill = type), color = "#f0f0f0", size = 0.5) +
  labs(y = "",
       x = "",
       title = "",
       subtitle =  "",
       caption =  "Source: Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")  +
  scale_fill_manual(values = c( "Light" = "#E7C99A",
                                "Medium"= "#D1986B",
                                "Dark"= "#764118")) +
  theme(
    plot.title = element_text(margin = margin(b = 10, t= 10), 
                              color = "#22222b",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10,
                                 hjust = 0.95,
                                 family = font_labels),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.x    = element_text(angle = 45, color = "#525252",vjust = 1.2, hjust = 1,family = font_labels, size = 10),
    axis.text.y    = element_blank(), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(1, 1, 1, 2), "cm"),
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.text=element_text(size=8, color = "#525252"),
    legend.key.size = unit(0.2, "cm"),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.background=element_blank(),
    legend.margin=margin(b = 0.1, unit='cm')) +
  guides(fill = guide_legend(
    label.position = "bottom",
    nrow = 1,
    family = font_labels, 
    color = "#525252",
    keywidth = 3, keyheight = 0.5)) +  
  annotate("text",x = 2, y = 18, fontface = "bold", label = "Which brands have more shades with 'Natural' in the name?", 
                                                family = font_labels, size = 5, hjust = 0) +  
  annotate("text",x = 2, y = 15, fontface = "italic", label = str_wrap("Analysis of total number of shades whose name contains 'Natural'."), 
           family = font_labels, size = 5, hjust = 0) +
  annotate("text",x = 2, y = 13, fontface = "italic", label = str_wrap("Analysis by brand and type of lightness."), 
           family = font_labels, size = 5, hjust = 0) +
  annotate("text",x = 2, y = 10.5, fontface = "italic", label = str_wrap("The lightness value is represented as a decimal from 0 to 1 where 0 is pure black and 1 is pure white."), 
           family = font_labels, size = 5, hjust = 0) 
  




p_lightness
