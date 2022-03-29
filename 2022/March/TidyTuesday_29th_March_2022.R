# Upload data -------------------------------------------------------------


sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')


# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)


# Prepare the data --------------------------------------------------------

sports_data <- sports %>% filter(year == "2018") %>% group_by(institution_name, year) %>%
  mutate(total_participation = sum_partic_men	+ sum_partic_women,
         perct_participation = total_participation/sum(total_participation),
         perct_revenue = total_rev_menwomen/sum(total_rev_menwomen)) %>%
  filter(sports == "Football") %>% filter (classification_name == "NCAA Division I-FBS") %>%
  mutate(power_five = case_when(
    institution_name %in% c("Boston College", 
                            "Clemson University", 
                            "Duke University",
                            "Florida State University", 
                            "Georgia Institute of Technology-Main Campus", 
                            "University of Louisville", 
                            "University of Miami", 
                            "University of North Carolina at Chapel Hill",  
                            "North Carolina State University at Raleigh",
                            "University of Pittsburgh-Pittsburgh Campus", 
                            "Syracuse University",
                            "University of Virginia-Main Campus",
                            "Virginia Polytechnic Institute and State University",
                            "Wake Forest University",
                            "University of Notre Dame") ~ "ACC",
    institution_name %in% c("University of Illinois at Urbana-Champaign",
                            "Indiana University-Bloomington",
                            "University of Iowa",
                            "University of Maryland-College Park",
                            "University of Michigan-Ann Arbor",
                            "Michigan State University",
                            "University of Minnesota-Twin Cities",
                            "University of Nebraska-Lincoln",
                            "Northwestern University",
                            "Ohio State University-Main Campus",
                            "Pennsylvania State University-Main Campus",
                            "Purdue University-Main Campus",
                            "Rutgers University-New Brunswick",
                            "University of Wisconsin-Madison") ~ "Big Ten",
    institution_name %in% c("Baylor University",
                            "Iowa State University",
                            "University of Kansas",
                            "Kansas State University",
                            "Oklahoma State University-Main Campus",
                            "Texas Tech University",
                            "Texas Christian University",
                            "West Virginia University",
                            "The University of Texas at Austin",
                            "University of Oklahoma-Norman Campus") ~ "Big 12",
    institution_name %in% c("University of Arizona",
                            "Arizona State University-Tempe",
                            "University of California-Berkeley",
                            "University of California-Los Angeles",
                            "University of Colorado Boulder",
                            "University of Oregon",
                            "Oregon State University",
                            "University of Southern California",
                            "Stanford University",
                            "University of Utah",
                            "University of Washington-Seattle Campus",
                            "Washington State University") ~ "Pac-12",
    institution_name %in% c("The University of Alabama",
                            "University of Arkansas",
                            "Auburn University",
                            "University of Florida",
                            "University of Georgia",
                            "University of Kentucky",
                            "Louisiana State University and Agricultural & Mechanical College",
                            "University of Mississippi",
                            "Mississippi State University",
                            "University of Missouri-Columbia",
                            "University of South Carolina-Columbia",
                            "The University of Tennessee-Knoxville",
                            "Texas A & M University-College Station",
                            "Vanderbilt University") ~ "SEC",
    TRUE ~  "Others")
  ) %>% select(institution_name,perct_participation,perct_revenue, year, power_five) %>%
filter(power_five != "Others")




# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("DM Serif Display")

font_labels <- "DM Serif Display"

showtext_auto()


# Scatter plot ------------------------------------------------------------


football_graph <- sports_data %>% 
  ggplot(aes(x=perct_participation, y = perct_revenue, fill = power_five)) +
  geom_point(size=6, shape=21) +
  scale_fill_manual(values = c(  
    "ACC" = "#e3007d",
    "Big 12" = "#015193",
    "Big Ten" = "#00906e",
    "Pac-12"= "#249edc",
    "SEC" = "#5d2882"
  )) +
  scale_x_continuous(limits = c(0.1, 0.3), breaks = seq(0.1, 0.3, by = 0.1),label = percent_format(),
                     name = "Football participation as percentage of institution participation") +
  scale_y_continuous(limits = c(0.25, 1), breaks = seq(0.25, 1, by = 0.25),label = percent_format(),
                     name = "Football revenue as percentage of institution revenue") +
  labs(x = "",y = "",
       title = "Participation & Revenue analysis in Football in 2018",
       subtitle = "College football and the Power Five conferences analysis. Participation and Revenue as percentage of each institution.",
       caption = "Source: #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 20,
                              hjust = 0,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#636363", size = 14, family = font_labels,
                                 hjust = 0),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#808080", size = 10,family = font_labels,
                                 hjust = 0.95),
    axis.title.x = element_text(face = "bold",size = 14, color = "#636363",family = font_labels,
                                margin = margin(t = 20, r = 0, b = 5, l = 0)),
    axis.title.y = element_text(face = "bold",size = 14, color = "#636363",family = font_labels,
                                margin = margin(t = 0, r = 20, b = 0, l = 5)),
    
    axis.text.x    = element_text(color = "#808080", size = 10, family = font_labels, margin = margin(t = 0, r = 0, b = 0, l = 0)),
    axis.text.y    = element_text(color = "#808080",size = 10, family = font_labels, margin = margin(t = 0, r = 0, b = 0, l = 0)),
    panel.background = element_rect(fill = "#f7f7f7", color = NA), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.7, 1), 
    legend.justification = c(0, 0),
    legend.direction = "horizontal",
    legend.title=element_blank(),
    legend.margin=margin(b = 0.5, unit='cm'),
    legend.text=element_text(size=10,family = font_labels),
    strip.background = element_rect(fill = "#f7f7f7"),
    strip.text.x = element_blank(),
    legend.key.size = unit(0.5, "cm"),
    legend.background =  element_rect(fill = "#f7f7f7", color = NA),
    legend.box.background = element_rect(fill = "#f7f7f7", colour = "transparent"),
    legend.key = element_rect(fill = "transparent", colour = "transparent")
  ) +
  guides(fill = guide_legend(
    label.position = "bottom",
    family = font_labels, 
    color = "#808080",
    keywidth = 3, keyheight = 0.5)) +
  geom_segment(data= sports_data, mapping=aes(x=0.1,xend=0.3,y=0.25,yend=0.25),linetype="dotted",colour = "#000000")+
  geom_segment(data= sports_data, mapping=aes(x=0.1,xend=0.3,y=0.5,yend=0.5),linetype="dotted",colour = "#000000") +
  geom_segment(data= sports_data, mapping=aes(x=0.1,xend=0.3,y=0.75,yend=0.75),linetype="dotted",colour = "#000000") +
  geom_segment(data= sports_data, mapping=aes(x=0.1,xend=0.3,y=1,yend= 1),linetype="dotted",colour = "#000000")


football_graph

