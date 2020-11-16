
# Upload data -------------------------------------------------------------


black_in_data <- data.frame(
  date = seq(as.Date("2020-11-16"), as.Date("2020-11-21"), 1),
  hashtag = c("#BlackInDataRollCall",
              "#BlackInDataJourney",
              "#BlackInDataSkills",
              "#BlackInDataViz",
              "#BlackInDataJustice", 
              "#BlackInDataMentorship"),
  purpose = c(
    "Giving Black people in data a space to introduce themselves and their work. Introducing and valuing intersecting parts of their identities. We welcome contributions from a wide spectrum of Data Fields including but not limited to Informatics, Technology, Data Science, Coding, Social Science and Data Analytics.",
    "Further fostering community for Black people in data, by encouraging them to share their varied journeys in data.",
    "Discussing the skills Black people in data have learned, communal sharing of resources and advice for skills development.",
    "Creating space for Black people in data to share their work in the form of favourite data visualisation images.",
    "Hosting forums for learning and discussion of bias in the data field (and possible paths to address biases in data).",
    "Join us for career development and mentorship events!"
  ),
  link = c(
    "https://blkindata.github.io/project/blackindatarollcall/",
    "https://blkindata.github.io/project/blackindatajourney/",
    "https://blkindata.github.io/project/blackindataskills/",
    "https://blkindata.github.io/project/blackindataviz/",
    "https://blkindata.github.io/project/blackindatajustice/",
    "https://blkindata.github.io/project/blackindatacommunity/"
  )
)




# Upload packages ---------------------------------------------------------


pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,stringr,showtext)



# Prepare the data --------------------------------------------------------

black_in_data_data<-black_in_data%>%mutate(hastag_position=c(-1,-1.06,-1.11,-1,-1.06,-1.11),
                                           purpose_position=c(0,0,0,0,0,0),
                                           purpose_position_2=c(0.05,0.05,0.05,0.05,0.05,0.05),
                                           text_position = c(-1.023,-1.08,-1.13,-1.02,-1.08,-1.125),
                                           column=c(1,1,1,2,2,2))



# Fonts -------------------------------------------------------------------

font_add_google("Lora")

font_labels <- "Lora"

font_add_google("Anton")

font_title <- "Anton"

showtext_auto()


# Graph -------------------------------------------------------------------



ggplot(fill="#e13d3d") +
  # Extinct plants names
  geom_text(data = black_in_data_data,  aes(x = purpose_position, y = hastag_position, label = str_wrap(paste0(hashtag," ", "|", " ",date))),
            color="grey20", size=8, fontface="bold", hjust = 0, family = font_labels)+
  geom_text(data = black_in_data_data,  aes(x = purpose_position_2, y = text_position, label = str_wrap(purpose)), hjust = 0, size = 4.5,fontface = "italic", 
            family = font_labels) +
  scale_x_continuous(limits = c(-0.2,1)) +
  scale_y_continuous(limits = c(-1.135,-1)) +
  facet_wrap(vars(column)) +
  labs(x = "",y = "",
       title = "#BlackInDataWeek  |  November 16 - 21, 2020",
       subtitle = "",
       caption = "Source: #BlackInDataViz website | TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  guides(fill = NULL) +
  theme(
    plot.title = element_text(margin = margin(b = 10, t= 10), 
                              color = "#22222b",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_title),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#22222b", size = 9,
                                 hjust = 0.5,
                                 family = font_labels),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10,
                                 hjust = 0.95),
    axis.title.x = element_text(margin = margin(t = 10),
                                color = "#22222b",
                                family = font_labels),
    axis.title.y = element_text(margin = margin(r = 15), 
                                color = "#22222b",
                                family = font_labels),
    legend.position = "none",
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y =  element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )


