# Database ----------------------------------------------------------------

tornados <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv')

# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts, showtext,ggflags,usmap)

# Font --------------------------------------------------------------------

font_add_google("DM Serif Display")

font_labels <- "DM Serif Display"

font_add_google("Lora")

font_labels2 <- "Lora"

showtext_auto()

# Prepare the data --------------------------------------------------------

tornados_by_state <-tornados |> filter(yr == '2022') |> group_by(st) |> summarize(number_of_tornados=n())

tornados_by_state<-tornados_by_state%>% full_join(statepop, by = c("st"="abbr"))

tornados_by_state<-tornados_by_state%>%
  mutate(
    color = case_when(number_of_tornados < 6 ~ "< 6",
                      number_of_tornados >= 6 & number_of_tornados <= 20 ~ "6 - 20",
                      number_of_tornados >= 21 & number_of_tornados <= 40 ~ "21 - 40",
                      number_of_tornados >= 41 & number_of_tornados <= 60 ~ "41 - 60",
                      number_of_tornados >= 61 & number_of_tornados <= 80 ~ "61 - 80",
                      number_of_tornados >= 81 & number_of_tornados <= 100 ~ "81 - 100",
                      number_of_tornados >= 101 & number_of_tornados <= 120 ~ "101 - 120",
                      number_of_tornados > 120 ~ "> 120",
                      TRUE ~ "No value"))

tornados_by_state$color <- fct_relevel(tornados_by_state$color, c("101 - 120",
                                                                  "81 - 100","61 - 80", "41 - 60","21 - 40",
                                                                  "6 - 20","< 6", "No value"))

tornados_by_state$color <- factor(tornados_by_state$color, c("101 - 120",
                                                             "81 - 100","61 - 80", "41 - 60","21 - 40",
                                                             "6 - 20","< 6", "No value"))


tornados_by_state_map<-plot_usmap(data = tornados_by_state, values = "color",labels = TRUE) + 
  scale_fill_manual(values = c(  "No value" = "#c3c3c3",
                                 "< 6"= "#FFFFFF",
                                 "6 - 20" = "#FEFBB9",
                                 "21 - 40" = "#F8D282",
                                 "41 - 60"= "#F2AA5F",
                                 "61 - 80" = "#EF884E",
                                 "81 - 100" = "#EB5739",
                                 "101 - 120" = "#DA3B2B"
  )) + 
  labs(fill = "color") +
   labs(x = "",y = "",
       title = "Tornadoes by State in 2022",
      x = "",
     y = "") +
  theme(
    plot.title = element_text(margin = margin(b = 15, t=10), 
                              color = "#22222b",face = "bold",size = 20,
                              hjust = 0.5,
                              family = font_labels),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border,
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border,
    panel.border = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    legend.position = "bottom",
    legend.text=element_text(size=12, color = "#22222b"),
    legend.key.size = unit(0.2, "cm"),
    legend.key = element_blank(),
    legend.background=element_blank(),
    axis.ticks = element_blank(),
    legend.margin=margin(b = 0.1, unit='cm')
  ) +
  guides(fill = guide_legend(
    label.position = "bottom",
    nrow = 2,
    family = font_labels, 
    color = "#525252",
    keywidth = 3, keyheight = 0.5))



# Fatalities --------------------------------------------------------------

# Prepare the data --------------------------------------------------------

tornadosfatalities_by_state <-tornados |> filter(yr == '2022') |> group_by(st) |> summarize(number_of_fatalities=sum(fat))

tornadosfatalities_by_state<-tornadosfatalities_by_state%>% full_join(statepop, by = c("st"="abbr"))

tornadosfatalities_by_state<-tornadosfatalities_by_state%>%
  mutate(
    color = case_when(number_of_fatalities < 1 ~ "< 1",
                      number_of_fatalities >= 1 & number_of_fatalities <= 2 ~ "1 - 2",
                      number_of_fatalities >= 3 & number_of_fatalities <= 4 ~ "3 - 4",
                      number_of_fatalities >= 5 & number_of_fatalities <= 6 ~ "5 - 6",
                      number_of_fatalities >= 7 & number_of_fatalities <= 8 ~ "7 - 8",
                      number_of_fatalities >= 9 & number_of_fatalities<= 10 ~ "9 - 10",
                      number_of_fatalities > 10 ~ "> 10",
                      TRUE ~ "No value"))


tornadosfatalities_by_state$color <- fct_relevel(tornadosfatalities_by_state$color, c("7 - 8", "3 - 4",
                                                                                      "1 - 2","< 1", "No value"))

tornadosfatalities_by_state$color <- factor(tornadosfatalities_by_state$color, c("7 - 8", "3 - 4",
                                                                                 "1 - 2","< 1", "No value"))

tornadosfatalities_by_state_map<-plot_usmap(data = tornadosfatalities_by_state, values = "color",labels = TRUE) + 
  scale_fill_manual(values = c(  "No value" = "#c3c3c3",
                                 "< 1"= "#FFFFFF",
                                 "1 - 2" = "#FEFBB9",
                                 "3 - 4" = "#F8D282",
                                 "5 - 6"= "#F2AA5F",
                                 "7 - 8" = "#EF884E",
                                 "9 - 10" = "#EB5739",
                                 "> 10" = "#DA3B2B"
  )) + 
  labs(fill = "color") +
   labs(x = "",y = "",
       title = "Tornadoes fatalities by State in 2022",
      x = "",
     y = "") +
  theme(
    plot.title = element_text(margin = margin(b = 15, t=10), 
                              color = "#22222b",face = "bold",size = 20,
                              hjust = 0.5,
                              family = font_labels),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border,
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border,
    panel.border = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    legend.position = "bottom",
    legend.text=element_text(size=12, color = "#22222b"),
    legend.key.size = unit(0.2, "cm"),
    legend.key = element_blank(),
    legend.background=element_blank(),
    axis.ticks = element_blank(),
    legend.margin=margin(b = 0.1, unit='cm')
  ) +
  guides(fill = guide_legend(
    label.position = "bottom",
    nrow = 2,
    family = font_labels, 
    color = "#525252",
    keywidth = 3, keyheight = 0.5))


# Graphs ------------------------------------------------------------------

ggarrange(tornados_by_state_map,tornadosfatalities_by_state_map, ncol=2, nrow=1, common.legend = FALSE, legend="bottom") +
  theme_ipsum() +
  labs(x = "",y = "",
       title ="Tornadoes analysis by State in 2022",
       caption = "Source: #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)"
  ) +
  theme(
    plot.title = element_text(margin = margin(b = 20), 
                              color = "#343434",face = "bold",size = 24,
                              hjust = 0,
                              family = font_labels),
    plot.caption =  element_text(margin = margin(t = 30, b = 10), 
                                 color = "#343434", size = 14, family = font_labels2,
                                 hjust = 0.5),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),
    legend.background =  element_rect(fill = "#f7f7f7", color = NA))  



