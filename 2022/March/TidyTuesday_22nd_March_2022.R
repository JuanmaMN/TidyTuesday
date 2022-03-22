
# Upload data -------------------------------------------------------------

babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')



# Upload packages ---------------------------------------------------------

pacman::p_load(lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts, showtext,ggflags,usmap)


# Prepare the data for gt -------------------------------------------------

babynames_female_top_10 <-   babynames %>% filter(sex == "F") %>% group_by(name)  %>% summarize(sum_n=sum(n)) %>% 
  top_n(10) %>% select(1)


babynames_2<-babynames%>%
  mutate( decade=case_when(
      babynames$year %in% 1960:1969 ~ "60s",
      babynames$year %in% 1970:1979 ~ "70s",
      babynames$year %in% 1980:1989 ~ "80s",
      babynames$year %in% 1990:1999 ~ "90s",
      TRUE ~ as.character(babynames$year)
    )
  ) %>% filter(decade %in% c("60s","70s", "80s","90s")) %>%
  inner_join(babynames_female_top_10, by = "name") %>% group_by(name, decade) %>% summarize(sum_n=sum(n)) %>%
  mutate(sum_n = comma_format()(sum_n))



names(babynames_2)[3]<- "Value"

babynames_2 <- babynames_2 %>% pivot_wider(names_from = "decade", values_from = "Value") %>% ungroup()



# gt table ----------------------------------------------------------------

library(gt)
library(gtExtras)

stream_table2_baby <- babynames_2 %>% 
  gt() %>% 
  gt_theme_nytimes() %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "#000000",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(font = "Lora", weight = "bold")
    ),
    locations = list(
      cells_column_labels(gt::everything())
    )
  ) %>%
  tab_header(
    title = "Top 10 Female names in the US",
    subtitle = md("\n *Analysis* from 1960 to 1999")
  ) %>% 
  tab_style(
    style = cell_fill(color = "#35b0ab"),
    locations = cells_body(
      columns = "60s",
      rows = name == "Mary")) %>%
  tab_style(
    style = cell_fill(color = "#35b0ab"),
    locations = cells_body(
      columns = "70s",
      rows = name == "Jennifer")) %>%
  tab_style(
    style = cell_fill(color = "#35b0ab"),
    locations = cells_body(
      columns = "80s",
      rows = name == "Jennifer")) %>%
  tab_style(
    style = cell_fill(color = "#35b0ab"),
    locations = cells_body(
      columns = "90s",
      rows = name == "Sarah")) %>%
  gt_add_divider(name, color = "#000000", weight = px(1)) %>% 
  tab_source_note(md("**Visualization:**: JuanmaMN (Twitter @Juanma_MN) | **Source**: [Tidy Tuesday](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-22/readme.md)")) %>% 
  tab_options(
    table.border.bottom.color = "white",
    table.width = px(610)
  ) %>%
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Lora"
      ), align = "center",size='large', color = "#000000")),
    locations = cells_column_labels(
      columns = c("60s", "70s", "80s", "90s")
    ) ) %>%
  cols_align("center",
             vars("60s", "70s", "80s", "90s"))  %>%
  tab_style(
    style = cell_text(font = google_font("Lora")),
    locations = cells_body(columns = c("60s", "70s", "80s", "90s"))
  ) %>%
  tab_options(
    table.font.size = px(15L)
  ) %>%
  cols_label(
    "name" = " ",
    "60s" = md("**60-69**"),
    "70s" = md("**70-79**"),
    "80s" = md("**80-89**"),
    "90s" = md("**90-99**")
  )
stream_table2_baby



# Second graph ------------------------------------------------------------


babynames_female <-   babynames %>% filter(sex == "F") %>% group_by(name)  %>% summarize(sum_n=sum(n)) %>% top_n(8) %>% select(1)

babynames_male <-   babynames %>% filter(sex == "M") %>% group_by(name)  %>% summarize(sum_n=sum(n)) %>% top_n(8) %>% select(1)

babynames_both <- union(babynames_female,babynames_male)


baby_names_graph <- babynames %>% inner_join(babynames_both, by = "name") %>% group_by(name, year) %>% summarize(sum_n=sum(n))
 
# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("DM Serif Display")

font_labels <- "DM Serif Display"

font_add_google("Lora")

font_labels2 <- "Lora"

showtext_auto()



# Graph -------------------------------------------------------------------

graph_babynames <- baby_names_graph%>%ggplot(aes(year, sum_n, group = name)) +
  geom_area(alpha = 1,fill="#c6dabf",colour="#c6dabf") +
  facet_wrap(~ name, ncol = 4) +
  scale_y_continuous(limits = c(0, 100000), breaks = seq(50000, 100000, by = 50000),
                     labels = c("50K","100K")) +
  labs(x = "",y = "",
       title = "Top 8 baby names by gender in the US",
       subtitle = "Number of times each baby name has been used per year. 1880 - 2017",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")+
  guides(fill = NULL) +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 16,
                              hjust = 0,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#22222b", size = 12,
                                 hjust = 0,
                                 family = font_labels),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10,
                                 hjust = 0.99),
    axis.title.x = element_text(margin = margin(t = 10),
                                color = "#22222b",
                                family = font_labels),
    axis.title.y = element_text(margin = margin(r = 15), 
                                color = "#22222b",
                                family = font_labels),
    legend.position = "none",
    axis.text.y    = element_text(color = "#22222b"),
    panel.background = element_blank(), 
    panel.grid.major.y =  element_line(colour = "#f0f0f0", size = 0.25),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) + 
  geom_text(aes(x = 1880, y = 25000, label = name), hjust = 0, nudge_x = 0, 
            family = font_labels, fontface = "plain",  size = 4, colour = "#676767")



