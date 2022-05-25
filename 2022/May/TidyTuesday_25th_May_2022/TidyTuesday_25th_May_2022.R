
# Upload data -------------------------------------------------------------

sevens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/sevens.csv')
fifteens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/fifteens.csv')

# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)

library(gt)
library(gtExtras)

# Prepare 7s --------------------------------------------------------------


sevens_cl<- sevens %>% filter (tournament == "World Cup") %>%
  summarize(
    
    home_team_nz = sum(team_1 == "New Zealand"),
    away_team_nz = sum(team_2 == "New Zealand"),
    winner_nz = sum(winner == "New Zealand"),
    loser_nz = sum(loser == "New Zealand"),
    win_rate_sevens_nz = winner_nz/(home_team_nz+away_team_nz),
    
    home_team_en = sum(team_1 == "England"),
    away_team_en = sum(team_2 == "England"),
    winner_en = sum(winner == "England"),
    loser_en = sum(loser == "England"),
    win_rate_sevens_en = winner_en/(home_team_en+away_team_en),
    
    home_team_ca = sum(team_1 == "Canada"),
    away_team_ca = sum(team_2 == "Canada"),
    winner_ca = sum(winner == "Canada"),
    loser_ca = sum(loser == "Canada"),
    win_rate_sevens_ca = winner_ca/(home_team_ca+away_team_ca),
    
    home_team_us = sum(team_1 == "United States"),
    away_team_us = sum(team_2 == "United States"),
    winner_us = sum(winner == "United States"),
    loser_us = sum(loser == "United States"),
    win_rate_sevens_us = winner_us/(home_team_us+away_team_us),
    
    home_team_fr = sum(team_1 == "France"),
    away_team_fr = sum(team_2 == "France"),
    winner_fr = sum(winner == "France"),
    loser_fr = sum(loser == "France"),
    win_rate_sevens_fr = winner_fr/(home_team_fr+away_team_fr),
    
    home_team_sp = sum(team_1 == "Spain"),
    away_team_sp = sum(team_2 == "Spain"),
    winner_sp = sum(winner == "Spain"),
    loser_sp = sum(loser == "Spain"),
    win_rate_sevens_sp = winner_sp/(home_team_sp+away_team_sp),
    
    home_team_ie = sum(team_1 == "Ireland"),
    away_team_ie = sum(team_2 == "Ireland"),
    winner_ie = sum(winner == "Ireland"),
    loser_ie = sum(loser == "Ireland"),
    win_rate_sevens_ie = winner_ie/(home_team_ie+away_team_ie),
    
    home_team_au = sum(team_1 == "Australia"),
    away_team_au = sum(team_2 == "Australia"),
    winner_au = sum(winner == "Australia"),
    loser_au = sum(loser == "Australia"),
    win_rate_sevens_au = winner_au/(home_team_au+away_team_au),
    
    
    home_team_sa = sum(team_1 == "South Africa"),
    away_team_sa = sum(team_2 == "South Africa"),
    winner_sa = sum(winner == "South Africa"),
    loser_sa = sum(loser == "South Africa"),
    win_rate_sevens_sa = winner_sa/(home_team_sa+away_team_sa),
    
    
    home_team_nl = sum(team_1 == "Netherlands"),
    away_team_nl = sum(team_2 == "Netherlands"),
    winner_nl = sum(winner == "Netherlands"),
    loser_nl = sum(loser == "Netherlands"),
    win_rate_sevens_nl = winner_nl/(home_team_nl+away_team_nl)
  ) %>%
  select(contains("win_rate")) %>% pivot_longer(1:10, names_to = "Country", values_to = "Win_rate_7s")


sevens_cl<- sevens_cl %>% mutate(Country = recode(Country,
                                                  win_rate_sevens_nz = "New Zealand",
                                                  win_rate_sevens_en = "England",
                                                  win_rate_sevens_ca = "Canada",
                                                  win_rate_sevens_us = "United States",
                                                  win_rate_sevens_fr = "France",
                                                  win_rate_sevens_sp = "Spain",
                                                  win_rate_sevens_au = "Australia",
                                                  win_rate_sevens_ie = "Ireland",
                                                  win_rate_sevens_sa = "South Africa",
                                                  win_rate_sevens_nl = "Netherlands"))



# Prepare 15s -------------------------------------------------------------

fifteens_cl<- fifteens %>% filter (tournament == "World Cup") %>%
  summarize(
    
    home_team_nz = sum(team_1 == "New Zealand"),
    away_team_nz = sum(team_2 == "New Zealand"),
    winner_nz = sum(winner == "New Zealand"),
    loser_nz = sum(loser == "New Zealand"),
    win_rate_fifteens_nz = winner_nz/(home_team_nz+away_team_nz),
    
    home_team_en = sum(team_1 == "England"),
    away_team_en = sum(team_2 == "England"),
    winner_en = sum(winner == "England"),
    loser_en = sum(loser == "England"),
    win_rate_fifteens_en = winner_en/(home_team_en+away_team_en),
    
    home_team_ca = sum(team_1 == "Canada"),
    away_team_ca = sum(team_2 == "Canada"),
    winner_ca = sum(winner == "Canada"),
    loser_ca = sum(loser == "Canada"),
    win_rate_fifteens_ca = winner_ca/(home_team_ca+away_team_ca),
    
    home_team_us = sum(team_1 == "United States"),
    away_team_us = sum(team_2 == "United States"),
    winner_us = sum(winner == "United States"),
    loser_us = sum(loser == "United States"),
    win_rate_fifteens_us = winner_us/(home_team_us+away_team_us),
    
    home_team_fr = sum(team_1 == "France"),
    away_team_fr = sum(team_2 == "France"),
    winner_fr = sum(winner == "France"),
    loser_fr = sum(loser == "France"),
    win_rate_fifteens_fr = winner_fr/(home_team_fr+away_team_fr),
    
    home_team_sp = sum(team_1 == "Spain"),
    away_team_sp = sum(team_2 == "Spain"),
    winner_sp = sum(winner == "Spain"),
    loser_sp = sum(loser == "Spain"),
    win_rate_fifteens_sp = winner_sp/(home_team_sp+away_team_sp),
    
    home_team_ie = sum(team_1 == "Ireland"),
    away_team_ie = sum(team_2 == "Ireland"),
    winner_ie = sum(winner == "Ireland"),
    loser_ie = sum(loser == "Ireland"),
    win_rate_fifteens_ie = winner_ie/(home_team_ie+away_team_ie),
    
    home_team_au = sum(team_1 == "Australia"),
    away_team_au = sum(team_2 == "Australia"),
    winner_au = sum(winner == "Australia"),
    loser_au = sum(loser == "Australia"),
    win_rate_fifteens_au = winner_au/(home_team_au+away_team_au),
    
    
    home_team_sa = sum(team_1 == "South Africa"),
    away_team_sa = sum(team_2 == "South Africa"),
    winner_sa = sum(winner == "South Africa"),
    loser_sa = sum(loser == "South Africa"),
    win_rate_fifteens_sa = winner_sa/(home_team_sa+away_team_sa),
    
    
    home_team_nl = sum(team_1 == "Netherlands"),
    away_team_nl = sum(team_2 == "Netherlands"),
    winner_nl = sum(winner == "Netherlands"),
    loser_nl = sum(loser == "Netherlands"),
    win_rate_fifteens_nl = winner_nl/(home_team_nl+away_team_nl)
  ) %>%
  select(contains("win_rate")) %>% pivot_longer(1:10, names_to = "Country", values_to = "Win_rate_15s")


fifteens_cl<- fifteens_cl %>% mutate(Country = recode(Country,
                                                      win_rate_fifteens_nz = "New Zealand",
                                                      win_rate_fifteens_en = "England",
                                                      win_rate_fifteens_ca = "Canada",
                                                      win_rate_fifteens_us = "United States",
                                                      win_rate_fifteens_fr = "France",
                                                      win_rate_fifteens_sp = "Spain",
                                                      win_rate_fifteens_au = "Australia",
                                                      win_rate_fifteens_ie = "Ireland",
                                                      win_rate_fifteens_sa = "South Africa",
                                                      win_rate_fifteens_nl = "Netherlands"))


# Join & Flag -------------------------------------------------------------

US_flag <- png::readPNG("R/US_flag.png")  .... # Upload all images from directory


rugby_join<- sevens_cl%>% left_join(fifteens_cl, by = "Country") %>% mutate(
  Win_rate_7s = percent_format()(Win_rate_7s),
  Win_rate_15s = percent_format()(Win_rate_15s),
  Country_flag = case_when(
    str_detect(Country,'New Zealand') ~ New_Zealand_flag,
    str_detect(Country,'England') ~ England_flag,
    str_detect(Country,'Canada') ~ Canada_flag,
    str_detect(Country,'United States') ~ US_flag,
    str_detect(Country,'France') ~ France_flag,
    str_detect(Country,'Spain') ~ Spain_flag,
    str_detect(Country,'Australia') ~ Australia_flag,
    str_detect(Country,'Ireland') ~ Ireland_flag,
    str_detect(Country,'South Africa') ~ South_Africa_flag,
    str_detect(Country,'Netherlands') ~ NL_flag
    
  )) %>% select(1,4,2,3)



# https://www.scrumqueens.com/news/womens-international-rugby-rankings   Top 10 combined



# gt table ----------------------------------------------------------------



gt_rugby<-rugby_join %>% gt() %>%
  tab_header(
    title = "Women's rugby (7s and 15s) - Win rate in World Cups"
  ) %>%
  cols_label(
    Win_rate_7s = html(
      "<span style='color:#808080;'>Rugby 7s</span>"
    ), 
    Win_rate_15s = html(
      "<span style='color:#808080;'>Rugby 15s</span>"
    ) )%>%
  cols_label(
    Country_flag = " ") %>%
  gt_theme_nytimes() %>% 
  gtExtras::gt_img_rows(columns =Country_flag, height = 20) %>% 
  tab_source_note(md("**Visualization:**: JuanmaMN (Twitter @Juanma_MN) | **Source**:  ScrumQueens for #TidyTuesday")) %>% 
  tab_options(
    table.border.bottom.color = "white",
    table.width = px(610)
  ) %>%
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Playfair Display"
      ), align = "center",size='small')),
    locations = cells_column_labels(
      columns = c("Country_flag", "Win_rate_7s", "Win_rate_15s")
    )
  ) %>%
  cols_align("center",
             vars("Country_flag", "Win_rate_7s", "Win_rate_15s"))  %>%
  tab_style(
    style = cell_text(font = google_font("Playfair Display")),
    locations = cells_body(columns = c("Country", "Win_rate_7s", "Win_rate_15s"))
  ) %>%
  tab_options(
    table.font.size = px(16L),
    heading.padding = px(25)
  ) %>%
  tab_footnote(
    footnote = "Top 10 countries in the combined ranking - www.scrumqueens.com",
    locations = cells_title(groups = "title")
  )


gt_rugby