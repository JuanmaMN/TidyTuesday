

# Upload the packages -----------------------------------------------------

pacman::p_load(tidyverse, ggplot2, gt)


# Upload data -------------------------------------------------------------

tournament <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')


tournament_year_2018<-tournament%>%filter(year==2018) %>% group_by(conference, year)%>%summarize(full_w= sum(full_w),
                                                                                                 full_l	= sum(full_l),
                                                                                                 total_games=full_w+full_l,
                                                                                                 prct_win=full_w/total_games) %>% select(1,3,4,6) %>% ungroup()



# GT table ----------------------------------------------------------------

TT_gt_conference_2018<- tournament_year_2018%>% gt(
  rowname_col = "conference",
)%>%
  tab_header(
    title = md("**Conference stats**"),
    subtitle = md("NCAA Women's Basketball Tournament")
  ) %>% 
  fmt_percent(
    columns = vars(`prct_win`),
    decimals = 2
  ) %>%
  tab_footnote(
    footnote = "Analysis of total number of games played",
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_source_note(
    source_note = md(
      "Source: NCAA official page [ncaa.com](https://www.ncaa.com/). Designed by: JuanmaMN for TidyTuesday")
  )  %>%
  tab_stubhead(label = md("**Conference**"))  %>%
  tab_spanner(
    label = md("**2018**"),
    columns = vars(`full_w`,`full_l`,`prct_win`)
  ) %>%
  cols_width( vars(conference) ~ px(160), 
              vars(full_w) ~ px(80), 
              vars(full_l) ~ px(80),  
              vars(prct_win) ~ px(150) ) %>%
  cols_align(align = "center",  columns = vars(conference)) %>% 
  cols_align(align = "center", columns = vars(full_w)) %>% 
  cols_align(align = "center",  columns = vars(full_l)) %>% 
  cols_align(align = "center",  columns = vars(prct_win)) %>% 
  data_color(
    columns = vars(`prct_win`),
    colors = scales::col_numeric(
      palette = c(
        "grey90", "grey70", "grey50"),
      domain = c(0.54, 0.75,0.95))
  ) %>%  cols_label(
    full_w = md("Wins"),
    full_l = md("Losses"),
    `prct_win`= md("Wins %")
  ) %>% 
  tab_style( style = cell_text(font = "Times New Roman"),  locations = cells_body(columns = 1:4)) %>% 
  tab_options(
    table.width = pct(60),
    table.layout = "auto",
    row_group.border.top.width = px(5),
    row_group.border.top.color = "white",
    row_group.border.bottom.width = px(1),
    row_group.border.bottom.color = "lightgrey",
    table.border.top.color = "white",
    table.border.bottom.color = "white",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(1),
    column_labels.border.top.width = px(10),
    column_labels.border.top.color = "white",
    column_labels.background.color = "#1d428a",
    heading.title.font.size = "medium",
    heading.subtitle.font.size = "small",
    heading.background.color = "#1a1a1a",
    stub.border.style = "dotted"
  )


TT_gt_conference_2018