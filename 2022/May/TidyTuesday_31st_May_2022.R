# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)

library(gt)
library(gtExtras)

# Upload data -------------------------------------------------------------

poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')


names(poll)[3]<- "rank_2022"
names(poll)[4]<- "rq_2022"


data<- poll %>% filter(between(rank_2022,1,10)) %>%
  drop_na(rq) %>%
  group_by(company, rank_2022,rq_2022,change) %>%
  summarize(trend_data = list(rq), .groups = "drop") %>%
  arrange(rank_2022) %>% mutate(rank= paste0("#",rank_2022)) %>% mutate(arrow = case_when(
    change == 0 ~ "= ",
    change > 0 ~ "??? ",
    change < 0 ~ "??? ",
  ))

data[is.na(data)] = 0

names(data)[5]<- "rank_chg"


# Upload logos from Drive -------------------------------------------------



# Organize columns --------------------------------------------------------

data<- data %>% select(rank,company,logo,rank_chg,rq_2022, arrow, change)



# Gt table ----------------------------------------------------------------

gt_ranking2<-data %>% gt() %>%
  tab_header(
    title = "The 2022 Axios Harris Poll 10 reputation rankings"
  ) %>%
  cols_label(
    rank = html(
      "<span style='color:#808080;'>2022 Ranking</span>"
    ), 
    company = html(
      "<span style='color:#808080;'>Company</span>"
    ),
    rank_chg = html(
      "<span style='color:#808080;'>Trend 2017-2021</span>"
    ),
    rq_2022 = html(
      "<span style='color:#808080;'>2022 Score</span>"
    ),change = html(
      "<span style='color:#808080;'>Change in rank</span>"
    ),
    arrow = " ",
    logo = " "
    
  )%>% 
  gt_theme_nytimes() %>% 
  gtExtras::gt_img_rows(columns =logo, height = 20) %>%
  tab_source_note(md("**Visualization:**: JuanmaMN (Twitter @Juanma_MN) | **Source**:  Axios and Harris Poll for #TidyTuesday")) %>% 
  tab_options(
    table.border.bottom.color = "white",
    table.width = px(610)
  ) %>%
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "PT Sans"
      ), align = "center",size='small')),
    locations = cells_column_labels(
      columns = c("logo", "rank_chg", "rq_2022")
    )
  ) %>%
  cols_align("center",
             vars("rank_chg", "logo", "rq_2022"))  %>%
  cols_align("left",
             vars("change"))  %>%
  cols_align("right",
             vars("arrow"))  %>%
  tab_style(
    style = cell_text(font = google_font("PT Sans"),
                      color = "#808080"),
    locations = cells_body(columns = c("rank", "company", "logo", "rank_chg", "rq_2022"))
  ) %>%
  tab_options(
    table.font.size = px(14L),
    heading.padding = px(25)
  ) %>%
  tab_footnote(
    footnote = "Top 10 most visible brands in America",
    locations = cells_title(groups = "title")
  ) %>%
  gt_sparkline(rank_chg,
               range_colors = c("#ef233c", "#008000"),
               line_color = "#d3d3d3",
               label = FALSE) %>%
  tab_style(
    style = list(
      cell_text(color = "#008000")
    ),
    locations = cells_body(
      columns = c(arrow, change),
      rows = change > 0
    )
  ) %>% tab_style(
    style = list(
      cell_text(color = "#ef233c")
    ),
    locations = cells_body(
      columns = c(arrow, change),
      rows = change < 0
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(color = "#8d99ae")
    ),
    locations = cells_body(
      columns = c(arrow, change),
      rows = change == 0
    )
  ) 

