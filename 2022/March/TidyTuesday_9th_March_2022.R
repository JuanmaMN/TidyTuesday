
# Upload data -------------------------------------------------------------

erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')


# Upload packages ---------------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts, showtext,ggflags,usmap)

# Prepare the data --------------------------------------------------------

erasmus_2 <- erasmus %>% filter(sending_country_code != receiving_country_code)

erasmus__top_4_senders<-erasmus %>% group_by(sending_country_code)%>%summarize(n=n()) %>% top_n(4)


erasmus_2_c<-erasmus_2%>% filter(sending_country_code %in% c("DE", "ES", "FR", "PL")
                                 & receiving_country_code%in% c("DE", "ES", "FR", "PL"))%>%
  group_by(sending_country_code,receiving_country_code) %>% summarize(n=n()) %>% ungroup()

erasmus_2_c$sending_country_code <- recode(erasmus_2_c$sending_country_code, "PL" = "Poland")
erasmus_2_c$sending_country_code <- recode(erasmus_2_c$sending_country_code, "DE" = "Germany")
erasmus_2_c$sending_country_code <- recode(erasmus_2_c$sending_country_code, "ES" = "Spain")
erasmus_2_c$sending_country_code <- recode(erasmus_2_c$sending_country_code, "FR" = "France")

erasmus_2_c$receiving_country_code <- recode(erasmus_2_c$receiving_country_code, "PL" = "Poland")
erasmus_2_c$receiving_country_code <- recode(erasmus_2_c$receiving_country_code,"DE" = "Germany")
erasmus_2_c$receiving_country_code <- recode(erasmus_2_c$receiving_country_code,"ES" = "Spain")
erasmus_2_c$receiving_country_code <- recode(erasmus_2_c$receiving_country_code, "FR" = "France")

names(erasmus_2_c)[1]<-"from"
names(erasmus_2_c)[2]<-"to"

erasmus_2_c$n<-as.numeric(erasmus_2_c$n)

erasmus_2_c<- erasmus_2_c%>% pivot_wider(names_from = to, values_from = n)

# reorder columns

erasmus_2_c<- erasmus_2_c %>% select(1,5,2,3,4)

erasmus_2_c2 <- erasmus_2_c[-1]
row.names(erasmus_2_c2) <- erasmus_2_c$from


erasmus_2_c2<-as.matrix(erasmus_2_c2)


# Graph -------------------------------------------------------------------

# 002395  - France  (blue)
# #FCB507 - SPain (gold)
# C8102E  - Poland (red)
# 000000  - Germany (black)

groupColors <- c("#000000", "#FCB507", "#002395", "#C8102E")

library(circlize)
chordDiagram(erasmus_2_c2, grid.col = groupColors, annotationTrack = c("name", "grid"), directional = -1)

