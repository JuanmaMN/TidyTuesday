
# Upload file -------------------------------------------------------------

library(readr)
r4ds_members <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv")
View(r4ds_members)
colnames(r4ds_members)



# Upload packages ---------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)



# Data wrangling ----------------------------------------------------------

a1<-r4ds_members%>%mutate(month=format(r4ds_members$date,"%B"))%>% select(-1)%>%
  group_by(month)%>%
  summarise(total_membership=sum(total_membership),
            total_full_members= sum(full_members),
            total_daily_active_members= sum(daily_active_members),
            total_messages_in_public_channels= sum(messages_in_public_channels),
            total_messages_in_private_channels= sum(messages_in_private_channels),
            total_messages_in_d_ms= sum(messages_in_d_ms)) 


# Prepare the data for heatmap --------------------------------------------

a1<-a1[c(5,4,8,1,9,7,6,2,12,11,10,3),]
View(a1)

## add and index column
a1_2 <-a1  %>% mutate(id = row_number())

## Pass the first column to the number
library(dplyr)
a1_2 <- a1[, -(1)]
View(a1_2)

rownames(a1_2) <- a1$month



# heatmap -----------------------------------------------------------------


library(d3heatmap)

d3heatmap(a1_2, scale = "column", colors = "GnBu", dendrogram = "none", 
          
          xaxis_font_size = "6pt", yaxis_font_size = "7pt", 
          xaxis_height = 160, yaxis_width = 160,
          theme= "dark",
          show_grid = TRUE,
          brush_color = "#0000FF")

# https://rdrr.io/github/rstudio/d3heatmap/man/d3heatmap.html




