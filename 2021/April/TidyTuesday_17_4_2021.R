# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)



# Upload data -------------------------------------------------------------

post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')

post_offices_DF <- post_offices[is.na(post_offices$duration),]   #  filter the data for post offices which never shut down

# Bring US states ---------------------------------------------------------

US_States <- read_excel("US States.xlsx")  # bring State data

names(US_States)[2]<-"state"

post_offices_DF<-post_offices_DF%>%left_join(US_States, by = "state") 

post_offices_DF_count<-post_offices_DF%>%group_by(state)%>%summarize(n=n())


# Bring st and fips

election <- read_csv("election.csv")   # bring st and fips

election<-election%>% select(2:4)

post_offices_DF_count_join<-post_offices_DF_count%>%left_join(election, by = c("state"= "st"))


# Prepare the data --------------------------------------------------------

post_offices_DF_count_join<-post_offices_DF_count_join%>%select(state.y,n) %>%na.omit()

post_offices_DF_count_join<-post_offices_DF_count_join %>%
  remove_rownames() %>%
  column_to_rownames(var = 'state.y')

post_offices_DF_count_join$state<- rownames(post_offices_DF_count_join)

post_offices_DF_count_join<-post_offices_DF_count_join %>% mutate(color = case_when(
  n < 500 ~ "< 500",
  n >=500 & n < 1000 ~ "500 to 1000",
  n >= 1000 ~ "> 1000",
  TRUE ~ "Others"))

post_offices_DF_count_join$color <- fct_relevel(post_offices_DF_count_join$color, c("> 1000","500 to 1000",
                                                                  "< 500"))

# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Quicksand")

font_add_google("Work Sans")

font_labels <- "Quicksand"

font_labels2 <- "Work Sans"

showtext_auto()

# Graph -------------------------------------------------------------------

library(statebins)

ggplot(post_offices_DF_count_join, aes(state=state, fill=color, color=color)) +
  geom_statebins(border_col ="#fbfaf6",border_size = 1) +
  coord_equal() +   scale_fill_manual(values = c( "< 500" = "#e3eddf",
                                                 "500 to 1000" = "#c6dabf",
                                                 "> 1000"= "#b8c375"))  +
  scale_color_manual(values = c( "< 500" = "#e3eddf",
                                "500 to 1000" = "#c6dabf",
                                "> 1000"= "#b8c375")) +
  labs(y = "",
       x = "",
       title = "US Post Offices",
       subtitle =  "Total number of post offices which have never shut down by State ",
       caption =  "Source: TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")  +
  theme(
    plot.title = element_text(
      color = "#22222b",face = "bold",size = 14,
      hjust = 0,
      family = font_labels),
    plot.subtitle = element_text(margin = margin(t=5, b= 30),
                                 color = "#22222b", size = 10, family = font_labels,
                                 hjust = 0),
    plot.caption =  element_text(margin = margin(t = 50), 
                                 color = "#22222b", size = 10,
                                 hjust = 0.99,
                                 family = font_labels),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.x    = element_blank(), 
    axis.text.y    = element_blank(), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text=element_text(size=8, color = "#22222b",family = font_labels),
    legend.key.size = unit(0.2, "cm"),
    legend.key = element_blank(),
    legend.background=element_blank(),
    legend.margin=margin(b = 0.1, unit='cm')
  ) +
  guides(fill = guide_legend(
    label.position = "bottom",
    nrow = 1,
    family = font_labels, 
    color = "#525252",
    keywidth = 2, keyheight = 0.5))
