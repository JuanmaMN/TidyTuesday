
# Upload data -------------------------------------------------------------

nominees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-21/nominees.csv')

# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, scales, ggtext, ggpubr,ggbump,wesanderson,
               grid, gridtext, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)



# Prepare the data --------------------------------------------------------

nominees_tv_networks_A <- nominees %>% mutate(category = str_replace_all(category, "[1234567890]", "")) %>% 
  filter (type == "Winner" & year == "2021") %>%
  mutate(distributor = recode(distributor, "App Store, Apple TV+" = "Apple TV+",
                              "HBO" = "HBO/HBO Max",
                              "HBO Max" = "HBO/HBO Max")) %>%
  mutate(awards = case_when(
    category == "Outstanding Individual Achievement In Animation - " ~ production,
    TRUE ~ distributor
  )) %>%
  distinct(category,title,distributor, year, awards) %>% 
  group_by(year, distributor) %>% summarize (n=n()) %>%
  top_n(8, n)


# Outstanding Limited Or Anthology Series - 2021  is not included. Hence, Netflix is 44 instead of 43

nominees_tv_networks_A<- nominees_tv_networks_A %>% rename("awards"= n) 

nominees_tv_networks_A$awards<-as.numeric(nominees_tv_networks_A$awards)

nominees_tv_networks_A<- nominees_tv_networks_A %>% 
  mutate(awards = case_when(
    distributor == "Netflix" & year == 2021 ~ 44,
    TRUE ~ awards)) %>%
  mutate(distributor = fct_reorder(distributor, awards))


# Images ------------------------------------------------------------------

img = c("abc.png",
        "apple_tv.png",
        "disney.png",
        "FX.png",
        "HBO.png",
        "NBC.png",
        "netflix.png",
        "VH1.png") # Logos are there in my directory

# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Quicksand")

font_add_google("Work Sans")

font_labels <- "Quicksand"

font_labels2 <- "Work Sans"

showtext_auto()

# Graph -------------------------------------------------------------------


emmys_graph <- nominees_tv_networks_A %>% ggplot(aes(x= awards, y = distributor, fill = distributor)) +
  geom_bar(stat = "identity", position = "identity", width = 0.4) +
  scale_fill_manual(values = c("Netflix" = "#e13d3d",
                               "HBO/HBO Max" = "#e13d3d",
                               "Disney+" = "#e13d3d",
                               "Apple TV+" = "#e13d3d",
                               "NBC" = "#e13d3d",
                               "VH1" = "#e13d3d",
                               "ABC" = "#e13d3d",
                               "FX Networks" = "#e13d3d")) +
  scale_x_continuous(limits= c(-2.5,52)) +
  labs(y = "",
       x = "",
       caption ="Source: #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")  +
  theme(plot.caption =  element_text(margin = margin(t = 50), 
                                 color = "#22222b", size = 10,
                                 hjust = 0.5,
                                 family = font_labels),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.x    = element_blank(), 
    axis.text.y    = element_blank(), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border,
    axis.ticks = element_blank(),
    legend.position = "none")

emmys_graph + geom_image(aes(x = -1.8, y = 1, image = img[4]), asp = 2) +
  geom_image(aes(x = -1.8, y = 2, image = img[1]), asp = 2) +
  geom_image(aes(x = -1.8, y = 3, image = img[8]), asp = 2) +
  geom_image(aes(x = -1.8, y = 4, image = img[6]), asp = 2) + 
  geom_image(aes(x = -1.8, y = 5, image = img[2]), asp = 2) +
  geom_image(aes(x = -1.8, y = 6, image = img[3]), asp = 2) +
  geom_image(aes(x = -1.8, y = 7, image = img[5]), asp = 2) + 
  geom_image(aes(x = -1.8, y = 8, image = img[7]), asp = 2) +
  geom_text(aes(label = awards), vjust= 0.5, hjust= 1, nudge_x = -0.8, color = "white", size = 5,family = font_labels, fontface = "bold") +
  annotate("text",x =30.5,  y = 4.5, label = "Netflix",colour = "#e13d3d",  vjust = 0.5, size = 15, family = font_labels, fontface = "bold") +
  annotate("text",x = 30.5,  y = 2.6, label = "won the highest number of Emmys \nfor first time in 2021 \n with 44 awards",
           colour = "#525252",  vjust = 0.5, size = 12, family = font_labels) 
