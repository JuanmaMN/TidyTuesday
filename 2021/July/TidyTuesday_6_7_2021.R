
# Upload the data ---------------------------------------------------------

holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')


# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, scales, ggtext, ggpubr,ggbump,wesanderson,
               grid, gridtext, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)



# Prepare the data --------------------------------------------------------


holidays_days <-holidays %>% select(country,date_parsed, independence_from) 

holidays_days$independence_from[is.na(holidays_days$independence_from)] <- "No Indep. day"

holidays_days$independence_from<- fct_recode(holidays_days$independence_from, 
                                      "Spain" = "Spanish Empire",
                                      "Spain" = "Spanish Empire[72]",
                                      "United Kingdom" = "Kingdom of Great Britain",
                                      "Soviet Union" = "Soviet Union[55]",
                                      "Soviet Union" = "Soviet Union[80]",
                                      "Netherlands" = "United Netherlands",
                                      "Japan" = "Empire of Japan",
                                      "Portugal"= "United Kingdom of Portugal, Brazil and the Algarves",
                                      "United Kingdom" = "United Kingdom of Great Britain and Ireland"
                                      )

holidays_days$country<- fct_recode(holidays_days$country, 
"Antigua & Barbuda"= "Antigua and Barbuda",
"Bahamas"= "Bahamas, The", 
"Bosnia & Herzegovina" = "Bosnia and Herzegovina",
"China", "China, People's Republic of",
"China" = "China, Republic of",
"Congo" = "Congo, Republic of the",
"Dem. Rep. of Congo" = "Congo, Democratic Republic of the",
"Czechia" = "Czech Republic",
"Gambia" = "Gambia, The", 
"Côte d'Ivoire" = "Ivory Coast",
"Micronesia (Federated States of)" = "Micronesia", 
"Myanmar (Burma)" = "Myanmar",
"Netherlands" = "Netherlands, The",
"São Tomé & Príncipe" = "São Tomé and Príncipe",
"Trinidad & Tobago" = "Trinidad and Tobago",
"Vatican" = "Vatican City",
"St. Kitts & Nevis" = "Saint Kitts and Nevis",
"St. Lucia" = "Saint Lucia",
"St. Vincent & Grenadines" = "Saint Vincent and the Grenadines",
"Timor-Leste" = "East Timor")


holidays_days_2<-holidays_days %>% mutate(independence_from2 = case_when(
  independence_from == "Spain" ~ "Spain",
  independence_from == "United Kingdom" ~ "United Kingdom",
  independence_from == "Soviet Union" ~ "Soviet Union",
  independence_from == "Portugal" ~ "Portugal",
  independence_from == "France" ~ "France",
  independence_from == "No Indep. day" ~ "No Indep. day",
  TRUE ~ "Others" )) 




# World -------------------------------------------------------------------


world <- map_data("world") %>%   filter(region != "Antarctica") 

world$region <- recode(world$region, "Syria" = "Syrian Arab Republic")
world$region <- recode(world$region, "Iran" = "Islamic Rep. of Iran")
world$region <- recode(world$region, "Trinidad" = "Trinidad and Tobago")
world$region <- recode(world$region, "Curacao" = "Curaçao")
world$region <- recode(world$region, "China" = "People's Rep. of China")
world$region <- recode(world$region, "Vietnam" = "Viet Nam")
world$region <- recode(world$region, "Laos" = "Lao People's Democratic Republic")
world$region <- recode(world$region, "North Korea" = "DPR of Korea")
world$region <- recode(world$region, "Brunei" = "Brunei Darussalam")
world$region <- recode(world$region, "Tanzania" = "United Rep. of Tanzania")
world$region <- recode(world$region, "Democratic Republic of the Congo" = "Dem. Rep. of Congo")
world$region <- recode(world$region, "Ivory Coast" = "Côte d'Ivoire")
world$region <- recode(world$region, "Republic of Congo" = "Congo")
world$region <- recode(world$region, "Macedonia" = "Republic of North Macedonia")
world$region <- recode(world$region, "Moldova" = "Republic of Moldova")
world$region <- recode(world$region, "UK" = "United Kingdom")
world$region <- recode(world$region, "Slovakia" = "Slovak Republic")
world$region <- recode(world$region, "South Korea" = "Korea")
world$region <- recode(world$region, "USA" = "United States")
world$region <- recode(world$region, "Russia" = "Russian Federation")
world$region <- recode(world$region, "Republic of North Macedonia" = "North Macedonia")
world$region <- recode(world$region, "People's Rep. of China" = "China (People's Republic of)")
world$region <- recode(world$region, "DPR of Korea" = "Korea, Dem. People's Rep.")
world$region <- recode(world$region, "Republic of Moldova" = "Moldova")
world$region <- recode(world$region, "Islamic Rep. of Iran" = "Iran")
world$region <- recode(world$region, "Russian Federation" = "Russia")
world$region <- recode(world$region, "United Rep. of Tanzania" = "Tanzania")
world$region <- recode(world$region, "Trinidad and Tobago" = "Trinidad & Tobago")
world$region <- recode(world$region, "Saint Lucia" = "St. Lucia")
world$region <- recode(world$region, "Saint Vincent" = "St. Vincent & Grenadines")
world$region <- recode(world$region, "Antigua" = "Antigua & Barbuda")
world$region <- recode(world$region, "Czech Republic" = "Czechia")
world$region <- recode(world$region, "Slovak Republic" = "Slovakia")
world$region <- recode(world$region, "Bosnia and Herzegovina" = "Bosnia & Herzegovina")
world$region <- recode(world$region, "Sao Tome and Principe" = "São Tomé & Príncipe")
world$region <- recode(world$region, "Côte d'Ivoire" = "Côte d'Ivoire")
world$region <- recode(world$region, "Syrian Arab Republic" = "Syria")
world$region <- recode(world$region, "China (People's Republic of)" = "China")
world$region <- recode(world$region, "Democratic Republic of the Congo" = "Congo - Brazzaville")
world$region <- recode(world$region, "Sao Tome and Principe" = "São Tomé & Príncipe")
world$region <- recode(world$region, "Korea, Dem. People's Rep." = "North Korea")
world$region <- recode(world$region, "Korea" = "South Korea")
world$region <- recode(world$region, "Myanmar" = "Myanmar (Burma)")
world$region <- recode(world$region, "Lao People's Democratic Republic" = "Laos")
world$region <- recode(world$region, "Viet Nam" = "Vietnam")
world$region <- recode(world$region, "Serbia" = "Yugoslavia") # The Republic of Serbia joined the United Nations on November 1, 2000, as the Federal Republic of Yugoslavia.
world$region <- recode(world$region, "Brunei Darussalam" = "Brunei")
world$region <- recode(world$region, "Micronesia" = "Micronesia (Federated States of)")
world$region <- recode(world$region, "Dem. Rep. of Congo" = "Congo - Brazzaville")
world$region <- recode(world$region, "Saint Kitts" = "St. Kitts & Nevis")



# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Work Sans")

font_labels <- "Work Sans"

showtext_auto()


# Join --------------------------------------------------------------------


holidays_days_3 <- world%>%
  left_join(holidays_days_2, by = c('region'='country'))


holidays_days_3<-holidays_days_3 %>% select(lat, long, group, order, region, independence_from2)

holidays_days_3$independence_from2[is.na(holidays_days_3$independence_from2)] <- "No Indep. day"


# Correct India

holidays_days_3<-holidays_days_3 %>% mutate(independence_from2 = case_when(
  region == "India" ~ "United Kingdom",
  TRUE ~ independence_from2)) 


# Graph -------------------------------------------------------------------

independent_graph <- ggplot() + 
  geom_map(data = world, map = world,
           aes(long, lat, group = group,  map_id = region),
           fill = "#525252", color = "#525252") +
  geom_map(data =holidays_days_3, map = world,
           aes(fill = independence_from2, map_id = region),
           color = "#525252", size = 0.15, alpha = .8) +
  scale_fill_manual(values = c( "United Kingdom" = "#6d597a",
                                "Portugal" = "#89b0ae",
                                "Soviet Union"= "#eaac8b",
                                "Spain" = "#e56b6f",
                                "France" = "#355070",
                                "No Indep. day" = "#fcf7e1",
                                "Others" = "#fafafa")) +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "",y = "",
       title = "International Independence Days",
       subtitle = "Countries with international independence days",
       caption = "Source: #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#22222b", size = 10, family = font_labels,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10,family = font_labels,
                                 hjust = 0.95),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_text(size=8, color = "#525252",family = font_labels),
    legend.title.align = 3.2,
    #legend.position = "none",
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#b9cad4"),
    panel.border = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "bottom",
    legend.text=element_text(size=8, color = "#525252", family = font_labels),
    legend.key.size = unit(0.2, "cm"),
    legend.key = element_blank(),
    legend.background=element_blank(),
    axis.ticks = element_blank(),
    legend.margin=margin(b = 0.1, unit='cm')
  ) +
  guides(fill = guide_legend(
    title="Independence from",
    label.position = "bottom",
    nrow = 1,
    family = font_labels, 
    color = "#525252",
    keywidth = 3, keyheight = 0.5))


independent_graph




