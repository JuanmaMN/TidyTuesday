
# Upload data -------------------------------------------------------------

# I have downloaded the data from freedomhouse.org

library(readxl)
freedom <- read_excel("freedom_status.xlsx") 

# Upload packages ---------------------------------------------------------

pacman::p_load(lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts, showtext,ggflags,usmap)

# Prepare the data --------------------------------------------------------

names(freedom)[1]<-"Country"

freedom_2 <- freedom %>% filter (Edition == "2022") %>% select(Country,Edition, Status)



# World -------------------------------------------------------------------


world <- map_data("world") %>%   filter(region != "Antarctica") 
world$region <- recode(world$region, "Syria" = "Syrian Arab Republic")
world$region <- recode(world$region, "Iran" = "Islamic Rep. of Iran")
world$region <- recode(world$region, "Trinidad" = "Trinidad and Tobago")
world$region <- recode(world$region, "Curacao" = "Curaçao")
world$region <- recode(world$region, "China" = "People's Rep. of China")
world$region <- recode(world$region, "Vietnam" = "Viet Nam")
world$region <- recode(world$region, "Laos" = "Lao People's Democratic Republic")
#world$region <- recode(world$region, "North Korea" = "DPR of Korea")
world$region <- recode(world$region, "Tanzania" = "United Rep. of Tanzania")
world$region <- recode(world$region, "Democratic Republic of the Congo" = "Dem. Rep. of Congo")
world$region <- recode(world$region, "Ivory Coast" = "Côte d'Ivoire")
world$region <- recode(world$region, "Republic of Congo" = "Republic of the Congo")
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
#world$region <- recode(world$region, "Serbia" = "Yugoslavia") # The Republic of Serbia joined the United Nations on November 1, 2000, as the Federal Republic of Yugoslavia.
world$region <- recode(world$region, "Micronesia" = "Micronesia (Federated States of)")
world$region <- recode(world$region, "Dem. Rep. of Congo" = "Congo - Brazzaville")
world$region <- recode(world$region, "Saint Kitts" = "St. Kitts & Nevis")


## To further match (some of them might not be needed depending on the below - extra for freedom)
world$region<- recode(world$region, "Bosnia & Herzegovina" ="Bosnia and Herzegovina" )
world$region<- recode(world$region,"Côte d'Ivoire"="Cote d'Ivoire" )
world$region<- recode(world$region,"Congo - Brazzaville" ="Democratic Republic of the Congo" )
world$region<- recode(world$region,"Cape Verde" ="Cabo Verde" )
world$region<- recode(world$region, "Guinea-Bissau" ="Guinea Bissau" )
world$region<- recode(world$region, "South Korea" ="Korea, South" )
world$region<- recode(world$region, "Myanmar (Burma)" ="Myanmar"  )
world$region<- recode(world$region, "North Korea" = "Korea, North" )
world$region<- recode(world$region, "Trinidad & Tobago" = "Trinidad and Tobago")
world$region<- recode(world$region, "United States" ="United States of America" )
world$region<- recode(world$region, "St. Vincent & Grenadines"= "Saint Vincent and the Grenadines" )
world$region<- recode(world$region, "São Tomé & Príncipe"= "Sao Tome and Principe" )
world$region <- recode(world$region, "Brunei" = "Brunei Darussalam")
world$region <- recode(world$region, "St. Lucia" = "Saint Lucia")


## Extra for freedom

freedom_2$Country<- recode(freedom_2$Country,"Antigua and Barbuda"="Antigua & Barbuda")
freedom_2$Country<- recode(freedom_2$Country,"Brunei"="Brunei Darussalam")
freedom_2$Country<- recode(freedom_2$Country,"Congo (Brazzaville)"="Republic of the Congo")
freedom_2$Country<- recode(freedom_2$Country,"Congo (Kinshasa)"="Democratic Republic of the Congo")
freedom_2$Country<- recode(freedom_2$Country,"Czech Republic"="Czechia")
freedom_2$Country<- recode(freedom_2$Country,"Guinea-Bissau"="Guinea Bissau")
freedom_2$Country<- recode(freedom_2$Country,"Micronesia"="Micronesia (Federated States of)")
freedom_2$Country<- recode(freedom_2$Country,"North Korea"="Korea, North")
freedom_2$Country<- recode(freedom_2$Country,"South Korea"="Korea, South")
freedom_2$Country<- recode(freedom_2$Country,"St. Kitts and Nevis"="St. Kitts & Nevis")
freedom_2$Country<- recode(freedom_2$Country,"St. Lucia"="Saint Lucia")
freedom_2$Country<- recode(freedom_2$Country,"St. Vincent and the Grenadines"="Saint Vincent and the Grenadines")
freedom_2$Country<- recode(freedom_2$Country,"The Gambia"="Gambia")
freedom_2$Country<- recode(freedom_2$Country,"United States"="United States of America")


# Fix Somaliand -----------------------------------------------------------

world <- world %>% mutate (region = case_when(subregion == "Somaliland" ~ "Somaliland",
                                              TRUE ~ region))

# Join --------------------------------------------------------------------

freedom_3 <- freedom_2%>%
  left_join(world, by = c('Country' ='region'))

freedom_3$Status<- recode(freedom_3$Status,"F"="Free")
freedom_3$Status<- recode(freedom_3$Status,"NF"="Not Free")
freedom_3$Status<- recode(freedom_3$Status,"PF"="Partly Free")


# Check NA ----------------------------------------------------------------

freedom_3_Na <-freedom_3%>% filter(is.na(lat))

# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("DM Serif Display")

font_labels <- "DM Serif Display"

showtext_auto()

# Graph -------------------------------------------------------------------

freedom_graph_2022 <- ggplot() + 
  geom_map(data = world, map = world,
           aes(long, lat, group = group,  map_id = region),
           fill = "#525252", color = "#525252") +
  geom_map(data =freedom_3, map = world,
           aes(fill = Status, map_id = Country),
           color = "#525252", size = 0.15, alpha = .8) +
  scale_fill_manual(values = c(  
    "Not Free" = "#6a71a7",
    "Free" = "#00aa83",
    "Partly Free" = "#e7b80a",
    "No value"= "#525252"
  )) +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "",y = "",
       title = "Freedom in the World - 2022",
       subtitle = "",
       caption = "Source: https://freedomhouse.org/\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
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
    legend.title.align = 0,
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#d9e2e7"),
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
    title="",
    title.position = "top",
    label.position = "bottom",
    nrow = 1,
    family = font_labels, 
    color = "black",
    keywidth = 3, keyheight = 0.5))



freedom_graph_2022

