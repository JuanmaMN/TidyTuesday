# Upload the data ---------------------------------------------------------

women <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')

# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, ggtext, hrbrthemes,systemfonts, sysfonts,showtext, ggpubr,ggflags)

# Prepare the data --------------------------------------------------------

women<-women%>% filter(category %in% c("Leadership","Creativity","Knowledge","Identity"))%>%select(1,3,4)%>%
  mutate(
    x_number = case_when(
      category == "Leadership" ~ 0,
      category == "Creativity" ~ 5,
      category == "Knowledge" ~ 10,
      category == "Identity" ~ 15),
    flag_position = x_number - 0.25,
    code = case_when(
      country == "Ethiopia"  ~ "et",
      country == "Netherlands"  ~ "nl",
      country == "Sierra Leone" ~ "sl",
      country == "Somaliland" ~ "so",
      country == "Egypt"  ~ "et",
      country == "India"  ~ "in",                                       
      country == "Argentina" ~ "ar",                                     
      country == "Hong Kong" ~ "hk",                          
      country == "UK"  ~ "gb",        
      country == "Somalia"  ~ "so",
      country == "South Korea"  ~ "kr",
      country == "Jamaica"  ~ "jm",
      country == "Indonesia"  ~ "id",
      country == "Colombia"  ~ "co",
      country == "Finland"  ~ "fi",
      country == "Uganda"   ~ "ug",
      country == "Ecuador" ~ "ec",
      country == "Kenya" ~ "ke",
      country == "Russia" ~ "ru",
      country == "Thailand"   ~ "th",
      country == "Iran"  ~ "ir",
      country == "Belarus"    ~ "by",
      country == "Mexico"  ~ "mx",
      country == "Nigeria"  ~ "ng",
      country == "Kyrgyzstan"    ~ "kg",
      country == "Morocco"   ~ "ma",
      country == "Syria"     ~ "sy",
      country == "Zimbabwe" ~ "zw",
      country == "Norway"  ~ "no",
      country == "US"   ~ "us",
      country == "Japan"  ~ "jp",
      country == "France"  ~ "fr",
      country == "Zambia" ~ "zm",
      country == "Pakistan"    ~ "pk",
      country == "Benin"   ~ "bj",
      country == "Vietnam"  ~ "vn",
      country == "South Africa"   ~ "za",
      country == "Myanmar" ~ "mm",
      country == "Wales, UK" ~ "gb",
      country == "Malaysia" ~ "my",
      country == "Bangladesh"  ~ "bd",
      country == "UAE"   ~ "ae",
      country == "Italy"   ~ "it",
      country == "Iraq/UK"   ~ "ir",
      country == "Australia"   ~ "au",
      country == "China"    ~ "cn",
      country == "Afghanistan"     ~ "af",
      country == "Yemen"              ~ "ye",
      country == "Tanzania"    ~ "tz",
      country == "Northern Ireland"     ~ "gb",
      country == "DR Congo"   ~ "cd",
      country == "Venezuela"                                     ~ "ve",
      country == "Nepal"                                         ~ "np",
      country == "Peru"                                          ~ "pe",
      country == "Ukraine"                                        ~ "ua",
      country == "Singapore"                                     ~ "sg",
      country == "Germany"                                        ~ "de",
      country == "El Salvador"                                   ~ "sv",
      country == "Republic of Ireland"                            ~ "ie",
      country == "Exiled Uighur from Ghulja (in Chinese, Yining)"~ "cn",
      country == "Turkey"                                       ~ "tr",
      country == "Mozambique"                                    ~ "mz",
      country == "Lebanon"                                       ~ "lb",
      country == "Brazil"  ~ "br")) %>%
  arrange(x_number,name)

# Exiled Uighur from Ghulja (in Chinese, Yining) and Iraq/UK according to place of birth

# Position ----------------------------------------------------------------

# Thanks to @ijeamaka_a for the idea

counts_number = women %>%
  count(x_number) %>%
  select(n) %>%
  unname() %>%
  unlist()


women =
  women %>%
  mutate(counts_number = c(seq(1, counts_number[1]),
                         seq(1, counts_number[2]),
                         seq(1, counts_number[3]),
                         seq(1, counts_number[4])),
         counts_number = -counts_number) 


# Font --------------------------------------------------------------------

font_add_google("Lora")

font_labels <- "Lora"

showtext_auto()



# Graph -------------------------------------------------------------------

women%>%ggplot() +
  # Extinct plants names
  geom_text(aes(x = x_number, y = counts_number, label = name), hjust = 0, fontface = "italic",  size = 4, family = font_labels) +
  annotate(geom = "text", x = 1, y = 0.4, label= "Leadership", hjust = 1, fontface = "bold",  color = "#ee741c", size = 4, family = font_labels, alpha=0.4)+
  annotate(geom = "text", x = 6, y = 0.4, label= "Creativity", hjust = 1,fontface = "bold",   color = "#d04592", size = 4, family = font_labels, alpha=0.4)+
  annotate(geom = "text", x = 11, y = 0.4, label= "Knowledge", hjust = 1,fontface = "bold",   color = "#5ac2de", size = 4, family = font_labels, alpha=0.4)+
  annotate(geom = "text", x = 16, y = 0.4, label= "Identity", hjust = 1, fontface = "bold",  color = "#34aa4d", size = 4, family = font_labels,alpha=0.4)+
  annotate(geom = "text", x = 4, y = -35, label= "*One name on the 100 Women list has been left blank as a tribute", hjust = 1,  size =3, family = font_labels,color = "#22222b")+
  #annotate(geom = "text", x = 4, y = -35.5, label= "*", hjust = 1,  size =3, family = font_labels,color = "#22222b")+
  geom_flag(aes(x=  flag_position, y = counts_number, country = code), size = 4, hjust = -2) +
  scale_x_continuous(limits = c(-0.4,20))+
  labs(x = "",y = "",
       title = "The BBC's 100 women of 2020",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")+
  theme(
    plot.title = element_text(margin = margin(b = 10, t= 10), 
                              color = "#e6d492",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10,
                                 hjust = 0.95,
                                 family = font_labels),
    legend.position = "none",
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y =  element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7", color = NA),    # color removes the border
    plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) 
