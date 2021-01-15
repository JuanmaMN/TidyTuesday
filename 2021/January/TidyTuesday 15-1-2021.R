
# Upload raw data ---------------------------------------------------------

artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

data_artwork<-artwork
data_artists<-artists

# Upload packages ---------------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, ggtext, hrbrthemes, gridExtra,patchwork, systemfonts, sysfonts,showtext, ggpubr,
               extrafont, systemfonts,circlepackeR,packcircles,viridis, ggflags)
               

# Data preparation --------------------------------------------------------

data_artists$new <- sub("^.*?,", "", data_artists$placeOfBirth)

data_artists$new <-sub(".*? ", "", data_artists$new)

data_artists_2<-data_artists%>%select(name, gender, placeOfBirth,new)

data_artwork_2<-data_artwork%>%group_by(artist, acquisitionYear)%>%summarize(n=n())

names(data_artwork_2)[1]<-"name"

data_art__join<- data_artwork_2%>% left_join(data_artists_2, by= "name")

data_art__join_names1<-data_art__join%>%
  mutate(nationality = case_when(
    str_detect(name, "Jones, George")         ~ "United Kingdom",
    str_detect(name, "British")     ~ "United Kingdom",
    str_detect(name, "Barlow, Francis")           ~ "United Kingdom",
    str_detect(name, "Duncombe, Susanna")     ~ "United Kingdom",
    str_detect(name, "Hoare, Prince")     ~ "United Kingdom",
    str_detect(name, "Wharncliffe, Lady")     ~ "Ireland",
    str_detect(name, "Aylesford, Heneage Finch, Fourth Earl of")     ~ "United Kingdom",
    str_detect(name, "González, Joan")     ~ "Spain",
    str_detect(name, "Takamatsu, Jiro")     ~ "Japan",
    str_detect(name, "Guerrilla Girls")     ~ "United States",
    str_detect(name, "González, Joan")     ~ "United Kingdom",
    str_detect(name, "Townroe, Reuben")     ~ "United Kingdom",
    str_detect(name, "Percy, Lady Susan Elizabeth")     ~ "United Kingdom",
    str_detect(name, "Laporte, John")     ~ "United Kingdom",
    str_detect(name, "Bone, Sir Muirhead")     ~ "United Kingdom",
    str_detect(name, "Monro, Dr Thomas")     ~ "United Kingdom",
    str_detect(name, "Michell, Keith")     ~ "Australia",
    str_detect(name, "Ophir, Gilad")     ~ "Israel",
    str_detect(name, "Owen, Samuel")     ~ "United Kingdom",
    str_detect(name, "Barnard, Rev. William Henry")     ~ "United Kingdom",
    str_detect(name, "Langlands and Bell, Ben and Nikki")     ~ "United Kingdom",
    str_detect(name, "Gilpin, Rev. William")     ~ "United Kingdom",
    str_detect(name, "Kerrich, Thomas")     ~ "United Kingdom",
    str_detect(name, "Rochdale")     ~ "United Kingdom",
    str_detect(name, "Plymouth")     ~ "United Kingdom",
    str_detect(name, "Republika")     ~ "Czech republic",
    str_detect(name, "Ceská Republika")     ~ "Czech republic",
    str_detect(name, "Kitadai, Shozo")     ~ "Japan",
    str_detect(name, "Laurence, Samuel")     ~ "United Kingdom",
    str_detect(name, "Pinkney, Richard ")     ~ "United Kingdom",
    str_detect(name, "Martin, Charles")            ~ "France", 
    str_detect(name, "Terry Atkinson, born 1939" )           ~ "United Kingdom", 
    str_detect(name, "Pinkney, Richard")            ~ "United Kingdom", 
    str_detect(name, "Underwood, Thomas Richard")            ~ "United Kingdom", 
    str_detect(name, "Christie, John")            ~ "United Kingdom", 
    str_detect(name, "Becker, Ferdinand")            ~ "Germany", 
    str_detect(name, "Lairesse")            ~ "Netherland", 
    str_detect(name, "Posenenske, Charlotte")            ~ "Germany", 
    str_detect(name, "Berry, John, CBE")            ~ "United Kingdom", 
    str_detect(name, "Gordon, Julia Emily")            ~ "United Kingdom", 
    str_detect(name, "Murray, Charles Fairfax")            ~ "United Kingdom", 
    str_detect(name, "Pinkney, Richard")            ~ "United Kingdom", 
    str_detect(name, "Piper, Edward")            ~ "United Kingdom", 
    str_detect(new, "Kingdom")  ~ "United Kingdom",
    str_detect(new, "London")  ~ "United Kingdom",
    str_detect(new,  "Liverpool")  ~ "United Kingdom",
    str_detect(new, "Kensington")  ~ "United Kingdom",
    str_detect(new, "Hertfordshire")  ~ "United Kingdom",
    str_detect(new, "Edinburgh")  ~ "United Kingdom",
    str_detect(new,  "on Trent")  ~ "United Kingdom",
    str_detect(new, "Canterbury")  ~ "United Kingdom",
    str_detect(new, "Bristol")  ~ "United Kingdom",
    str_detect(new, "Blackheath")  ~ "United Kingdom",
    str_detect(new, "Mehoz, Yisra'el")  ~ "Israel",
    str_detect(new, "D.C., Colombia")  ~ "Colombia",
    str_detect(new, "Zealand")  ~ "New Zealand",
    str_detect(new, "Mehoz, Yisra'el")  ~ "Israel",
    str_detect(new, "Yisra'el")  ~ "Israel",
    str_detect(new, "Département de la, France")  ~ "France",
    str_detect(new, "Magyarország")  ~ "Hungary",
    str_detect(new, "Stockholm")  ~ "Sweden",
    str_detect(new, "Sverige")  ~ "Sweden",
    str_detect(new, "States")  ~ "United States",
    str_detect(new, "Perth")  ~ "Australia",
    str_detect(new, "Ísland")  ~ "Iceland",
    str_detect(new, "Nihon")  ~ "Japan",
    str_detect(new, "España")  ~ "Spain",
    str_detect(new, "Éire")  ~ "Ireland",
    str_detect(new, "Plymouth")  ~ "United Kingdom",
    str_detect(new, "Braintree")  ~ "United Kingdom",
    str_detect(new, "Al-Lubnan")  ~ "United Kingdom",
    str_detect(new, "Charlieu")  ~ "France",
    str_detect(new, "Ellás")  ~ "Greece",
    str_detect(new, "Epsom")  ~ "United Kingdom",
    str_detect(new, "Douglas")  ~ "United States",
    str_detect(new, "Lietuva")  ~ "Lithuania",
    str_detect(new, "Min-kuo")  ~ "China",
    str_detect(new, "Zhonghua")  ~ "China",
    str_detect(new, "Bharat")  ~ "India",
    str_detect(new, "Hrvatska")  ~ "Croatia",
    str_detect(new, "Polska")  ~ "Poland",
    str_detect(new, "Danmark")  ~ "Denmark",
    str_detect(new, "Lietuva")  ~ "Lithuania",
    str_detect(new, "Latvija")  ~ "Latvia", 
    str_detect(new, "Auteuil")  ~ "France", 
    str_detect(new, "Schlesien")  ~ "Poland", 
    str_detect(new, "Österreich")  ~ "Austria", 
    str_detect(new, "Niederschlesien")  ~ "Germany", 
    str_detect(new, "Rossiya")  ~ "Russia", 
    str_detect(new, "Schweiz")  ~ "Swtizerland", 
    str_detect(new, "Deutschland")  ~ "Germany", 
    str_detect(new, "Italia")  ~ "Italy", 
    str_detect(new, "België")  ~ "Belgium", 
    str_detect(new, "Nederland")  ~ "Netherland", 
    str_detect(new, "Republika")  ~ "Czech Republic", 
    str_detect(new, "Highmore")  ~ "United Kingdom", 
    str_detect(new, "Norge")  ~ "Norway", 
    str_detect(new, "Nederland")  ~ "Netherland", 
    str_detect(new, "Misr")  ~ "Egypt", 
    TRUE ~ new
  ))  %>% filter (nationality != "United Kingdom" 
                  & nationality != "United States")


# Artists above 8 

data_art__join_names2 <-  data_art__join_names1%>%
  group_by(nationality)%>% summarize(n=sum(n))

data_art__join_names3<-data_art__join_names2%>% 
  mutate(colour = case_when(nationality == "Germany" ~ "#3b9ab2",
                            nationality  == "Ireland" ~ "#c27c7c", 
                            nationality  == "France" ~ "#a0c4a9",
                            TRUE ~ "#aaa4b0"))


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Lora")

font_labels <- "Lora"

showtext_auto()


# Graphs ------------------------------------------------------------------


packing_art <- data_art__join_names3 %>% 
  circleProgressiveLayout(sizecol = "n", sizetype = 'area') %>% 
  mutate(radius = radius * 0.85)


dat.gg_artist <- circleLayoutVertices(packing_art, npoints = 100)

labels_artist <- data_art__join_names3  %>% 
  select(nationality, n) %>% 
  bind_cols(packing_art) %>% mutate(label = case_when(
    nationality == "Germany"  ~ paste0(nationality, " ", "- ", n),
    nationality == "France"  ~ paste0(nationality, " ", "- ", n),
    nationality == "Ireland"  ~ paste0(nationality, " ", "- ", n),
    TRUE ~ ""))


p_first_graph <- 
  ggplot() + 
  geom_polygon(data = dat.gg_artist, aes(x, y, group = id, fill = factor(id)),
               colour = NA) +
  geom_text(data = labels_artist, aes(x, y, size = n, label =label),
            color = "#ffffff", fontface = "bold", size = 3,
            family = font_labels) +
  scale_size_continuous(range = c(1, 3)) +
  scale_fill_manual(values = data_art__join_names3$colour) +
  coord_equal() + 
  theme(legend.position="none") +
  theme(
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
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) 


test_nationality <- data_art__join_names3%>% top_n(10,n)%>%  arrange(n)%>%
  mutate(x_axis = rep(5, each = 10, length = n()),
         y_axis = rep(-10:1, length = n()),
         flag_position = x_axis - 0.5,
         code = case_when(
           nationality == "Germany"  ~ "de",
           nationality == "France"  ~ "fr",
           nationality == "Ireland"  ~ "ie",
           nationality == "Russia"  ~ "ru",
           nationality == "Australia"  ~ "au",
           nationality == "Italy"  ~ "it",
           nationality == "Spain"  ~ "es",
           nationality == "Argentina"  ~ "ar",
           nationality == "Japan"  ~ "jp",
           nationality == "Swtizerland"  ~ "ch",
           TRUE ~ ""))


p_second_graph<-test_nationality %>%ggplot() +
  geom_text(aes(x = x_axis, y = y_axis, label = nationality), hjust = 0, fontface = "italic", color = "#525252",  size = 4, 
            family = font_labels) +
  geom_flag(aes(x=  flag_position, y = y_axis, country = code), size = 6, hjust = -2)   +
  scale_x_continuous(limits = c(0,10))+
  labs(title = "Top 10 nationalities of artists") +
  theme(
    plot.title = element_text(margin = margin(t=10, b = 10), 
                              color = "#525252", size = 10, family = font_labels,face = "bold",
                              hjust = 0.6),
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



test_artist<-data_art__join_names1 %>%
    group_by(name)%>% summarize(n=sum(n)) %>% top_n(10,n)%>% arrange(n)%>%
  mutate(x_axis = rep(5, each = 10, length = n()),
         y_axis = rep(-10:1, length = n()),
         flag_position = x_axis - 0.5,
         code = case_when(
           name == "De Loutherbourg, Philip James"  ~ "fr",
           name == "Kippenberger, Martin"  ~ "de",
           name == "Ferrari, León"  ~ "ar",
           name == "Hamilton Finlay, Ian"  ~ "bs",
           name == "Schütte, Thomas"  ~ "de",
           name == "Nolan, Sir Sidney"  ~ "au",
           name == "Gabo, Naum"  ~ "ru",
           name == "Winner, Gerd"  ~ "de",
           name == "Beuys, Joseph"  ~ "de",
           name == "Cozens, Alexander"  ~ "ru",
           TRUE ~ ""))


p_third_graph<-test_artist%>%ggplot() +
  geom_text(aes(x = x_axis, y = y_axis, label = name), hjust = 0, fontface = "italic",color = "#525252",   size = 4, 
            family = font_labels) +
  geom_flag(aes(x=  flag_position, y = y_axis, country = code), size = 6, hjust = -2)   +
  scale_x_continuous(limits = c(0,10))+
  labs(title = "Top 10 artists") +
  theme(plot.title = element_text(margin = margin(t=10, b = 10), 
                                 color = "#525252", size = 10, family = font_labels,
                                 face = "bold",
                                 hjust = 0.6),
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
    axis.ticks = element_blank()
  ) 



ggarrange(p_second_graph,p_first_graph,p_third_graph, ncol=3, nrow=1, common.legend = FALSE)+
  theme_ipsum() +
  labs(x = "",y = "",
       title = "Art Collections",
       subtitle = "Total artworks in the Tate Museum from artists outside UK and US",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#e6d492",face = "bold",size = 14,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10, b = 25), 
                                 color = "#525252", size = 10, family = font_labels,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                 color = "#808080", size = 8, family = font_labels,
                                 hjust = 0.95),
    plot.background = element_rect(fill = "#f7f7f7", color = NA))  


