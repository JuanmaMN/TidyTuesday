library(ggtext)
library(tidyverse)
library(scales)
library(patchwork)
library(ggflags)
library(countrycode)
library(ggfittext)
library(scales)


# Raw data ----------------------------------------------------------------

vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

View(vb_matches)


library(ggtext)
library(tidyverse)
library(scales)
library(patchwork)
library(ggflags)
library(countrycode)
library(ggfittext)
library(scales)


# Male

MdonutFIVB3<-vb_matches%>%
  mutate(
    Analysis=case_when(
      w_p1_country == w_p2_country ~ "Same",
      w_p1_country != w_p2_country ~ "Different",
      TRUE ~ as.character(w_p1_country))) %>% 
  filter(bracket == "Winner's Bracket" 
         & round == "Round 1" 
         & gender == "M"
         & circuit == "FIVB") %>%
  group_by(w_p1_country) %>% 
  summarize(total=n()) %>%
  mutate(ftotal=total/sum(total), 
         pcnt=round(ftotal*100, 1)) %>%
  top_n(8,ftotal)



MdonutFIVB3$code=c("at","br","de", "nl","no","ru","ch","us")

WdonutFIVB3<-vb_matches%>%
  mutate(
    Analysis=case_when(
      w_p1_country == w_p2_country ~ "Same",
      w_p1_country != w_p2_country ~ "Different",
      TRUE ~ as.character(w_p1_country))) %>% 
  filter(bracket == "Winner's Bracket" 
         & round == "Round 1" 
         & gender == "W"
         & circuit == "FIVB") %>%
  group_by(w_p1_country) %>% 
  summarize(total=n()) %>%
  mutate(ftotal=total/sum(total), 
         pcnt=round(ftotal*100, 1)) %>%
  top_n(8,ftotal)


WdonutFIVB3$code=c("au",
                   "br",
                   "ch",
                   "cz",
                   "de",
                   "nl",
                   "ch",
                   "us")


#WdonutFIVB3  differs from WdonutFIVB2 a bit. The same for M database



ggplotMdonutFIVB2B<-MdonutFIVB3 %>% ggplot(aes(x=fct_reorder(w_p1_country,ftotal), y=ftotal,group = w_p1_country,
                                               fill= factor(w_p1_country))) +
  #geom_bar(stat="identity", fill="#69b3a2", width=0.6) +
  geom_flag(y = -.005, aes(country = code), size = 8, hjust = -2) +
  #geom_text(nudge_y = 0.2, color = "darkred", size = 5) +
  scale_fill_manual(values = c( "Brazil" = "#4e79a7",
                                "United States"      = "#76B7B2",
                                "Switzerland"     = "#76B7B2",
                                "Russia" = "#76B7B2",
                                "Norway" = "#76B7B2",
                                "Netherlands"      = "#76B7B2",
                                "Germany" = "#76B7B2",
                                "Austria" = "#76B7B2"))+
  coord_flip()+
  #coord_flip(clip = "off", expand = FALSE)  +
  geom_col(width = 0.6,show.legend=F) +
  geom_bar_text(place = "right", contrast = TRUE, size=10,
                aes(label=paste0(w_p1_country, "  ",pcnt)))  +
  guides(fill = NULL) +
  ylim(-0.01, 0.2)+ 
  theme_void() 


ggplotWdonutFIVB2B<-WdonutFIVB3 %>% ggplot(aes(x=fct_reorder(w_p1_country,ftotal), y=ftotal,group = w_p1_country,
                                               fill= factor(w_p1_country))) +
  #geom_bar(stat="identity", fill="#69b3a2", width=0.6) +
  geom_flag(y = -.005, aes(country = code), size = 8, hjust = -2) +
  #geom_text(nudge_y = 0.2, color = "darkred", size = 5) +
  scale_fill_manual(values = c( "Brazil" = "#4e79a7",
                                "United States"= "#76B7B2",
                                "Switzerland" = "#76B7B2",
                                "Czech Republic"= "#76B7B2",
                                "Australia"= "#76B7B2",
                                "Netherlands"= "#76B7B2",
                                "Germany" = "#76B7B2",
                                "China" = "#76B7B2"))+
  coord_flip()+
  #coord_flip(clip = "off", expand = FALSE)  +
  geom_col(width = 0.6,show.legend=F) +
  geom_bar_text(place = "right", contrast = TRUE, size=10,
                aes(label=paste0(w_p1_country, "  ",pcnt)))  + 
  guides(fill = NULL) +
  ylim(-0.01, 0.2)+ 
  theme_void() 



patchwork4 <- ggplotWdonutFIVB2B | ggplotMdonutFIVB2B


PWFIVB <- patchwork4 + plot_annotation(title = "Beach Volleyball - FIVB circuit",
                                       subtitle = "Country analysis - Percentage of winners - Top 8 countries",
                                       caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)",
                                       theme = theme(plot.background = element_rect(fill = "#f4efe1"),
                                                     plot.title = element_text(margin = margin(b = 8), 
                                                                               color = "#000000",face = "bold",size = 12,
                                                                               hjust = 0.5,
                                                                               family = "Arial"),
                                                     plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                                                                  color = "#000000", size = 10, family = "Arial",
                                                                                  hjust = 0.5),
                                                     plot.caption =  element_text(margin = margin(t = 20), 
                                                                                  color = "#000000", size = 8, family = "Arial",
                                                                                  hjust = 0.95)))



PWFIVB
