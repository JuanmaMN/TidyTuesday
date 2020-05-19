
# Upload the package ------------------------------------------------------

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


# Male --------------------------------------------------------------------

## Prepare the data

Mdonut<-vb_matches%>%
  mutate(
    Analysis=case_when(
      w_p1_country == w_p2_country ~ "Same",
      w_p1_country != w_p2_country ~ "Different",
      TRUE ~ as.character(w_p1_country))) %>% 
  filter(bracket == "Winner's Bracket" 
         & round == "Round 1" 
         & Analysis %in% c("Same", "Different")
         & gender == "M"
         & circuit == "AVP") %>%
  group_by(Analysis) %>% 
  summarize(total=n()) %>%
  mutate(ftotal=total/sum(total), 
         pcnt=round(ftotal*100, 1), 
         yaxismax=cumsum(ftotal), 
         yaxismin = c(0, head(yaxismax, n=-1)),
         label_position = (yaxismax+yaxismin)/2) 


View(Mdonut)

## Graph

gender_donutMdonut<-ggplot(Mdonut, aes(ymax=yaxismax, ymin=yaxismin, xmax=4, xmin=3, fill=Analysis)) +
  geom_rect(show.legend=F, alpha=0.5) + 
  coord_polar(theta="y") +
  xlim(c(1, 5))+ 
  theme_void() + 
  scale_fill_manual(values=c("#CDAA25", "#43a07e")) +
  geom_text(x=3.5, aes(y=label_position, label=paste(pcnt,"%",sep="")), size=4, col=c("black", "black"))  + 
  geom_richtext(aes(label="<span style='color:#000000'>AVP circuit<br></span>
                      <span style='color:#CDAA25'>Different nationality<br></span>
                      <span style='color:#43a07e'>Same nationality<br></span>",
                    x=1, y=0),
                fill=NA, label.color=NA,
                family="Arial",
                size=3)  +
  geom_text(aes(label="Male competition", x=5, y=0), color="#000000", family="Arial Bold", size=4)




# Female ------------------------------------------------------------------

Wdonut<-vb_matches%>%
  mutate(
    Analysis=case_when(
      w_p1_country == w_p2_country ~ "Same",
      w_p1_country != w_p2_country ~ "Different",
      TRUE ~ as.character(w_p1_country))) %>% 
  filter(bracket == "Winner's Bracket" 
         & round == "Round 1" 
         & Analysis %in% c("Same", "Different")
         & gender == "W"
         & circuit == "AVP") %>%
  group_by(Analysis) %>% 
  summarize(total=n()) %>%
  mutate(ftotal=total/sum(total), 
         pcnt=round(ftotal*100, 1), 
         yaxismax=cumsum(ftotal), 
         yaxismin = c(0, head(yaxismax, n=-1)),
         label_position = (yaxismax+yaxismin)/2)



gender_donutWdonut<-ggplot(Wdonut, aes(ymax=yaxismax, ymin=yaxismin, xmax=4, xmin=3, fill=Analysis)) +
  geom_rect(show.legend=F, alpha=0.5) + 
  coord_polar(theta="y") + 
  xlim(c(1, 5))+ 
  theme_void() + 
  scale_fill_manual(values=c("#CDAA25", "#43a07e")) +
  geom_text(x=3.5, aes(y=label_position, label=paste(pcnt,"%",sep="")), size=4, col=c("black", "black"))  + 
  geom_richtext(aes(label="<span style='color:#000000'>AVP circuit<br></span>
        <span style='color:#CDAA25'> Different nationality<br></span>
                      <span style='color:#43a07e'> Same nationality<br></span>
                      ",
                    x=1, y=0),
                fill=NA, label.color=NA,
                family="Arial",
                size=3) +
  geom_text(aes(label="Female competition", x=5, y=0), color="#000000", family="Arial Bold", size=4) 







patchwork <- gender_donutWdonut | gender_donutMdonut


pWM2 <- patchwork + plot_annotation(title = "Beach Volleyball - AVP circuit",
                                    subtitle = "Analysis of the winners within the AVP circuit\n Percentage of winners where the team had the same and different nationalities",
                                    caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)",
                                    theme = theme(plot.background = element_rect(fill = "#ffffff"),
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



pWM2 



