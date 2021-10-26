
# Raw data ----------------------------------------------------------------

ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')

race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')


# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts, showtext,ggflags)



# Font --------------------------------------------------------------------

font_add_google("DM Serif Display")

font_labels <- "DM Serif Display"

font_add_google("Bodoni Moda")

font_labels2 <- "Bodoni Moda"

showtext_auto()



# Prepare the data --------------------------------------------------------


race_nat<-ultra_rankings %>%group_by(nationality)%>% summarize(n=n()) %>% top_n(20)%>%select(nationality)

race_nat2<-ultra_rankings %>%group_by(nationality, gender)%>% summarize(n=n())

data_nat_join<-race_nat%>%left_join(race_nat2, by="nationality")

data_nat_join2<-data_nat_join%>%na.omit() %>%group_by(nationality)%>%mutate(perct=n/sum(n))


data_nat_join2$nationality <- factor(data_nat_join2$nationality, levels = c("GRE","ESP","POR","FRA","BEL", "POL", 
                                                                            "ITA","JPN","SWE", "SUI","GER","CHN", "RUS", "GBR", "ARG",
                                                                            "HKG","AUS", "NZL", "USA", "CAN"))


data_nat_join2$code=c("ar","ar",
                      "au","au",
                      "be","be",
                      "ca","ca",
                      "cn","cn",
                      "ea","ea",
                      "fr","fr",
                      "gb","gb",
                      "de","de",
                      "gr","gr",
                      "hk","hk",
                      "it","it",
                      "jp","jp",
                      "nz","nz",
                      "pl","pl",
                      "pt","pt",
                      "ru","ru",
                      "ch","ch",
                      "se","se",
                      "us","us")




# Graph -------------------------------------------------------------------



g_nat<-ggplot(data_nat_join2, aes(x = fct_reorder(nationality,perct), y = perct, fill = gender)) +
  scale_fill_manual(values = c( "M" = "#b9cad4",
                                "W" = "#c6dabf"),
                    expand = c(.007, .007)) +
  #stacked bar
  geom_bar(stat = "identity") +
  geom_text(aes(x =nationality, y = perct, label = sprintf("%1.0f%%",perct*100)), position = position_stack(vjust = 0.5), 
            color="white",
            family = font_labels, size = 3.5) +    coord_flip() +
  geom_flag(y = -.07, aes(country = code), size = 7, hjust = -2) +
  theme(
    plot.title = element_text(margin = margin(b = 1), 
                              color = "#22222b",face = "bold",size = 9,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=5,b = 1), 
                                 color = "#22222b", size = 7, family = font_labels,
                                 hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    axis.ticks = element_blank()) 



# Add text ----------------------------------------------------------------

textpmp2 <- data.frame(
  purpose = c("Ultra Trail Running"),
  purpose2 = c("Gender participation distribution across top 20 countries"),
  purpose3 = c("Female"),
  purpose4 = c(" & "),
  purpose5 = c("  Male")
  
)


textpmpgraph2<-ggplot(fill="#ffa365") +
  geom_text(data = textpmp2,  aes(x = 0.55, y = 0.15, label = purpose),
            color="#808080", size=16, fontface="bold", hjust = 0.7, family = font_labels2) +
  geom_text(data = textpmp2,  aes(x = 0.55, y = 0.05, label = purpose2),
            color="#808080", size=6.5, fontface="bold", hjust = 0.7, family = font_labels) +
  geom_text(data = textpmp2,  aes(x = 0.2, y = -0.05, label = purpose3),
            color="#c6dabf", size=9, fontface="bold", hjust = 0.7, family = font_labels) +
  geom_text(data = textpmp2,  aes(x = 0.35, y = -0.05, label = purpose4),
            color="#808080", size=9, fontface="bold", hjust = 0.7, family = font_labels) +
  geom_text(data = textpmp2,  aes(x = 0.5, y = -0.05, label = purpose5),
            color="#b9cad4", size=9, fontface="bold", hjust = 0.7, family = font_labels) +
  scale_x_continuous(limits = c(-0.2,1)) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  theme(        legend.position = "none",
                panel.background = element_blank(), 
                axis.title.x = element_blank(), 
                axis.title.y = element_blank(), 
                axis.text.x    = element_blank(), 
                axis.text.y    = element_blank(), 
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank(), 
                panel.grid.major = element_blank(),
                panel.grid.major.y = element_blank(),
                plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border
                plot.margin = unit(c(2, 2, 2, 2), "cm"),
                axis.ticks = element_blank()
  ) 


# Final graph -------------------------------------------------------------


ggarrange(g_nat,textpmpgraph2, ncol=2, nrow=1, common.legend = FALSE)+
  theme_ipsum() +
  labs(x = "",y = "",
       title = "",
       subtitle = "",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)") +
  theme(
    plot.caption =  element_text(margin = margin(t = 20, b = 10), 
                                 color = "#808080", size = 8, family = font_labels,
                                 hjust = 0.95),
    plot.background = element_rect(fill = "#fbfaf6", color = NA))  
