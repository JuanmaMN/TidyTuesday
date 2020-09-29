
# Update dataset ----------------------------------------------------------

members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')


# Upload the packages -----------------------------------------------------


pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext,htmltools,reactable,patchwork, choroplethr,choroplethrMaps,
               choroplethrZip,mapproj,hrbrthemes, usmap,scales,ggtext, ggpubr,ggflags)


# Bar ---------------------------------------------------------------------

members_citizenship<-members %>% filter(success =="TRUE" & expedition_role == "Climber")%>% group_by(citizenship)%>%summarize(total=n())%>% top_n(10)

members_citizenship$code=c("cn", "fr", "de", "in", "it","jp", "es", "ch", "gb","us")


ggplotmembers_citizenship<-members_citizenship%>% ggplot(aes(x=fct_reorder(citizenship,total), y=total,group =citizenship,
                                               fill= factor(citizenship))) +
  geom_flag(y = -50, aes(country = code), size = 8, hjust = -2) +
  scale_fill_manual(values = c( "USA" = "#4e79a7",
                                "UK"      = "#76B7B2",
                                "Switzerland"     = "#76B7B2",
                                "Japan" = "#76B7B2",
                                "Italy" = "#76B7B2",
                                "India"      = "#76B7B2",
                                "Spain"      = "#76B7B2",
                                "Germany" = "#76B7B2",
                                "France" = "#76B7B2",
                                "China" = "#76B7B2")) +
  coord_flip()+
  geom_col(width = 0.6,show.legend=F) +
  geom_bar_text(place = "right", contrast = TRUE, size=8,
                aes(label=paste0(citizenship, "  ",comma(total))))  +
  guides(fill = NULL) +
  ylim(-100, 1600)+ 
  theme_void() +
  labs(x = NULL, y = NULL, 
       title = "Top 10 countries with the highest number of successful climbers") +
  theme(
    plot.title = element_text(margin = margin(t=10, b = 10), 
                              color = "#22222b",face = "bold",size = 12,
                              hjust = 0.5,
                              family = "Calibri"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f4efe1", color = NA),    # color removes the border
    plot.margin = unit(c(2, 1, 1, 2), "cm"),
    axis.ticks = element_blank(),
    strip.text.x = element_blank()
  ) 



# Donut chart -------------------------------------------------------------

members_season<-members%>%filter(success == "TRUE" & season !="Unknown" & expedition_role == "Climber") %>%
  group_by(season) %>% summarize (total=n()) %>% na.omit()%>%
  mutate(ftotal=total/sum(total),
         pcnt=round(ftotal*100, 1), 
         yaxismax=cumsum(ftotal), 
         yaxismin = c(0, head(yaxismax, n=-1)),
         label_position = (yaxismax+yaxismin)/2)



color_mine2 <- c("#a0c4a9", "#c27c7c", "#734b5e", "#aaa4b0")


gender_donut_season<-ggplot(members_season, aes(ymax=yaxismax, ymin=yaxismin, xmax=4, xmin=3, fill=season)) +
  geom_rect(show.legend=T, alpha=0.5) + 
  coord_polar(theta="y")+
  xlim(c(1, 5))+ 
  theme_void()+
  expand_limits(x = 0, y = 0) +
  scale_fill_manual(values = color_mine2) +
  geom_text(x=3.5, data = members_season %>% filter(season %in% c("Autumn", "Spring")), aes(y=label_position, label=paste(pcnt,"%",sep="")), size=2, col="black") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.margin=margin(b = 2, unit='cm'),
        legend.box = "horizontal",
        legend.text=element_text(size=8),
        strip.background = element_rect(fill = "#f4efe1"),
        strip.text.x = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.key.size = unit(0.3, "cm")) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
 geom_text(aes(label="Percentage by season", x=5, y=0), color="#000000", family="Calibri", size=4)


members$age<-as.character(members$age)

members_age<-members%>%filter(success == "TRUE" & expedition_role == "Climber") %>%
  mutate(age_bucket=case_when(age  < 18  ~ "Under 18",
                              age  >=18 & age <=24 ~ "18-24",
                              age  >=25 & age <=34 ~ "25-34",
                              age  >=35 & age <=44 ~ "35-44",
                              age  >=45 & age <=54 ~ "45-54",
                              age  >=55 & age <=64 ~ "55-64",
                              age  >=65  ~ "Above 65",
                              TRUE ~ age))    %>%
  group_by(age_bucket) %>% summarize (total=n()) %>% na.omit()%>%
  mutate(ftotal=total/sum(total),
         pcnt=round(ftotal*100, 1), 
         yaxismax=cumsum(ftotal), 
         yaxismin = c(0, head(yaxismax, n=-1)),
         label_position = (yaxismax+yaxismin)/2)





color_mine3 <-  c("#a0c4a9", "#c27c7c", "#734b5e", "#aaa4b0",  "#c8a774","#e6d492","#3b9ab2")

gender_donut_second<-ggplot(members_age, aes(ymax=yaxismax, ymin=yaxismin, xmax=4, xmin=3, fill=age_bucket)) +
  geom_rect(show.legend=T, alpha=0.5) + 
  coord_polar(theta="y")+
  xlim(c(1, 5))+ 
  theme_void()+
  expand_limits(x = 0, y = 0) +
  scale_fill_manual(values = color_mine3) +
  geom_text(x=3.5, data = members_age %>% filter(age_bucket %in% c("18-24", "25-34", "35-44", "45-54", "55-64")), aes(y=label_position, label=paste(pcnt,"%",sep="")), size=2, col="black")  + 
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.margin=margin(b = 2, unit='cm'),
        legend.box = "horizontal",
        legend.text=element_text(size=8),
        strip.background = element_rect(fill = "#f4efe1"),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        strip.text.x = element_blank(),
        legend.key.size = unit(0.3, "cm")) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  geom_text(aes(label="Percentage by age", x=5, y=0), color="#000000", family="Calibri", size=4)



# Patchwork ---------------------------------------------------------------

patchwork4 <- ggplotmembers_citizenship | (gender_donut_season / gender_donut_second)

PWTT <- patchwork4 + plot_annotation(title = "Successful climbing expeditions",
                                       caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)",
                                       theme = theme(plot.background = element_rect(fill = "#f4efe1"),
                                                     plot.title = element_text(margin = margin(t=8, b= 8), 
                                                                               color = "#000000",face = "bold",size = 12,
                                                                               hjust = 0.5,
                                                                               family = "Calibri"),
                                                     plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                                                                  color = "#000000", size = 10, family = "Arial",
                                                                                  hjust = 0.5),
                                                     plot.caption =  element_text(margin = margin(t = 20), 
                                                                                  color = "#000000", size = 8, family = "Arial",
                                                                                  hjust = 0.95)))



PWTT 