# Upload the data ---------------------------------------------------------

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')



# Upload packages ---------------------------------------------------------


library(tidyverse)
library(ggchicklet)
library(scales)
library(patchwork)


##########################    Hotel Analysis



# Prepare the data --------------------------------------------------------

hotels_data<-hotels%>% select(hotel,arrival_date_month)  %>%
  group_by(hotel,arrival_date_month) %>%
  count() %>%
  group_by(arrival_date_month)%>%
  mutate(prop = n / sum(n))


# Order the months --------------------------------------------------------

hotels_data$arrival_date_month <- as.factor(hotels_data$arrival_date_month)

hotels_data$arrival_date_month<- fct_relevel(hotels_data$arrival_date_month, c("December",
                                                                               "November",
                                                                               "October",
                                                                               "September",
                                                                               "August",
                                                                               "July",
                                                                               "June",
                                                                               "May",
                                                                               "April",
                                                                               "March",
                                                                               "February","January"))



plot_hotel<-ggplot(hotels_data, aes(x = arrival_date_month, y = prop, fill = hotel)) +
  geom_chicklet() +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c("#add8e6", "#20b2aa")) +
  labs(caption = "", 
       title = "Which hotel is booked the most in each month?", y = "", x = "") +
  theme(plot.title = element_text(margin = margin(b = 10), 
                                  color = "#22222b",face = "bold",size = 14,
                                  hjust = 0.5,
                                  family = "Arial"),
        plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                     color = "#22222b", size = 9, family = "Arial",
                                     hjust = 0.5),
        plot.caption =  element_text(margin = margin(t = 20), 
                                     color = "#22222b", size = 10, family = "Arial",
                                     hjust = 0.95),
        #axis.text.x  = element_blank(),
        #axis.text.y    = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        #plot.background = element_rect(fill = "#f7f7f7"),
        #plot.margin = unit(c(2, 2, 2, 1), "cm"),
        axis.ticks = element_blank(),
        legend.position="bottom") +
  guides(fill = guide_legend(title = "")) + scale_y_continuous(labels = function(x) paste0(x*100, "%")) 





##########################    Meal Analysis


# Prepare the data --------------------------------------------------------



hotels_meal	<-hotels%>% select(meal,arrival_date_month)  %>%
  group_by(meal,arrival_date_month) %>%
  count() %>%
  group_by(arrival_date_month)%>%
  mutate(prop = n / sum(n))


hotels_meal$arrival_date_month <- as.factor(hotels_meal$arrival_date_month)

hotels_meal$arrival_date_month<- fct_relevel(hotels_meal$arrival_date_month, c("December",
                                                                               "November",
                                                                               "October",
                                                                               "September",
                                                                               "August",
                                                                               "July",
                                                                               "June",
                                                                               "May",
                                                                               "April",
                                                                               "March",
                                                                               "February","January"))



# Graph -------------------------------------------------------------------


plot_meal<-ggplot(hotels_meal, aes(x = arrival_date_month, y = prop, fill = meal)) +
  geom_chicklet() +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c("#add8e6", "#f08080","#20b2aa", "#bbbbbb",
                               "#f6f6f6")) +
  labs(caption = "", 
       title = "Type of meal booked per month", y = "", x = "") +
  theme(plot.title = element_text(margin = margin(b = 10), 
                                  color = "#22222b",face = "bold",size = 14,
                                  hjust = 0.5,
                                  family = "Arial"),
        plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                     color = "#22222b", size = 9, family = "Arial",
                                     hjust = 0.5),
        plot.caption =  element_text(margin = margin(t = 20), 
                                     color = "#22222b", size = 10, family = "Arial",
                                     hjust = 0.95),
        #axis.text.x  = element_blank(),
        #axis.text.y    = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        #plot.background = element_rect(fill = "#f7f7f7"),
        #plot.margin = unit(c(2, 2, 2, 1), "cm"),
        axis.ticks = element_blank(),
        legend.position="bottom") +
  #scale_x_discrete(labels = scales::percent_format()) +
  guides(fill = guide_legend(title = ""))  + scale_y_continuous(labels = function(x) paste0(x*100, "%")) 



##########################    Cancellations Analysis


# Prepare the data --------------------------------------------------------


hotels_cancelled	<-hotels%>% select(is_canceled,arrival_date_month)  %>%
  group_by(is_canceled,arrival_date_month) %>%
  count() %>%
  group_by(arrival_date_month)%>%
  mutate(prop = n / sum(n))

hotels_cancelled$is_canceled<- if_else(hotels_cancelled$is_canceled == "0", "not cancelled", "cancelled")

hotels_cancelled$arrival_date_month <- as.factor(hotels_cancelled$arrival_date_month)

hotels_cancelled$arrival_date_month<- fct_relevel(hotels_cancelled$arrival_date_month, c("December",
                                                                                         "November",
                                                                                         "October",
                                                                                         "September",
                                                                                         "August",
                                                                                         "July",
                                                                                         "June",
                                                                                         "May",
                                                                                         "April",
                                                                                         "March",
                                                                                         "February","January"))



plot_cancelled<-ggplot(hotels_cancelled, aes(x = arrival_date_month, y = prop, fill = is_canceled)) +
  geom_chicklet() +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c("#add8e6", "#20b2aa")) +
  labs(caption = "", 
       title = "Which month has the highest number of cancellations?", y = "", x = "") +
  theme(plot.title = element_text(margin = margin(b = 10), 
                                  color = "#22222b",face = "bold",size = 14,
                                  hjust = 0.5,
                                  family = "Arial"),
        plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                     color = "#22222b", size = 9, family = "Arial",
                                     hjust = 0.5),
        plot.caption =  element_text(margin = margin(t = 20), 
                                     color = "#22222b", size = 10, family = "Arial",
                                     hjust = 0.95),
        #axis.text.x  = element_blank(),
        #axis.text.y    = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        #plot.background = element_rect(fill = "#f7f7f7"),
        #plot.margin = unit(c(2, 2, 2, 1), "cm"),
        axis.ticks = element_blank(),
        legend.position="bottom") +
  guides(fill = guide_legend(title = ""))  + scale_y_continuous(labels = function(x) paste0(x*100, "%")) 





##########################   Returning VS New  Analysis



# Prepare the data --------------------------------------------------------

hotels_return	<-hotels%>% select(is_repeated_guest,arrival_date_month)  %>%
  group_by(is_repeated_guest,arrival_date_month) %>%
  count() %>%
  group_by(arrival_date_month)%>%
  mutate(prop = n / sum(n))

hotels_return$is_repeated_guest	<- if_else(hotels_return$is_repeated_guest	== "0", "New guest", "Returning guest")



hotels_return$arrival_date_month <- as.factor(hotels_return$arrival_date_month)

hotels_return$arrival_date_month<- fct_relevel(hotels_return$arrival_date_month, c("December",
                                                                                   "November",
                                                                                   "October",
                                                                                   "September",
                                                                                   "August",
                                                                                   "July",
                                                                                   "June",
                                                                                   "May",
                                                                                   "April",
                                                                                   "March",
                                                                                   "February","January"))



# Graph -------------------------------------------------------------------


plot_returned<-ggplot(hotels_return, aes(x = arrival_date_month, y = prop, fill = is_repeated_guest)) +
  geom_chicklet() +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c("#add8e6", "#20b2aa")) +
  labs(caption = "", 
       title = "Percentage of booking by repeated and new guests ", y = "", x = "") +
  theme(plot.title = element_text(margin = margin(b = 10), 
                                  color = "#22222b",face = "bold",size = 14,
                                  hjust = 0.5,
                                  family = "Arial"),
        plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                     color = "#22222b", size = 9, family = "Arial",
                                     hjust = 0.5),
        plot.caption =  element_text(margin = margin(t = 20), 
                                     color = "#22222b", size = 10, family = "Arial",
                                     hjust = 0.95),
        #axis.text.x  = element_blank(),
        #axis.text.y    = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        #plot.background = element_rect(fill = "#f7f7f7"),
        #plot.margin = unit(c(2, 2, 2, 1), "cm"),
        axis.ticks = element_blank(),
        legend.position="bottom") +
  guides(fill = guide_legend(title = ""))  + scale_y_continuous(labels = function(x) paste0(x*100, "%")) 




# Patchwork ---------------------------------------------------------------




patchwork<-(plot_hotel|plot_meal)/(plot_cancelled|plot_returned)


patchwork + plot_annotation(title="",
                            caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN") +
  plot_layout(nrow = 2, heights = c(2, 2))












