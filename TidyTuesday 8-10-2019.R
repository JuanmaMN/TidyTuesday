
# Upload the data ---------------------------------------------------------

ipf_lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")


# Data manipulation -------------------------------------------------------


ipf_lifts$place[ipf_lifts$place == "1"] <- "Gold"

ipf_lifts$place[ipf_lifts$place =="2"] <- "Silver"

ipf_lifts$place[ipf_lifts$place =="3"] <- "Bronze"

ipf_lifts$place[ipf_lifts$place %in% c("4", "5","6", "7", "8", "9", "10")] <- "4-10"

ipf_lifts$place[ipf_lifts$place %in% c("11", "12","13", "14", "15", "16", "17",
                                       "18", "19", "20", "21", "22", "23",
                                       "24", "25", "26", "27", "28", "29", "30", "31")] <- "11-31"

ipf_lifts$place[ipf_lifts$place =="G"] <- "Guest lifter"
ipf_lifts$place[ipf_lifts$place =="DQ"] <- "Disqualified"
ipf_lifts$place[ipf_lifts$place =="DD"] <- "Doping Disqualification"
ipf_lifts$place[ipf_lifts$place =="NS"] <- "No-Show"

ipf_lifts1<-ipf_lifts%>%select(age_class, place)%>%group_by(age_class, place) %>%
  summarise(n=n())



ipf_lifts2<-ipf_lifts1 %>% spread(place,n)

# Reorder columns ---------------------------------------------------------

ipf_lifts3 <- ipf_lifts2[, c(1, 8,6,5,2,3,4,9,7)] %>% filter(age_class != "5-12")

View(ipf_lifts3)
# Prepare the data for heatmap --------------------------------------------

ipf_lifts4<-ipf_lifts3%>% mutate(id = row_number())

ipf_lifts4<-ipf_lifts3[, -(1)]
rownames(ipf_lifts4) <- ipf_lifts3$age_class

View(ipf_lifts4)



# heatmap -----------------------------------------------------------------


library(d3heatmap)
d3heatmap(ipf_lifts4, scale = "column", colors = "Blues", dendrogram = "none",xaxis_font_size = "7pt", yaxis_font_size = "7pt", show_legend = show.legend,main = "TidyTuesday")






