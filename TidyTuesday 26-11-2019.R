
# Upload packages ---------------------------------------------------------

library(lubridate)
library(tidyverse)
library(scales)
library(hrbrthemes)



# Upload data -------------------------------------------------------------

loans <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")


# Data manipulation -------------------------------------------------------

loans[is.na(loans)] <- 0
loans$agency_name[loans$agency_name == "Action Financial Services*" ] <- "Action Financial Services" 
loans$agency_name[loans$agency_name %in% c("Credit Adjustments, Inc.","Credit Adjustments, Inc.*")] <- "Action Financial Services" 
loans$agency_name[loans$agency_name == "Central Research*"  ] <- "Central Research" 
loans$agency_name[loans$agency_name == "Coast Professional, Inc." ] <- "Coast Professional Inc"
loans$agency_name[loans$agency_name == "GC Services LP"] <- "GC Services" 
loans$agency_name[loans$agency_name == "National Recoveries, Inc." ] <- "National Credit Services" 
loans$agency_name[loans$agency_name == "Immediate Credit Recovery, Inc." ] <- "Immediate Credit Recovery"  
loans$agency_name[loans$agency_name == "Pioneer"] <- "Pioneer Credit Recovery, Inc" 
loans$agency_name[loans$agency_name == "Windham"] <- "Windham Professionals, Inc."  
loans$agency_name[loans$agency_name == "FMS"] <- "FMS Investment Corp"  
loans$agency_name[loans$agency_name == "ACT"] <- "Account Control Technology, Inc." 

loans$year<-year(as.Date(as.character(loans$year), format="%y"))



# Prepare data for graph --------------------------------------------------

data3<-loans%>%mutate(diff=starting-total,
                      Quarter=as.character(quarter))%>% group_by(year,Quarter) %>%
  summarise(total=sum(diff))  
  
data4<-data3 %>% 
  mutate(total_B = as.numeric(format(round(total/1e9, 1), trim = TRUE))) %>% select(1,2,4)



# Bar chart ---------------------------------------------------------------


ggplot(data4,aes(x=year,y=total_B, fill=Quarter))+geom_bar(stat = "identity") + 
  coord_flip() +
  theme_ipsum()  +
  labs(x = "",y = "billion $",
       title = "Student Loan Payments",
       subtitle = "Difference between Total dollars repaid and Total loan value in dollars at start",
       caption = "Source:Tidy Tuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)")+
  scale_fill_manual(values=c('#f08080','#20b2aa','#87cefa','#f8d568'))+  
  theme(
    plot.title = element_text(margin = margin(b = 10), 
                              color = "#22222b",face = "bold",size = 17,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(b = 25), 
                                 color = "#22222b", size = 12, family = "Arial"),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10, family = "Arial"),
    #panel.grid.major = element_line(color = "#DAE1E7"),
    panel.grid.major.y = element_line(color = "#dbdbdb"),
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.title = element_text (size = 15),
    axis.title.y = element_text(margin = margin(r = 10), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10), hjust = 0.5),
    plot.margin = unit(c(1, 2, 1, 1), "cm"),
    legend.position="bottom"
  ) 



