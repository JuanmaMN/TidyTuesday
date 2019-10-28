
# Upload the packages -----------------------------------------------------

library(tidyverse)
library(sunburstR)

# Upload the data ---------------------------------------------------------

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

View(nyc_squirrels)



# Work with the date ------------------------------------------------------


nyc_squirrels$date <- as.character(nyc_squirrels$date)

nyc_squirrels$date <- as.Date(nyc_squirrels$date, "%m%d%Y")


# Prepare the data for SunburstR ------------------------------------------


#Extract month, day of week

nyc_squirrels<-nyc_squirrels%>%mutate(month=format(nyc_squirrels$date,"%B"),
                                      day=format(nyc_squirrels$date,"%A")) %>% select(shift,age,primary_fur_color,
                                                                                      day) %>%
  group_by(shift,age,primary_fur_color,day) %>%
  summarise(n=n())

# Prepare for sunburst

nyc_squirrels2<-nyc_squirrels%>%
  mutate(path2 = paste(day,shift,age,primary_fur_color, sep="-")) 

nyc_squirrels3<-nyc_squirrels2%>%ungroup()%>%select(path2,n)   #ungroup is necessary
  
nyc_squirrels3<- as.data.frame(sapply(nyc_squirrels3,gsub,pattern="-NA",replacement=""))
nyc_squirrels3<- as.data.frame(sapply(nyc_squirrels3,gsub,pattern="-NA-NA",replacement=""))
nyc_squirrels3<- as.data.frame(sapply(nyc_squirrels3,gsub,pattern="-NA-NA-NA",replacement=""))


# Upload the packages -----------------------------------------------------




p2 <- sunburst(nyc_squirrels3,legend=FALSE,
               width = "100%",
               height = 600,
               colors = c("#e6d8ad","#e6e6ad","#add8e6", "#ade6d8", "#e6adbb"),
               withD3=TRUE,
               valueField = "size")



