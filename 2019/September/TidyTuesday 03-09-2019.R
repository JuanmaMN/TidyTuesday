
# Database ----------------------------------------------------------------



cpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/cpu.csv")



# Packages to upload ------------------------------------------------------

library(tidyverse)
library(sunburstR)

SB2<-cpu%>%
  mutate(path2 = paste(date_of_introduction, designer, process, area, sep="-")) %>%
  filter(date_of_introduction %in% c(2005:2019)) %>%
  select(path2, transistor_count) 


SB_2 <- as.data.frame(sapply(SB2,gsub,pattern="-NA-NA",replacement=""))
SB_2 <- as.data.frame(sapply(SB2,gsub,pattern="-NA",replacement=""))


p2 <- sunburst(SB_2, legend=FALSE)
p2


# Alternative -------------------------------------------------------------


sb3 <- sund2b(SB_2, width="100%")




# Data checking -----------------------------------------------------------


SByear<-cpu%>% group_by(date_of_introduction)%>%
  summarise(count=sum(transistor_count))
