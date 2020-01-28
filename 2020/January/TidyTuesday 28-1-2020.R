# Upload the packages -----------------------------------------------------

library(tidyverse)
library(stringr)
library(sunburstR)



# Update the data ---------------------------------------------------------

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')
# View(sf_trees)


# Prepare the data --------------------------------------------------------

datag<-sf_trees%>%
  mutate(caretaker= if_else(caretaker == "Private", "Private", "Nonprivate"),
         legal_status = if_else(legal_status %in% c("DPW Maintained", "Permitted Site", "Undocumented"),legal_status,"Others"),
         site_info= word(site_info,1,sep = "\\:")) %>% 
  group_by(caretaker,legal_status,site_info)%>% summarize (n=n())


datag2<-datag%>%
  mutate(path2 = paste(caretaker,legal_status,site_info, sep="-"))  

datag3<-datag2%>%ungroup()%>%select(path2,n)   #ungroup is necessary



# Sunburst ----------------------------------------------------------------

p2 <- sunburst(datag3,legend=FALSE,
               width = "100%",
               height = 600,
               colors = c("#e6d8ad","#e6e6ad","#add8e6", "#ade6d8", "#e6adbb"),
               withD3=TRUE,
               valueField = "size")




p2 <- htmlwidgets::prependContent(p2, htmltools::h1("San Francisco Trees"))
p2 <- htmlwidgets::prependContent(p2, htmltools::h4("Caretaker >>> Legal Status >>> Site info"))
p2 <- htmlwidgets::prependContent(p2, htmltools::h5("TidyTuesday 28.1.2020"))

p2
