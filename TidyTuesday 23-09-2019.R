# Upload the data ---------------------------------------------------------

school_diversity <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")


# Data manipulation -------------------------------------------------------

school_diversity2<-school_diversity %>% filter(SCHOOL_YEAR == "1994-1995") %>%
  select(2,6:10,12)%>%top_n(10,Total)
  


# Prepare the data for heatmap --------------------------------------------

school_diversity3<-school_diversity2%>% mutate(id = row_number())

school_diversity3<-school_diversity2[, -(1)]
rownames(school_diversity3) <- school_diversity2$LEA_NAME

View(school_diversity3)


school_diversity3[,1:5]<-round(school_diversity3[,1:5],2)







school_diversity3[,6]<-lapply(school_diversity3[,6], comma_format())

View(school_diversity3)



library(heatmaply)

library(d3heatmap)
d3heatmap(school_diversity3, scale = "column", colors = "GnBu", dendrogram = "none",xaxis_font_size = "7pt", yaxis_font_size = "7pt", show_legend = show.legend,main = "TidyTuesday")







