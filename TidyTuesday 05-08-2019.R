
# Upload the data ---------------------------------------------------------

bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")
str(bob_ross)
colnames(bob_ross)
View(bob_ross)


# Data wrangling ----------------------------------------------------------


library(tidyverse)
test<-bob_ross%>%gather(Painting,value,3:69) %>%group_by(Painting)%>%summarise(total=sum(value))%>% arrange(total)

View(test)

test10<-top_n(test,10)


# Data visualization ------------------------------------------------------

library(hrbrthemes)
ggplot<-test10 %>% ggplot( aes(x=reorder(Painting,total), y=total)) +
  geom_bar(stat="identity", fill="#69b3a2", width=0.6) +
  coord_flip() +
  
  theme_ipsum() +
  
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(hjust = 0.5)
  ) + ylab("total") +
  xlab("Paiting") +
  labs(
    title = "Bob Ross - painting by the numbers - Top 10",
    subtitle = "TidyTuesday 5.8.2019",
    caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)",
    x = "",
    y = "") 




ggplot+geom_text(aes(label=total),hjust=-0.5) 
