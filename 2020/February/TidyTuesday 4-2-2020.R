# Upload the data ---------------------------------------------------------

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')


# Load the packages -------------------------------------------------------

library(scales)
library(tidyverse)
library(ggplot2)


# Prepare the data --------------------------------------------------------


dataa<-attendance%>% group_by(year)%>%summarise(total_attendance=sum(total))


dataa2<-dataa%>%mutate(lag1 = lag(total_attendance),
                       increase = (total_attendance/ lag1) - 1,
                       label=percent(increase))%>%
  filter (year !="2000")

dataa3<-dataa2


dataa3$color <- factor(ifelse(dataa2$increase < 0, "low", "high"),   levels = c("low", "high"))


datag<-dataa3%>% select(1,4,5,6)

datag$increase<-as.numeric(datag$increase)


# Graph -------------------------------------------------------------------

ggplot(datag,aes(year, increase, fill=color)) +
  geom_bar(stat = "identity") +
    scale_fill_manual(values = c("#f08080","#20b2aa"), name = NULL) +
  labs(x = "",y = "",
       title = "NFL Stadium Attendance",
       subtitle = "YOY Change in overall attendance in NFL games",
       caption = "\n Source: TidyTuesday
      Visualization: JuanmaMN (Twitter @Juanma_MN)")+
  guides(fill = NULL) +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 14,
                              hjust = 0.5,
                              family = "Arial"),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#22222b", size = 9, family = "Arial",
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#22222b", size = 10, family = "Arial",
                                 hjust = 0.95),
    axis.title.x = element_text(margin = margin(t = 10),
                                color = "#22222b"),
    legend.position = "none",
    axis.text.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "#f7f7f7"),
    plot.margin = unit(c(1, 2, 2, 1), "cm"),
    axis.ticks = element_blank()
  ) + 
  scale_x_continuous(breaks = c(2001:2019)) + 
  geom_text(position = position_dodge(0.9), 
            vjust = -0.9,
            color = "black", size = 3, aes(label=label)) +
  annotate("text", x = 2004, y =-0.01	,fontface ="bold",
           hjust = 0.5, color = "#20b2aa",
           size = 3, label = paste0("2002-2006 \nFive consecutive years with increase in attendance")) +
  annotate("text", x = 2018, y =0.01,fontface ="bold",
           hjust = 0.5, color = "#f08080",
           size = 3, label = paste0("2017-2019 \nThree consecutive years\nwith decrease in attendance")) 





