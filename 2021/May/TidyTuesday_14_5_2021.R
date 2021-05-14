# Upload the packages -----------------------------------------------------

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, patchwork, hrbrthemes, scales,ggtext, ggpubr,sf,
               grid, gridtext,hrbrthemes,scales,ggtext, ggpubr, biscale, cowplot,sysfonts,ggimage,extrafont,systemfonts, showtext, ggbeeswarm)


# Fonts -------------------------------------------------------------------

extrafont::loadfonts(device = "win", quiet = TRUE)

font_add_google("Quicksand")

font_add_google("Work Sans")

font_labels <- "Quicksand"

font_labels2 <- "Work Sans"

showtext_auto()



# Upload data -------------------------------------------------------------

broadband <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv')

names(broadband)[4] <-"broadbandavailability"
names(broadband)[5]<-"broadbandusage"
names(broadband)[2]<- "id"


# I changed - for 0 to assign colour --------------------------------------

broadband<-broadband%>%
  mutate(broadbandavailability = str_replace(broadbandavailability, "-", "0"),
         broadbandusage = str_replace(broadbandusage, "-", "0"))

broadband$broadbandavailability<-as.numeric(broadband$broadbandavailability)
broadband$broadbandusage<-as.numeric(broadband$broadbandusage)



# Merge with county data --------------------------------------------------

county_us <- read_csv("county_us.csv")

county_us <- county_us %>%select(2:7)
county_us<-county_us%>%add_row(county_map_Oglala_dif2)

county_map_Oglala_dif2 <- read_csv("~/R/county_map_Oglala_dif2.csv")

county_map_Oglala_dif2$id <- recode(county_map_Oglala_dif2$id, "46113" = "46102")
county_map_Oglala_dif2$name <- recode(county_map_Oglala_dif2$name, "Oglala County" = "Oglala Lakota County")


county_map_Oglala_dif2<-county_map_Oglala_dif2%>%select(2:7)

county_map_Oglala_dif2$group<-as.character(county_map_Oglala_dif2$group)
county_map_Oglala_dif2$id<-as.character(county_map_Oglala_dif2$id)


county_us<-county_us%>%add_row(county_map_Oglala_dif2)

county_us$id <- as.numeric(county_us$id)


broadband_full <- left_join(broadband, county_us, by = "id")



# Bi-variate --------------------------------------------------------------

broadband_full<-broadband_full%>% bi_class(x = broadbandusage, y =broadbandavailability, style = "quantile", dim = 3) 
  

# Graph -------------------------------------------------------------------


pA <- ggplot(data = broadband_full,
             mapping = aes(x = long, y = lat,
                           fill = bi_class, 
                           group = group,
                           label = ST))

p1A <- pA + geom_polygon(color = "gray90", size = 0.05) + coord_equal()

graphA<-p1A + 
  theme_map() +
  bi_scale_fill(pal = "GrPink", dim = 3, guide = F) +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  theme(
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#ffffff",face = "bold",size = 9,
                              hjust = 0.5,
                              family = font_labels),
    plot.subtitle = element_text(margin = margin(t=10,b = 25), 
                                 color = "#ffffff", size = 6, family = font_labels,
                                 hjust = 0.5),
    plot.caption =  element_text(margin = margin(t = 20), 
                                 color = "#ffffff", size = 5, family = font_labels,
                                 hjust = 0.95),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    #legend.position = "none",
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "#fbfaf6", color = NA), 
    plot.background = element_rect(fill = "#fbfaf6", color = NA),    # color removes the border,
    panel.border = element_blank(),
    plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  ) 

# Legend ------------------------------------------------------------------

legend_TT2 <- 
  bi_legend(pal = "GrPink",
            dim = 3,
            xlab = "Broadband usage",
            ylab = "Broadband availability",
            size = 2.5) +
  theme(rect = element_rect(fill = "grey10"),
        panel.border = element_blank(),
        axis.text = element_blank(),
        plot.background = element_rect(fill = "#fbfaf6", color = NA),
        panel.background = element_rect(fill = "#fbfaf6", color = NA), 
        axis.title.x = element_text(size = 10,family = font_labels,
                                    color = "#22222b"),
        axis.title.y = element_text(size = 10,family = font_labels,
                                    color = "#22222b"),
        legend.text = element_text(size = 5,family = font_labels),
        legend.text.align = 0)


# Cowplot -----------------------------------------------------------------


map_legend_TT<- ggdraw() +
  draw_plot(graphA, 0, 0, 1, 1) +
  draw_plot(legend_TT2, 0, 0.1, 0.2, 0.2) +
  draw_label("Source: #TidyTuesday\nVisualization: JuanmaMN (Twitter @Juanma_MN)", fontfamily= font_labels,
             color = "#22222b", size = 7.5, angle = 0, x = 0.9, y = 0.05) +
  draw_label("US Broadband", fontfamily = font_labels,fontface= "bold",
             color = "#22222b", size = 16, angle = 0, x =0.1, y = 0.97) + 
  draw_label("Percent of people per county that use the internet at broadband speeds of 25 Mbps/3 Mbps as of the end of 2017 and with access to fixed terrestrial broadband", 
             fontfamily = font_labels,fontface= "plain",
             color = "#22222b", size = 10, angle = 0.5, x =0.3152, y = 0.94) + 
  theme(panel.background = element_rect(fill = "#fbfaf6", color = NA))
