install.packages("tidyverse")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("gganimate")
install.packages("maps")
install.packages("leaflet")
install.packages("ggalt")
library(tidyverse)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(ggalt)

#COVID WORLDMAP----

##Retrieve & Clean Dataset----
###source: https://www.kaggle.com/imdevskp/corona-virus-report
read.csv("topic1_covid_data.csv") %>% view #will remove 7:10 col
covid <- read.csv("topic1_covid_data.csv")
df.covid <- select(covid, -c(7:10)) %>% 
              group_by(Country.Region)#dplyr
df.covid %>% view #removed
##Creating Basemap----
map.world <-map_data("world")

basemap1 <- ggplot() + 
  theme(legend.position = "none") +
  ggtitle("Confirmed cases of COVID-19 worldwide") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5)) +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group)) +
  # geom_point(data = df.covid, aes(x = Long, y = Lat, color = "red")) +
  coord_equal()
##Output: basemap 1
basemap1 #too much data ink, point sizes are not intuitive, borders needed, long & lat info not necessary

basemap2 <- ggplot() +
  geom_polygon(
      data = map.world,
      aes(x = long, y = lat, group = group) ,
      fill = "#FFFFFF",
      colour = "black",
      size = .25
  ) +
scale_size_continuous(
    range = c(1, 25),
    breaks = c(100, 1000, 10000, 100000),
    name = "Worldwide Cases of Confirmed COVID-19 in 2020 (Number of Cases)",
    labels = c("100", "1,000", "10,000", "100,000")
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(.38,1.058), 
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_text(color= "black", size = 13),
    legend.text = element_text(color = "black", size = 9),
  ) +
  coord_equal()

##Output: basemap 2
basemap2
##Creating Finalmap----
##Output: finalmap
finalmap <- basemap2 +
  geom_point(data = df.covid, 
             aes(x= Long, y= Lat,size = Confirmed),
             color = "#6A5ACD", alpha = .10)
finalmap