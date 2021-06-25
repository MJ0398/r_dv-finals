#Week 12
#week12_chart type1-leaflet ----
leaflet() %>% addTiles() %>% print
library(leaflet)
df <- data.frame(lat = runif(20, min=37.5, max= 37.6),
                 lng = runif(20, min =127, max = 127.1),
                 col = sample(c("#DB7093", "#708090", "#808000"), 20, replace =TRUE))
#locating Gangnam on map
gangnam<- leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = 127.047325, lat = 37.517235, popup = "Gangnam Style") %T>%  print()
#setting popups
popups<- sample(c("Fitness Center", "Hospital", "Health Center"), replace = TRUE)
#creating final leaflet
leaflet <- df %>% leaflet() %>%
  addTiles() %>% 
  # addMarkers(clusterOptions= markerClusterOptions(),popup = popups) %>% 
  addCircleMarkers(color = df$col) %>% 
  addLegend(labels = c("Fitness Center", "Hospital", "Health Center"), 
            color = c("pink", "grey", "olive"))
##Output: topic2_leaflet-week12
leaflet

#week12_chart type2-mapdata ----
install.packages("mapproj")
library(mapproj)
KCJ <- map_data(map = 'world',
                    region = c('South Korea', 'North Korea', 'China',
                               'Japan'))
KCJ %>% view
# mapping Korea,China Japan

basemap1 <- ggplot(KCJ, aes(x= long,
                                 y= lat, 
                                 group = group)) +
  geom_polygon(
    data = KCJ,
    aes(x = long, y = lat, group = group) ,
    colour = "black",
    size = .25
  )

##Output_basemap1-week12 (overall basemap on three countries)
basemap1

# bringing in data on GDP, GDP per capita from IMF
statistics <- 
  data.frame(
    region = c("China", "Japan", "South Korea", "North Korea"),
    perGdp = c(8643, 38440, 29891, 665)) #data extracted from IMF

KCJdata <- merge(x = KCJ,
                y = statistics,
                by = "region",
                all.x = TRUE)

KCJdata <- NE.Asia[order(NE.Asia$region, NE.Asia$order), ]
KCJdata %>% view

# A new base plot based on new data
basemap2 <- ggplot(data = KCJdata,
       mapping = aes(x = long,
                     y = lat,
                     group = group)) + 
  # coord_equal()+ (better without)
  geom_polygon(mapping = aes(fill = perGdp),
               color = 'black')
##Output_basemap2-week12 (newbasemap with perGDP info)
basemap2

##finalmap
finalmap1<-basemap2 + 
  ggtitle("GDP per capita in South Korea, North Korea, China and Japan", subtitle = "GDP per capita/USD") + 
  theme(legend.position = c(0.9, 0.1),
        legend.text = element_text(size = 8, face = 'bold'),
        legend.title = element_text(size = 10, face = 'bold')) +
  coord_map()
##Output: topic2_finalmap1-week12

#week12_chart type3-rworldmap (substitute for rtweet in week 10)
install.packages("rworldmap")
library(rworldmap)
map.world <- getMap(resolution = "low") #high resolution needs package rworldxtra
map.world %>% plot

indicatorName <- "Life expectancy at birth, total (years)"
indicatorYear <- 2013

indicators <- read_csv("../input/Indicators.csv")

filtered <- indicators[indicators$IndicatorName==indicatorName & indicators$Year==indicatorYear,]
write_csv(filtered, "filtered.csv")

sPDF <- joinCountryData2Map(filtered,
                            joinCode = "ISO3",
                            nameJoinColumn = "CountryCode",
                            verbose = TRUE)

png("map.png", width=900, height=550)
mapCountryData(sPDF, nameColumnToPlot='Value', mapTitle=indicatorName)
dev.off()