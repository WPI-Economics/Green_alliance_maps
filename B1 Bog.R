library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(webshot)
library(pdftools)
library(raster)

# bog.img <- brick("The Great North Bog_2.png" )
# plotRGB(bog.img)

bog.coords <- read_csv("Bog NSEW coords.csv")
bog.coords <- pivot_longer(bog.coords, cols = 2:5, values_to = "geometry", names_to = "NSEW")
bog.coords <- bog.coords %>% separate(geometry, c("lon", "lat"), sep = ", ", convert = T)

bog.coords <- st_as_sf(bog.coords, coords = c("lat", "lon"))
st_crs(bog.coords) <- 4326



ttwa.bounds <- geojson_sf("https://opendata.arcgis.com/datasets/60637d04599242c988898dc154048e52_0.geojson")

#check all ok map!
# leaflet() %>% addTiles() %>%
# 
#   addPolygons(data = ttwa.bounds, fillOpacity = 0.3, opacity = 1, weight = 0.2, fillColor = "red",color = "#E0E0E0") %>%
# 
#   addCircleMarkers(data = bog.coords, radius = 3, stroke = F,
#                    opacity = 0.85, fillOpacity = 0.85)
              
             
#filter to just the TTWAs that intersect with bog points
bog.coords <- st_transform(bog.coords,27700)
ttwa.bounds2 <- st_transform(ttwa.bounds,27700)
ttwa.bog <- st_intersection(bog.coords,ttwa.bounds2)

bog.coords <- st_transform(bog.coords,4326)
ttwa.bounds2 <- st_transform(ttwa.bounds,4326)
ttwa.bog <- st_transform(ttwa.bog, 4326)

ttwa.bog2 <- ttwa.bounds %>% filter(TTWA11CD %in% ttwa.bog$TTWA11CD)


leaflet() %>% addTiles() %>%
  
  addPolygons(data = ttwa.bog2, fillOpacity = 0.3, opacity = 1, weight = 0.2, fillColor = "red",color = "#E0E0E0") %>%
  
  addCircleMarkers(data = bog.coords, radius = 3, stroke = F,
                   opacity = 0.85, fillOpacity = 0.85)

pcons <- readRDS('pcons.RDS')
pcons <- st_transform(pcons, 27700)
ttwa.bog2 <- st_transform(ttwa.bog2,27700)
#pcons$bog.ttwa <- st_is_within_distance(pcons, ttwa.bog2, dist = 100, sparse = F)
pcons$bog.ttwa <- st_intersects(ttwa.bog2, pcons, sparse = F)
test <- st_intersection(ttwa.bog2, pcons)

pcons <- st_transform(pcons, 4326)
test <- st_transform(test, 4326)


#check all ok map!
pcons.bog <- pcons %>% filter(bog.ttwa == T)

leaflet() %>% addTiles() %>%

  addPolygons(data = test, fillOpacity = 0.3, opacity = 1, weight = 0.4, fillColor = "red",color = "#F1F1F1", popup = test$pcon19nm, 
              highlight= highlightOptions(color="white", weight=2, bringToFront= T)) %>%

  addCircleMarkers(data = bog.coords, radius = 3, stroke = F,
                   opacity = 0.85, fillOpacity = 0.85)


nat.parks <- geojson_sf("https://opendata.arcgis.com/datasets/6ed787ab793c449b9e036bf88f6e55b0_0.geojson")
