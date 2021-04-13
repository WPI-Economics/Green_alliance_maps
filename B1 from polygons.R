library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(webshot)
library(pdftools)
library(raster)
library(geojsonsf)
library(rmapshaper)
library(sf)

bog.coords <- read_csv("Bog NSEW coords.csv")

# aonb <- geojson_sf("https://opendata.arcgis.com/datasets/6f2ad07d91304ad79cdecd52489d5046_0.geojson")
# ons.national.parks <- geojson_sf("https://opendata.arcgis.com/datasets/6ed787ab793c449b9e036bf88f6e55b0_0.geojson")
# 
# aonb <- ms_simplify(aonb, keep = 0.05)
# ons.national.parks <- ms_simplify(ons.national.parks, keep = 0.05)
# 
# saveRDS(aonb, file = "aonb.RDS")
# saveRDS(ons.national.parks, file = "ons.national.parks.RDS")

aonb <- readRDS(file = "aonb.RDS")
ons.national.parks <- readRDS(file = "ons.national.parks.RDS")

national.character.areas <- geojson_sf("https://opendata.arcgis.com/datasets/9185e7efe65f4e47b4a722446c061e62_0.geojson")

t1 <- aonb %>% filter(tolower(NAME) %in% tolower(bog.coords$`Bog areas`))
t2 <- ons.national.parks %>% filter(tolower(gsub(" National Park","",npark18nm)) %in% tolower(bog.coords$`Bog areas`))

s.pennines <- national.character.areas %>% filter(FID == 36)
s.pennines <- s.pennines[,c(1,5)]
colnames(s.pennines) <- c("NAME", "geometry")
s.pennines <- st_transform(s.pennines, 27700)

t1 <- st_transform(t1, 27700)
t2 <- st_transform(t2, 27700)

t1 <- t1[,c(5,9)]
t2 <- t2[,c(3,11)]
colnames(t2) <- c("NAME", "geometry")

all <- bind_rows(t1, t2)
all <- bind_rows(all, s.pennines)

all <- st_union(all,by_feature = F )


pcons <- readRDS('pcons.RDS')
pcons <- st_transform(pcons, 27700)

pcons$near.bog <- st_is_within_distance(pcons, all, dist = 0.363, sparse = F) # 1 degree is 55.051 miles so 20 miles is 0.363 degrees

pcons <- st_transform(pcons, 4326)

pcons.bog <- pcons %>% filter(near.bog == T)

leaflet() %>% addTiles() %>%
  addPolygons(data = pcons.bog, fillOpacity = 0.3, opacity = 1, weight = 0.2, fillColor = "red",color = "#E0E0E0")

pcons.bog.df <- pcons[,c(1,2,11)]
pcons.bog.df <- st_drop_geometry(pcons.bog.df)

write.csv(pcons.bog.df, file = "Constituencies near bog.csv")


