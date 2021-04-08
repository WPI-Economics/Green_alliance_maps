#Seagrass areas by constituency

library(tidyverse)
library(readxl)
library(sf)
library(leaflet)
library(leaflet.extras)

`%nin%` <- Negate(`%in%`)

##Contemporary data
sg.contemp <- read_excel("Post 1997_Contemporary.geocoded CBR .xlsx", 
                         na = "NA")


#fill in long lats where same area

longlats <- sg.contemp %>% select(Location, PlaceName, lat, lon) %>% 
  na.omit()

sg.contemp2 <- sg.contemp %>% filter(is.na(lat)) %>% select(-lat, -lon)
sg.contemp <- sg.contemp %>% filter(!is.na(lat))

#replace NA long lats from longlats file
sg.contemp3 <- merge(sg.contemp2, longlats, by = c("Location", "PlaceName"), all.x = T)

sg.contemp <- bind_rows(sg.contemp, sg.contemp3)

#remove Ireland example
sg.contemp <- sg.contemp %>% filter(Area != "Ireland")
sg.contemp <- sg.contemp %>% select(Location,Area ,PlaceName, Area_Ha, lat, lon)
rm(sg.contemp2, sg.contemp3, longlats)

#summarise data
sg.contemp.sum <- sg.contemp %>%  group_by(PlaceName, Location, Area, lat, lon) %>% summarise(Area_Ha = sum(Area_Ha))
sg.contemp.sum <- sg.contemp.sum %>% mutate(source = "Contemporary")

##Historic data
sg.historic  <- read_excel("Pre 1998_Historic.geocoded CBR.xlsx", 
                             na = "NA")

sg.historic <- sg.historic %>% select(PlaceName, Area, Area_Ha = `Contemporary area`, lat, lon) %>% mutate(Location = NA, source = "Historic")

#Merge contemporary and historic files
sg.all <- bind_rows(sg.contemp.sum, sg.historic)
sg.all <- sg.all %>% drop_na(lon)

#make spatial
sg.all <- st_as_sf(sg.all, coords = c("lon","lat"))
st_crs(sg.all) <- 4326 #set CRS

leaflet(data = sg.all[sg.all$in_uk == F,]) %>% 
  addTiles() %>% 
  addCircleMarkers( radius = 0.5)


#IDENTIFY long lats that have been geocoded wrong by subsetting if outsode UK boundary
uk.bounds <- geojsonio::geojson_sf("https://opendata.arcgis.com/datasets/b789ba2f70fe45eb92402cee87092730_0.geojson") #Countries (December 2019) Boundaries UK BGC
uk.bounds2 <- uk.bounds %>% filter(ctry19nm != "Northern Ireland")
uk.bounds2 <- st_union(uk.bounds2)

sg.all$in_uk <- st_is_within_distance(sg.all, uk.bounds2, dist = 5000, sparse = F)

sg.errors <- sg.all %>% filter(in_uk == F)
sg.all.ok <- sg.all %>% filter(in_uk == T)
#write.csv(sg.errors, file = "Geocode errors seagrass.csv", row.names = F)
#read in corrected data
sg.corrected <- read_csv("Geocode errors seagrass.csv")
sg.corrected <- sg.corrected %>% filter(!is.na(`lat`))
sg.corrected <- sf::st_as_sf(sg.corrected, coords = c("lon", "lat"), crs = 4326)

sg.all.corrected <- bind_rows(sg.all.ok, sg.corrected)



leaflet(data = sg.all.corrected) %>% 
  addTiles() %>% 
  addCircleMarkers( radius = 0.5)

#saveRDS(sg.all.corrected, file = "sg.all.corrected.RDS")


#second round of errors to clean

wronguns <- c("Forth Estuary",
              "STOUR",
              "The Fleet",
              "Lindisfarne",
              "Thorpe Bay",
              "Poole Harbour",
              "THAMES LOWER",
              "East Coast Scotland",
              "Portsmouth Harbour",
              "Sunart",
              "North Nofolk AONB" ,
              "Torbay",
              "ORWELL",
              "Scilly Isles")


wronguns2 <- c(
  "Blackness",
  "stour",
  NA,
  "Spurn",
  "Burgess Terrace",
  "Newton Bay",
  "Allhallows",
  "Tay Estuary",
  "St Helen's",
"Sunart",
"Wells",
"Torre Abbey",
"Orwell",
"Scilly Isles"
)

sg.wronguns <- sg.all.corrected %>% filter(PlaceName %in% wronguns)
sg.wronguns <- sg.wronguns %>% filter(tolower(Location) %in% tolower(wronguns2))

remove <- sg.wronguns %>% filter(PlaceName == "Lindisfarne")
sg.wronguns <- sg.wronguns %>% filter(PlaceName != "Lindisfarne")
sg.wronguns <-  bind_rows(sg.wronguns, remove[1,])
sg.wronguns$temp <- paste0(sg.wronguns$PlaceName, sg.wronguns$Location, sg.wronguns$Area)

sg.all.corrected$temp <- paste0(sg.all.corrected$PlaceName, sg.all.corrected$Location, sg.all.corrected$Area)

sg.rightuns <- sg.all.corrected %>% filter(temp %nin% sg.wronguns$temp)

sg.rightuns$temp <- NULL
sg.all.corrected$temp <- NULL
sg.wronguns$temp <- NULL
#write.csv(sg.wronguns, file = "wronguns.csv", row.names = F)


sg.corrected2 <- read_csv("wronguns.csv")
sg.corrected2 <- sg.corrected2 %>% filter(!is.na(`lat`))
sg.corrected2 <- sf::st_as_sf(sg.corrected2, coords = c("lon", "lat"), crs = 4326)

sg.all.corrected <- bind_rows(sg.rightuns, sg.corrected2)

sg.all.corrected <- sg.all.corrected[,1:6]

saveRDS(sg.all.corrected, file = "sg.all.corrected.RDS")


#fix Holy island
sg.wronguns2 <- sg.all.corrected %>% filter(Location %in% "Holy Island")
sg.rightuns2 <- sg.all.corrected %>% filter(Location %nin% "Holy Island")

write.csv(sg.wronguns2, file = "wronguns2.csv", row.names = F)

sg.corrected3 <- read_csv("wronguns2.csv")
sg.corrected3 <- sg.corrected3 %>% filter(!is.na(`lat`))
sg.corrected3 <- sf::st_as_sf(sg.corrected3, coords = c("lon", "lat"), crs = 4326)

sg.all.corrected <- bind_rows(sg.rightuns2, sg.corrected3)

saveRDS(sg.all.corrected, file = "sg.all.corrected.RDS")
