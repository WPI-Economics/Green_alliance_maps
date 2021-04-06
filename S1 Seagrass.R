#Seagrass areas by constituency

library(tidyverse)
library(readxl)
library(sf)
library(leaflet)
library(leaflet.extras)

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
rm(sg.contemp2, sg.contemp3, sg.contemp4, longlats)

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

leaflet(data = sg.all) %>% 
  addTiles() %>% 
  addCircleMarkers( radius = 0.5)

######## constituency data ##########

constituencies 