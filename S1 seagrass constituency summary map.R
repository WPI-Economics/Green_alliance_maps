#Seagrass areas by constituency

library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)

sg.all.corrected <- readRDS(file = "sg.all.corrected.RDS")
sg.all.corrected <- sg.all.corrected %>% filter(!(PlaceName == "The Fleet" & source == "Contemporary")) #remove broken entry for The Fleet



pcons <- readRDS("pcons.RDS")



#change CRS 
pcons <- st_transform(pcons, 27700)
pcons <- st_simplify(pcons, dTolerance = 10)

sg.all.corrected27700 <- st_transform(sg.all.corrected, 27700)

#aggregate area per Pcon.
t5 <- st_buffer(sg.all.corrected27700, 500) %>%  st_set_precision(1e5) %>%  st_intersection(pcons) #buffer should be in meters

sg.all.corrected27700$nearestpcon.rowid <-st_nearest_feature(sg.all.corrected27700, pcons)
pconslook <- pcons %>%  st_drop_geometry() %>%  as_tibble()
pconslook <- pconslook[,c(1,2)]
sg.all.corrected27700 <- merge(sg.all.corrected27700, pconslook, by.x = "nearestpcon.rowid", by.y = 0) #by.y = 0 matches against rownames



 pcon.sg <- sg.all.corrected27700 %>% group_by(pcon19nm, pcon19cd) %>% summarise('Seagrass hectares' = round(sum(Area_Ha, na.rm = T),1))
 pcon.sg <- st_drop_geometry(pcon.sg)




# pcon.sg <- t5 %>% group_by(pcon19nm, pcon19cd) %>% summarise('Seagrass hectares' = round(sum(Area_Ha, na.rm = T),1))
# pcon.sg <- st_drop_geometry(pcon.sg)

write.csv(pcon.sg, file = "Seagrass hectares in Constituencies.csv", row.names = F)

test <- st_transform(t5, 4326)
test <- st_as_sf(test, coords = c("lon","lat"))


pcons <- st_transform(pcons, 4326)



leaflet(data = sg.all.corrected) %>% 
  addTiles() %>% 
  
  setMapWidgetStyle(list(background = "white")) %>%
  # addTiles() %>% 
  addPolygons(data = pcons, stroke = T, color = "red", fillColor = "blue",
              fillOpacity = 0, weight = 1
  ) %>% 
  addCircleMarkers(radius = 0.5, popup = paste0(sg.all.corrected$PlaceName,sg.all.corrected$`Location`,"<br>",sg.all.corrected$`Area`))
