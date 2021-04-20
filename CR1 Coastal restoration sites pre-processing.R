#coastal restoration

# Identify constituencies with any coastal restoration site (353), and any priority sites (72)

library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(raster)
library(geojsonio)
library(leaflet)
library(leaflet.extras)
library(rmapshaper)
library(rgeos)
library(maptools)
library(units)
library(htmltools)
library(readxl)

coastal.resto <- geojson_sf("https://opendata.arcgis.com/datasets/59fe52ba9f8744e88eca9cec371fae30_0.geojson")
priority.list <- read_excel("Coastal restoration locations.xlsx")
priority.list <- priority.list[,c(1:6)]


x <- priority.list$Site_Name[priority.list$`Priority location?` == "Yes"]
y <- coastal.resto$Site_Name[coastal.resto$Priority =="Y"]


intersect(x,y)
setdiff(x,y) #only one area not included in excel file, so manually changed in coastal.resto data
coastal.resto$Priority[coastal.resto$Site_Name == setdiff(x,y)] <- "Y"



rm(priority.list)

coastal.resto <- st_transform(coastal.resto, 27700)
pcons <- readRDS("pcons.RDS")
#remove NI
pcons <- pcons %>% filter(str_detect(pcon19cd,"N0", negate = T))
pcons <- st_transform(pcons, 27700)

#identify PCONS sites sit within
coastal.resto$nearestpcon.rowid <-st_nearest_feature(coastal.resto, pcons)

pconslook <- pcons %>%  st_drop_geometry() %>%  as_tibble()
pconslook <- pconslook[,c(1,2)]
coastal.resto <- merge(coastal.resto, pconslook, by.x = "nearestpcon.rowid", by.y = 0) #by.y = 0 matches against rownames

pcons.summary <- coastal.resto %>% group_by(pcon19nm, pcon19cd) %>% 
  summarise(`Priority sites (count)` = sum(Priority == "Y"), 
            `All sites (count)` = sum(Priority %in% c("Y", "N") ) 
            )
pcons.summary2 <- st_drop_geometry(pcons.summary)
pcons2 <- st_drop_geometry(pcons)

pcons.summary2 <- merge(pcons.summary2[,c(2,3,4)], pcons2[,c(1,2)], by = "pcon19cd", all.y = T)

#save data
write.csv(pcons.summary2, file = "Coastal restoration by constituency.csv", row.names = F)


#test mao
pcons %>% filter(pcon19cd %in% pcons.summary$pcon19cd) %>%  ggplot2::ggplot()+ geom_sf(color = "grey", fill = "green") + geom_sf(data = coastal.resto)
