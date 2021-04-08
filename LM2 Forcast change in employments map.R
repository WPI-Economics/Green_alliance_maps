
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

GA.raw <- read_excel("Green Alliance constituency model.xlsx", 
                     sheet = "Results (sorted)", skip = 1)


GA.1 <- GA.raw[,c(1,2,18,9)]



#geom file
pcons <- readRDS("pcons.RDS")

GA.2 <- merge(pcons, GA.1, by.x = "pcon19cd", by.y = "ONS code")
GA.2$`Forecast percentage change in employments 2019-2025` <- round(GA.2$`Forecast percentage change in employments 2019-2025`*100,1)



# MAP IT OUT
pallette7 <- c("#8a0000",
  "#c98271",
  "#f1f1f1",
  "#cfd4ec",
  "#adb8e6")

#660a0b
#7c2d25
#
#
#
#
#
#
#

#factpal1 <- colorFactor(pallette7, domain = woodland.pcon$Quintiles, reverse= TRUE) #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html
numpal1 <- colorNumeric(palette = pallette7, domain = GA.2$`Forecast percentage change in employments 2019-2025`, reverse = F)


#this makes the hover over popup label
#labels1 <- sprintf("<strong>%s</strong><br/>Quintile: %s<sup></sup><br/>Take up rate: %s<sup></sup>", pc_data3$LAD20NM.x , pc_data3$Quintile, pc_data3$`Furlough rate at January 31` ) %>% lapply(htmltools::HTML)
labels1 <- sprintf("<strong>%s</strong><br/>Forcast change in employment %s<sup></sup><br/>", 
                   GA.2$pcon19nm , GA.2$`Forecast percentage change in employments 2019-2025`) %>% 
  lapply(htmltools::HTML)


plot <- leaflet(height = "800px",options= leafletOptions(padding = 100, zoomSnap = 0.25, zoomDelta = 0.3)) %>%
  setView(lng =  -3.13,
          lat = 54.90,zoom = 6) %>% #setView gives centre coordinates and zoom level
  
  setMapWidgetStyle(list(background = "white")) %>%
  #addProviderTiles(providers$CartoDB.PositronNoLabels, providerTileOptions(opacity = 1) ) %>%
  
  addPolygons(data = GA.2, stroke = T, color = "white",
              fillColor = ~numpal1(GA.2$`Forecast percentage change in employments 2019-2025`),
              opacity = 1, 
              fillOpacity = 1, weight = 0.25, label = labels1,  
              highlight= highlightOptions(color="white", weight=2, bringToFront= T))  %>%
  
  
  
  addLegend(pal = numpal1, 
            values = GA.2$`Forecast percentage change in employments 2019-2025`,
            position = "topright",
            title="Forcast change <br>in employments (%)",
            opacity=1) %>%
  
  removeDrawToolbar(clearFeatures = T)


plot

#page element title
title <- tags$div(HTML("Forecast percentage change in employments 2019-2025 for Westminster Constituencies"), 
                  style = "font-family: Open Sans;color: #2A2A2A;font-weight: bold; font-size: 18px; text-align: center"
)

combo <- htmltools::tagList(title, plot) #I think this makes a combined html object
html_print(combo)
# htmltools::save_html(combo, "index.html") #this saves it as an HTML page in the default folder.


#mapshot(plot, file =  "LM2.png", remove_controls = T)

