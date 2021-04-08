
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


#read pcon.woodland data

woodland.pcon <- read_csv("Woodland within constituencies.csv")

#geom file
pcons <- readRDS("pcons.RDS")


woodland.pcon <- merge(pcons, woodland.pcon, by = 'pcon19cd')

woodland.pcon <- woodland.pcon %>% mutate(Quintiles = ntile(`Woodland area (ha)`,5))
woodland.pcon$Quintiles <- factor(woodland.pcon$Quintiles, levels = c(5,4,3,2,1), labels = c("5 - High","4", "3", "2", "1 - Low"))


# MAP IT OUT
pallette7 <- c("#186fa9", "#518bbe","#7aa8d3", "#a2c6e9", "#c9e5ff")

factpal1 <- colorFactor(pallette7, domain = woodland.pcon$Quintiles, reverse= F, na.color = "#D3D3D3") #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html



#this makes the hover over popup label
#labels1 <- sprintf("<strong>%s</strong><br/>Quintile: %s<sup></sup><br/>Take up rate: %s<sup></sup>", pc_data3$LAD20NM.x , pc_data3$Quintile, pc_data3$`Furlough rate at January 31` ) %>% lapply(htmltools::HTML)
labels1 <- sprintf("<strong>%s</strong><br/>Quintile: %s<sup></sup><br/>Hectares of woodland: %s<sup></sup>", 
                   woodland.pcon$pcon19nm , woodland.pcon$Quintiles, woodland.pcon$`Woodland area (ha)` ) %>% 
  lapply(htmltools::HTML)

plot <- leaflet(height = "800px",options= leafletOptions(padding = 100, zoomSnap = 0.25, zoomDelta = 0.3)) %>%
  setView(lng =  -3.13,
          lat = 54.90,zoom = 6) %>% #setView gives centre coordinates and zoom level
  
  setMapWidgetStyle(list(background = "white")) %>%
  #addProviderTiles(providers$CartoDB.PositronNoLabels, providerTileOptions(opacity = 1) ) %>%
  
  addPolygons(data = woodland.pcon, stroke = T, color = "white",
              fillColor = ~factpal1(woodland.pcon$Quintiles),opacity = 1, 
              fillOpacity = 1, weight = 0.25, label = labels1,  
              highlight= highlightOptions(color="white", weight=2, bringToFront= T))  %>%
  
  
  
  addLegend(pal = factpal1, 
            values = woodland.pcon$Quintiles,
            position = "topright",
            title="Quintiles of woodland area",
            opacity=1) %>%
  
  removeDrawToolbar(clearFeatures = T)


plot

#mapshot(plot, file =  "W1.png", remove_controls = T)

#page element title
title <- tags$div(HTML("Total woodland within Westminster Parliamentary Constituencies"), 
                  style = "font-family: Open Sans;color: #2A2A2A;font-weight: bold; font-size: 18px; text-align: center"
)

combo <- htmltools::tagList(title, plot) #I think this makes a combined html object

htmltools::save_html(combo, "index.html") #this saves it as an HTML page in the default folder.
