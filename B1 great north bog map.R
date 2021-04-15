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

cols <- c("#f0cfc7",
          "#dca091",
          "#c4725f",
          "#a94330",
          "#8a0000")

bog.areas <- readRDS("Great north bog areas.rds")

pcons.near.bog <- readRDS(file = "pcons.GN.bog.rds")

lmchallenge <- readRDS(file = "LMChallenge.sf.RDS")
lmchallenge <- st_drop_geometry(lmchallenge)


#merge LM challenge scores into bog pcons.

pcons.near.bog <- merge(pcons.near.bog, lmchallenge[,c(1,11,12,13)], by = "pcon19cd", all.x = T)

pcons.near.bog$`Relative labour market challenge score (100 = mean average constituency)` <- 
  round(pcons.near.bog$`Relative labour market challenge score (100 = mean average constituency)` ,0)


#this makes the hover over popup label
labels1 <- sprintf("<strong>%s</strong><br/>Challenge score: %s<sup></sup><br/>", 
                   GA.2$pcon19nm , round(GA.2$`Relative labour market challenge score (100 = mean average constituency)`),0) %>% 
  lapply(htmltools::HTML)

plot <- leaflet(height = 1600,options= leafletOptions(padding = 100, zoomSnap = 0.25, zoomDelta = 0.3, zoomControl = F)) %>%
  setView(lng =  -3.13,
          lat = 54.8,zoom = 6.8) %>% #setView gives centre coordinates and zoom level
  
  setMapWidgetStyle(list(background = "white")) %>%
  #addProviderTiles(providers$CartoDB.PositronNoLabels, providerTileOptions(opacity = 1) ) %>%
  
  addPolygons(data = pcons.near.bog, stroke = T, color = "white",
              fillColor = ~cols,
              opacity = 1, 
              fillOpacity = 1, weight = 0.25, label = labels1,  
              #highlight= highlightOptions(color="white", weight=2, bringToFront= T)
  )  %>%
  
  
  
  addLegend(colors = rev(cols), 
            labels = c(
              paste0("Very high (", very.high.sht,")"), 
              paste0("High (",high.sht,")"), 
              paste0("Average (",average.sht,")"), 
              paste0("Low (", low.sht,")"),
              paste0("Very low (",very.low.sht,")")
            ),
            position = "topright",
            title="Labour market challenge score",
            opacity=1) %>% 

  removeDrawToolbar(clearFeatures = T)


plot

