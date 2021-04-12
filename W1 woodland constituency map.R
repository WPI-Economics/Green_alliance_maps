
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

cols <- c("#186fa9", "#518bbe","#7aa8d3", "#a2c6e9", "#c9e5ff")
          #"#E0E0E0")
#read pcon.woodland data

woodland.pcon <- read_csv("Woodland within constituencies.csv")

#geom file
pcons <- readRDS("pcons.RDS")
#remove NI
pcons <- pcons %>% filter(str_detect(pcon19cd, "N", negate = T))


woodland.pcon <- merge(pcons, woodland.pcon, by = 'pcon19cd', all.x = T)
woodland.pcon$`Woodland area (ha)`[is.na(woodland.pcon$`Woodland area (ha)`)] <- 0 #make na values 0

woodland.pcon <- woodland.pcon %>% arrange(`Woodland area (ha)`)
woodland.pcon <- woodland.pcon %>% mutate(cumsum_ha = cumsum(`Woodland area (ha)`)) #cumulative sum of hectares of woodland
woodland.pcon <- woodland.pcon %>% arrange(`Woodland area (ha)`)
woodland.pcon <- woodland.pcon %>% mutate(cumsum_pc = cumsum((`Woodland area (ha)`/sum(`Woodland area (ha)`)*100)))  #cumulative sum of hectares of woodland as percentage

woodland.pcon$`Cumulative % share of woodland` <- NA
woodland.pcon$`Cumulative % share of woodland`[woodland.pcon$cumsum_pc >80] <- 1
woodland.pcon$`Cumulative % share of woodland`[woodland.pcon$cumsum_pc <= 80] <-2 
woodland.pcon$`Cumulative % share of woodland`[woodland.pcon$cumsum_pc <= 60] <- 3
woodland.pcon$`Cumulative % share of woodland`[woodland.pcon$cumsum_pc <= 40] <- 4
woodland.pcon$`Cumulative % share of woodland`[woodland.pcon$cumsum_pc <= 20] <- 5
woodland.pcon$`Cumulative % share of woodland` <- factor(woodland.pcon$`Cumulative % share of woodland`,levels = c(1:5),
                                                         labels = 
                                                           c("Most concentrated cumulative 20% of all woodland",
                                                             "60-80",
                                                             "40-60",
                                                             "20-40",
                                                             "Least concentrated cumulative 20% of all woodland"))




# MAP IT OUT
#pallette7 <- c("#186fa9", "#518bbe","#7aa8d3", "#a2c6e9", "#c9e5ff")

factpal1 <- colorFactor(cols, domain = woodland.pcon$`Cumulative % share of woodland`, reverse= F) #"Set3 is a colorbrewer preset https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html



#this makes the hover over popup label
#labels1 <- sprintf("<strong>%s</strong><br/>Quintile: %s<sup></sup><br/>Take up rate: %s<sup></sup>", pc_data3$LAD20NM.x , pc_data3$Quintile, pc_data3$`Furlough rate at January 31` ) %>% lapply(htmltools::HTML)
labels1 <- sprintf("<strong>%s</strong><br/>%s<sup></sup><br/>Hectares of woodland: %s<sup></sup>", 
                   woodland.pcon$pcon19nm , 
                   woodland.pcon$`Cumulative % share of woodland`, 
                   format(round(woodland.pcon$`Woodland area (ha)`,0),big.mark = ",")
                   ) %>% 
  lapply(htmltools::HTML)

plot <- leaflet(height = "800px",options= leafletOptions(padding = 100, zoomSnap = 0.25, zoomDelta = 0.3)) %>%
  setView(lng =  -3.13,
          lat = 54.90,zoom = 6) %>% #setView gives centre coordinates and zoom level
  
  setMapWidgetStyle(list(background = "white")) %>%
  #addProviderTiles(providers$CartoDB.PositronNoLabels, providerTileOptions(opacity = 1) ) %>%
  
  addPolygons(data = woodland.pcon, stroke = T, color = "white",
              fillColor = ~factpal1(woodland.pcon$`Cumulative % share of woodland`),opacity = 1, 
              fillOpacity = 1, weight = 0.25, label = labels1,  
              highlight= highlightOptions(color="white", weight=2, bringToFront= T))  %>%
  
  
  
  addLegend(pal = factpal1, 
            values = woodland.pcon$`Cumulative % share of woodland`,
            position = "topright",
            title="Distribution of woodland area",
            opacity=1) %>%
  
  removeDrawToolbar(clearFeatures = T)


#plot

#mapshot(plot, file =  "W1.png", remove_controls = T)

#page element title
title <- tags$div(HTML("Total woodland within Westminster Parliamentary Constituencies"), 
                  style = "font-family: Open Sans;color: #2A2A2A;font-weight: bold; font-size: 18px; text-align: center"
)

combo <- htmltools::tagList(title, plot) #I think this makes a combined html object

htmltools::save_html(combo, "index.html") #this saves it as an HTML page in the default folder.
