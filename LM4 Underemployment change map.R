
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

sd <- sd(GA.2$`Underemployment change Sept 2019 - Sept 2020 (%)`) #standard deviation of the data

cols <- c("#adb8e6", #blue
 "#ebc3b9",
"#d18978",
"#b04f3b",
 "#8a0000")

GA.raw <- read_excel("Green Alliance constituency model.xlsx", 
                     sheet = "Results (sorted)", skip = 1)


GA.1 <- GA.raw[,c(1,2,3,5)]
GA.1 <- mutate_if(GA.1 , is.numeric, ~ . * 100) #make percentages
GA.1 <- GA.1 %>% mutate(across(where(is.numeric),round,1))
colnames(GA.1) <- c("Constituency","ONS code","Underemployment % pre-pandemic (16-64)", "Underemployment change Sept 2019 - Sept 2020 (%)")#rename

#geom file
pcons <- readRDS("pcons.RDS")

pcons <- st_transform(pcons, 27700)
pcons <- rmapshaper::ms_simplify(pcons, keep = 0.2)
pcons <- st_transform(pcons, 4326)

GA.2 <- merge(pcons, GA.1, by.x = "pcon19cd", by.y = "ONS code")

#manually code colours
sd <- sd(GA.2$`Underemployment change Sept 2019 - Sept 2020 (%)`) #standard deviation of the data
GA.2$cols <- NA
GA.2$cols[GA.2$`Underemployment change Sept 2019 - Sept 2020 (%)` < 0] <- cols[1] #adb8e6 #blue
GA.2$cols[GA.2$`Underemployment change Sept 2019 - Sept 2020 (%)` > 0] <- cols[2] #ebc3b9
GA.2$cols[GA.2$`Underemployment change Sept 2019 - Sept 2020 (%)` > sd] <- cols[3] #d18978
GA.2$cols[GA.2$`Underemployment change Sept 2019 - Sept 2020 (%)` > sd*2] <- cols[4] #b04f3b
GA.2$cols[GA.2$`Underemployment change Sept 2019 - Sept 2020 (%)` > sd*3] <- cols[5] #8a0000

#this makes the hover over popup label
#labels1 <- sprintf("<strong>%s</strong><br/>Quintile: %s<sup></sup><br/>Take up rate: %s<sup></sup>", pc_data3$LAD20NM.x , pc_data3$Quintile, pc_data3$`Furlough rate at January 31` ) %>% lapply(htmltools::HTML)
labels1 <- sprintf("<strong>%s</strong><br/>Underemployment change %s%%<sup></sup><br/>", 
                   GA.2$pcon19nm , GA.2$`Underemployment change Sept 2019 - Sept 2020 (%)`) %>% 
  lapply(htmltools::HTML)


plot <- leaflet(height = 1600,options= leafletOptions(padding = 100, zoomSnap = 0.25, zoomDelta = 0.3)) %>%
  setView(lng =  -3.13,
          lat = 54.8,zoom = 6.8) %>% #setView gives centre coordinates and zoom level
  
  setMapWidgetStyle(list(background = "white")) %>%
  #addProviderTiles(providers$CartoDB.PositronNoLabels, providerTileOptions(opacity = 1) ) %>%
  
  addPolygons(data = GA.2, stroke = T, color = "white",
              fillColor = ~cols,
              opacity = 1, 
              fillOpacity = 1, weight = 0.25, label = labels1,  
              highlight= highlightOptions(color="white", weight=2, bringToFront= T))  %>%
  
  
  
  addLegend(colors = cols, 
            labels = c(
                      paste0(round(0-sd,0),"% - ",0,"%"),
                       paste0(0,"% - ", round(sd,0),"%"),
                       paste0(round(sd,0), "% - ",round(sd*2,0) ,"%"),
                       paste0(round(sd*2,0), "% - ",round(sd*3,0) ,"%"),
                       paste0(round(sd*3,0),"%+")
                       ),
            labFormat = labelFormat(suffix = "%%"),
            position = "topright",
            title="Underemployment change<br>
            Sept 2019 to Sept 2020 (%)",
            opacity=1) %>%
  
  removeDrawToolbar(clearFeatures = T)


#page element title
title <- tags$div(HTML("Percentage change in underemployment (Sept 2019 to Sept 2020)"), 
                  style = "font-family: Open Sans;color: #2A2A2A;font-weight: bold; font-size: 18px; text-align: center"
)

mapshot(plot, file =  "LM4.png", remove_controls = T)

combo <- htmltools::tagList(title, plot) #I think this makes a combined html object
html_print(combo)
# 
# htmltools::save_html(combo, "index.html") #this saves it as an HTML page in the default folder.
