
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

cols <- c("#f0cfc7",
          "#dca091",
          "#c4725f",
          "#a94330",
          "#8a0000")

GA.raw <- read_excel("Green Alliance constituency model.xlsx", 
                     sheet = "Results (sorted)", skip = 1)


GA.1 <- GA.raw[,c(1,2,3,5)]
GA.1 <- mutate_if(GA.1 , is.numeric, ~ . * 100) #make percentages
GA.1 <- GA.1 %>% mutate(across(where(is.numeric),round,1))
colnames(GA.1) <- c("Constituency","ONS code","Underemployment % pre-pandemic (16-64)", "Underemployment change Sept 2019 - Sept 2020 (%)")#rename

#geom file
pcons <- readRDS("pcons.RDS")

GA.2 <- merge(pcons, GA.1, by.x = "pcon19cd", by.y = "ONS code")




#manually code colours
sdval <- sd(GA.2$`Underemployment % pre-pandemic (16-64)`) #standard deviation of the data
meanval <- mean(GA.2$`Underemployment % pre-pandemic (16-64)`) 

GA.2$cols <- NA
GA.2$cols[GA.2$`Underemployment % pre-pandemic (16-64)` > meanval + (sdval*1.5)] <- cols[5] #8a0000 greater than 1.5 SD from mean VERY HIGH
GA.2$cols[GA.2$`Underemployment % pre-pandemic (16-64)` < meanval + (sdval*1.5)] <- cols[4] #b04f3b between +0.5 and +1.5 SD of mean HIGH
GA.2$cols[GA.2$`Underemployment % pre-pandemic (16-64)` < meanval + (sdval*0.5)] <- cols[3] #d18978 between -0.5 and +0.5 SD of mean AVERAGE              
GA.2$cols[GA.2$`Underemployment % pre-pandemic (16-64)` < meanval - (sdval*0.5)] <- cols[2] #ebc3b9 between -1.5 and -0.5 SD from mean LOW
GA.2$cols[GA.2$`Underemployment % pre-pandemic (16-64)` < meanval - (sdval*1.5)] <- cols[1] #ebc3b9 LT -1.5 SD from mean VERY LOW


#this makes the hover over popup label
#labels1 <- sprintf("<strong>%s</strong><br/>Quintile: %s<sup></sup><br/>Take up rate: %s<sup></sup>", pc_data3$LAD20NM.x , pc_data3$Quintile, pc_data3$`Furlough rate at January 31` ) %>% lapply(htmltools::HTML)
labels1 <- sprintf("<strong>%s</strong><br/>Underemployment pre-pandemic (16-64) %s%%<sup></sup><br/>", 
                   GA.2$pcon19nm , GA.2$`Underemployment % pre-pandemic (16-64)`) %>% 
  lapply(htmltools::HTML)


plot <- leaflet(height = "800px",options= leafletOptions(padding = 100, zoomSnap = 0.25, zoomDelta = 0.3)) %>%
  setView(lng =  -3.13,
          lat = 54.90,zoom = 6) %>% #setView gives centre coordinates and zoom level
  
  setMapWidgetStyle(list(background = "white")) %>%
  #addProviderTiles(providers$CartoDB.PositronNoLabels, providerTileOptions(opacity = 1) ) %>%
  
  addPolygons(data = GA.2, stroke = T, color = "white",
              fillColor = ~cols,
              opacity = 1, 
              fillOpacity = 1, weight = 0.25, label = labels1,  
              highlight= highlightOptions(color="white", weight=2, bringToFront= T))  %>%
  
  
  # 
  # addLegend(className = "panel panel-default legend", 
  #   pal = numpal1, 
  #           bins = 5,
  #           labFormat = labelFormat(suffix = "%"),
  #           values = GA.2$`Underemployment % pre-pandemic (16-64)`,
  #           position = "topright",
  #           title="Underemployment <br>
  #           Sept 2019 (16-64)",
  #           opacity=1) %>%
  
  addLegend(colors = cols, 
            labels = c(
              # "less than 5.7%",
              # "5.7% - 8.3%",
              # "8.3% - 11.0",
              # "11.0% - 13.6%",
              # "Greater than 13.6%"
              "Very low (<5.7%)",
              "Low (5.7% - 8.3%)",
              "Average (8.3% - 11.0%)",
              "High (11.0% - 13.6%)",
              "Very high (13.6%+)"
            ),
            labFormat = labelFormat(suffix = "%%"),
            position = "topright",
            title="Underemployment <br>
            Sept 2019 (16-64)",
            opacity=1) %>%
  
  removeDrawToolbar(clearFeatures = T)


plot

#page element title
title <- tags$div(HTML("Proportion of population age 16-64 underemployed pre-pandemic"), 
                  style = "font-family: Open Sans;color: #2A2A2A;font-weight: bold; font-size: 18px; text-align: center"
)

mapshot(plot, file =  "LM3.png", remove_controls = T)

combo <- htmltools::tagList(title, plot) #I think this makes a combined html object
html_print(combo)
# htmltools::save_html(combo, "index.html") #this saves it as an HTML page in the default folder.
