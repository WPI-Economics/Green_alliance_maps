
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


#function for adding circle sizes to the legend
addLegendCustom <- function(map, colors, labels, sizes, opacity = 1, position){
  
  colorAdditions <- paste0(colors, "; border-radius: 60%; width:", sizes, "px; height:",
                           sizes, "px", "; position: relative; left: ",max(sizes)-(sizes/2)-5,"px"
                           ,";margin-top: 7px;line-height: ", sizes, "px;'>")
  
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           sizes,";position:relative; left: ",max(sizes)-(sizes),"px","; bottom: ",
                           5,"px",";margin-top: 12px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, 
                   labels = labelAdditions, opacity = opacity, position = position))
}

cols <- c("#f0cfc7",
          "#dca091",
          "#c4725f",
          "#a94330",
          "#8a0000")

GA.raw <- read_excel("Green Alliance constituency model.xlsx", 
                                                sheet = "Results (sorted)", skip = 1)


GA.1 <- GA.raw[,c(1,2,18,9)]


#geom file
#reduce points to make files smaller and tidier.
pcons <- readRDS("pcons.RDS")

pcons <- st_transform(pcons, 27700)
pcons <- rmapshaper::ms_simplify(pcons, keep = 0.2)
pcons <- st_transform(pcons, 4326)
# saveRDS(pcons, "pcons.RDS")

GA.2 <- merge(pcons, GA.1, by.x = "pcon19cd", by.y = "ONS code")
meanval <- mean(GA.1$`Relative labour market challenge score (100 = mean average constituency)`)
sdval <- sd(GA.1$`Relative labour market challenge score (100 = mean average constituency)`)

GA.2$cols <- NA
GA.2$cols[GA.2$`Relative labour market challenge score (100 = mean average constituency)` > meanval + (sdval*1.5)] <- cols[5] #8a0000 greater than 1.5 SD from mean VERY HIGH
GA.2$cols[GA.2$`Relative labour market challenge score (100 = mean average constituency)` <= meanval + (sdval*1.5)] <- cols[4] #b04f3b between +0.5 and +1.5 SD of mean HIGH
GA.2$cols[GA.2$`Relative labour market challenge score (100 = mean average constituency)` <= meanval + (sdval*0.5)] <- cols[3] #d18978 between -0.5 and +0.5 SD of mean AVERAGE              
GA.2$cols[GA.2$`Relative labour market challenge score (100 = mean average constituency)` <= meanval - (sdval*0.5)] <- cols[2] #ebc3b9 between -1.5 and -0.5 SD from mean LOW
GA.2$cols[GA.2$`Relative labour market challenge score (100 = mean average constituency)` <= meanval - (sdval*1.5)] <- cols[1] #ebc3b9 LT -1.5 SD from mean VERY LOW

#written long
very.high <- paste0("Greater than ",round(meanval + (sdval*1.5),0))
high <- paste0("Greater than ",round(meanval + (sdval*0.5),0) , " and less than ",round(meanval + (sdval*1.5),0)   )
average <- paste0("Greater than ",round(meanval - (sdval*0.5),0) , " and less than ",round(meanval + (sdval*0.5),0)  )
low <- paste0("Greater than ", round(meanval - (sdval*1.5),0), " and less than ", round(meanval - (sdval*0.5),0)  )
very.low <- paste0("Less than ", round(meanval - (sdval*1.5) ,0))

long.cats <- c(very.high, high, average, very.low, low)

#condensed
very.high.sht <- paste0("> ",round(meanval + (sdval*1.5),0))
high.sht <- paste0("> ",round(meanval + (sdval*0.5),0) , " <= ",round(meanval + (sdval*1.5),0)   )
average.sht <- paste0("> ",round(meanval - (sdval*0.5),0) , " <= ",round(meanval + (sdval*0.5),0)  )
low.sht <- paste0("> ", round(meanval - (sdval*1.5),0), " <= ", round(meanval - (sdval*0.5),0)  )
very.low.sht <- paste0("<= ", round(meanval - (sdval*1.5) ,0))

short.cats <- c(very.high.sht, high.sht, average.sht, very.low.sht, low.sht)


#Seagrass locations
sg.all.corrected <- readRDS(file = "sg.all.corrected.RDS")
sg.all.corrected <- sg.all.corrected %>% filter(!(PlaceName == "The Fleet" & source == "Contemporary")) #remove broken entry for The Fleet
#remove NI
sg.all.corrected <- sg.all.corrected %>% filter(Area != "Northern Ireland")


#this makes the hover over popup label
labels1 <- sprintf("<strong>%s</strong><br/>Challenge score: %s<sup></sup><br/>", 
                   GA.2$pcon19nm , round(GA.2$`Relative labour market challenge score (100 = mean average constituency)`),0) %>% 
  lapply(htmltools::HTML)


plot <- tags$head(
        tags$style(
          ".leaflet .legend {
                 line-height: 30px;
                 font-size: 30px;
                 }",
          ".leaflet .legend i{
                width: 30px;
                height: 30px;
                 }"
        ))


plot <- leaflet(height = 1600,options= leafletOptions(padding = 100, zoomSnap = 0.25, zoomDelta = 0.3, zoomControl = F)) %>%
  setView(lng =  -3.13,
          lat = 54.8,zoom = 6.8) %>% #setView gives centre coordinates and zoom level
  
  setMapWidgetStyle(list(background = "white")) %>%
  #addProviderTiles(providers$CartoDB.PositronNoLabels, providerTileOptions(opacity = 1) ) %>%
  
  addPolygons(data = GA.2, stroke = T, color = "white",
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
  
  addCircleMarkers(data = sg.all.corrected,
                   color = "#315C73",
                   opacity = 1,
                   fillOpacity = 1,
                   radius = 3, 
                   weight = 1.5,
                   popup = paste0(sg.all.corrected$PlaceName,"<br>",sg.all.corrected$`Location`)) %>% 
  
  addLegendCustom(colors = c("#315C73"), 
                  labels = c("Seagrass locations"),
                  
                  sizes = c(10), position = "topright" ) %>% 
  
  removeDrawToolbar(clearFeatures = T)


plot



#page element title
title <- tags$div(HTML("Labour market challenge scores for Westminster Constituencies"), 
                  style = "font-family: Open Sans;color: #2A2A2A;font-weight: bold; font-size: 18px; text-align: center"
)

combo <- htmltools::tagList(title, plot) #I think this makes a combined html object
html_print(combo)

#mapshot(plot, file =  "LM1.png", remove_controls = T)

# 
# htmltools::save_html(combo, "index.html") #this saves it as an HTML page in the default folder.

#saveRDS(GA.2, file = "LMChallenge.sf.RDS")
