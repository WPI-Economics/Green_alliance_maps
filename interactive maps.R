
###########LM1

plot <- leaflet(sd.map, options= leafletOptions(padding = 100, zoomSnap = 0.25, zoomDelta = 0.3, zoomControl = T, minZoom = 5.3)) %>%
  setView(lng =  -3.13,
          lat = 54.8,zoom = 6.3) %>% #setView gives centre coordinates and zoom level
  
  setMapWidgetStyle(list(background = "white")) %>%
  
  addProviderTiles(providers$CartoDB.Positron, providerTileOptions(opacity = 0.1),
                   options = providerTileOptions(minZoom = 9, maxZoom = 15)) %>%
  
  
  addCircleMarkers(data = sd.centroids, lng = ~lng, lat = ~lat, group =  "LM Challenge",
                   radius = 5,
                   weight = 0,
                   opacity = 0,
                   fillOpacity = 0,
                   fillColor = "blue"
  ) %>%
  
  
  addPolygons( stroke = T, color = "white",
               group = "LM Challenge",
               fillColor = ~cols,
               opacity = 0.5,
               fillOpacity = 1, weight = 0.25, label = labels1,
               highlight= highlightOptions(color="white", weight=2, bringToFront= T))  %>%
  
  
    addCircleMarkers(data = sd.seagrass, lng = ~lng, lat = ~lat, group =  "Seagrass points",
                   radius = 5,
                   weight = 0,
                   fillOpacity = 1,
                   fillColor = "#00946E",
  ) %>%
  
  addCircleMarkers(data = sd.coastal.resto, lng = ~lng, lat = ~lat, group =  "Coastal restoration sites",
                   radius = 5,
                   weight = 0,
                   fillOpacity = 1,
                   fillColor = "red"
  ) %>%
  
  
  addPolygons(data = GNbogs, stroke = T, color = "black",
              group = "`Great Northern Bog`",
              fillColor = "white",
              opacity = 0.5,
              fillOpacity = 0.5, weight = 1
  )  %>%
  
  
  addCircleMarkers(data = sd.GNbogs.hidden.points, lng = ~long, lat = ~lat, group =  "`Great Northern Bog`",
                   radius = 5,
                   weight = 0,
                   opacity = 0,
                   fillOpacity = 0,
                   fillColor = "blue"
  ) %>%
  
  
  
  
  
  addPolygons(data = woodland.pcon, stroke = T, color = "green",
              group = "`High woodland`",
              fillColor = "white",
              opacity = 0.5,
              fillOpacity = 0.5, weight = 1
  )  %>%
  
  
  addCircleMarkers(data = sd.woodland.pcon.hidden,
                   lng = ~long,
                   lat = ~lat,
                   group =  "`High woodland`",
                   radius = 5,
                   weight = 0,
                   opacity = 0,
                   fillOpacity = 0,
                   fillColor = "blue"
  ) %>%
  
  addLayersControl(overlayGroups = c("Seagrass points",
                                     "Coastal restoration sites",
                                     "`Great Northern Bog`",
                                     "`High woodland`"),
                                                                         
                   options = layersControlOptions(collapsed = FALSE)) %>%
  
  hideGroup(c("Seagrass points",
              "Coastal restoration sites",
              "`Great Northern Bog`",
              "`High woodland`"
              )) %>%
  
  addResetMapButton() %>% 
  
  
  addLegend(group = "LM Challenge",
            colors = rev(cols),
            labels = c(
              paste0("Very high (", very.high.sht,")"),
              paste0("High (",high.sht,")"),
              paste0("Average (",average.sht,")"),
              paste0("Low (", low.sht,")"),
              paste0("Very low (",very.low.sht,")")
            ),
            position = "topright",
            title="Labour market challenge score",
            opacity=0.65) 

  



#######LM2

plot <- leaflet(sd.map, options= leafletOptions(padding = 100, zoomSnap = 0.25, zoomDelta = 0.3, zoomControl = T, minZoom = 5.3)) %>%
  setView(lng =  -3.13,
          lat = 54.8,zoom = 6.3) %>% #setView gives centre coordinates and zoom level
  
  setMapWidgetStyle(list(background = "white")) %>%
  
  addProviderTiles(providers$CartoDB.Positron, providerTileOptions(opacity = 0.1),
                   options = providerTileOptions(minZoom = 9, maxZoom = 15)) %>%
  
  
  addCircleMarkers(data = sd.centroids, lng = ~lng, lat = ~lat, group =  "LM Challenge",
                   radius = 5,
                   weight = 0,
                   opacity = 0,
                   fillOpacity = 0,
                   fillColor = "blue"
  ) %>%
  
  
 
    addPolygons( stroke = T, color = "white",
               group = "Employment change forecast",
               fillColor = ~LM2cols,
               opacity = 0.5,
               fillOpacity = 1, weight = 0.25, label = labels2,
               highlight= highlightOptions(color="white", weight=2, bringToFront= T))  %>%
  
      addCircleMarkers(data = sd.seagrass, lng = ~lng, lat = ~lat, group =  "Seagrass points",
                   radius = 5,
                   weight = 0,
                   fillOpacity = 1,
                   fillColor = "#00946E",
  ) %>%
  
  addCircleMarkers(data = sd.coastal.resto, lng = ~lng, lat = ~lat, group =  "Coastal restoration sites",
                   radius = 5,
                   weight = 0,
                   fillOpacity = 1,
                   fillColor = "red"
  ) %>%
  
  
  addPolygons(data = GNbogs, stroke = T, color = "black",
              group = "`Great Northern Bog`",
              fillColor = "white",
              opacity = 0.5,
              fillOpacity = 0.5, weight = 1
  )  %>%
  
  
  addCircleMarkers(data = sd.GNbogs.hidden.points, lng = ~long, lat = ~lat, group =  "`Great Northern Bog`",
                   radius = 5,
                   weight = 0,
                   opacity = 0,
                   fillOpacity = 0,
                   fillColor = "blue"
  ) %>%
  
  
  
  
  
  addPolygons(data = woodland.pcon, stroke = T, color = "green",
              group = "`High woodland`",
              fillColor = "white",
              opacity = 0.5,
              fillOpacity = 0.5, weight = 1
  )  %>%
  
  
  addCircleMarkers(data = sd.woodland.pcon.hidden,
                   lng = ~long,
                   lat = ~lat,
                   group =  "`High woodland`",
                   radius = 5,
                   weight = 0,
                   opacity = 0,
                   fillOpacity = 0,
                   fillColor = "blue"
  ) %>%
  
  addLayersControl(overlayGroups = c("Seagrass points",
                                     "Coastal restoration sites",
                                     "`Great Northern Bog`",
                                     "`High woodland`"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  
  hideGroup(c("Seagrass points",
              "Coastal restoration sites",
              "`Great Northern Bog`",
              "`High woodland`")) %>%
  
  addResetMapButton() %>% 
  
  
  
  addLegend(group = "Employment change forecast",
            colors = rev(LM2cols), 
            labels = c(
              "Very high", 
              "High", 
              "Average", 
              "Low",
              "Very low"
            ),
            position = "topright",
            title="Forecast change <br>in employments (%)",
            opacity=1) 
  
  
  


#####LM3


plot <- leaflet(sd.map, options= leafletOptions(padding = 100, zoomSnap = 0.25, zoomDelta = 0.3, zoomControl = T, minZoom = 5.3)) %>%
  setView(lng =  -3.13,
          lat = 54.8,zoom = 6.3) %>% #setView gives centre coordinates and zoom level
  
  setMapWidgetStyle(list(background = "white")) %>%
  
  addProviderTiles(providers$CartoDB.Positron, providerTileOptions(opacity = 0.1),
                   options = providerTileOptions(minZoom = 9, maxZoom = 15)) %>%
  
  
  addCircleMarkers(data = sd.centroids, lng = ~lng, lat = ~lat, group =  "LM Challenge",
                   radius = 5,
                   weight = 0,
                   opacity = 0,
                   fillOpacity = 0,
                   fillColor = "blue"
  ) %>%
  
  

  addPolygons( stroke = T, color = "white",
               group = "Underemployment pre-pandemic",
               fillColor = ~LM3cols,
               opacity = 0.5,
               fillOpacity = 1, weight = 0.25, label = labels3,
               highlight= highlightOptions(color="white", weight=2, bringToFront= T))  %>%
  

  
  
  addCircleMarkers(data = sd.seagrass, lng = ~lng, lat = ~lat, group =  "Seagrass points",
                   radius = 5,
                   weight = 0,
                   fillOpacity = 1,
                   fillColor = "#00946E",
  ) %>%
  
  addCircleMarkers(data = sd.coastal.resto, lng = ~lng, lat = ~lat, group =  "Coastal restoration sites",
                   radius = 5,
                   weight = 0,
                   fillOpacity = 1,
                   fillColor = "red"
  ) %>%
  
  
  addPolygons(data = GNbogs, stroke = T, color = "black",
              group = "`Great Northern Bog`",
              fillColor = "white",
              opacity = 0.5,
              fillOpacity = 0.5, weight = 1
  )  %>%
  
  
  addCircleMarkers(data = sd.GNbogs.hidden.points, lng = ~long, lat = ~lat, group =  "`Great Northern Bog`",
                   radius = 5,
                   weight = 0,
                   opacity = 0,
                   fillOpacity = 0,
                   fillColor = "blue"
  ) %>%
  
  
  
  
  
  addPolygons(data = woodland.pcon, stroke = T, color = "green",
              group = "`High woodland`",
              fillColor = "white",
              opacity = 0.5,
              fillOpacity = 0.5, weight = 1
  )  %>%
  
  
  addCircleMarkers(data = sd.woodland.pcon.hidden,
                   lng = ~long,
                   lat = ~lat,
                   group =  "`High woodland`",
                   radius = 5,
                   weight = 0,
                   opacity = 0,
                   fillOpacity = 0,
                   fillColor = "blue"
  ) %>%
  
  addLayersControl(overlayGroups = c("Seagrass points",
                                     "Coastal restoration sites",
                                     "`Great Northern Bog`",
                                     "`High woodland`"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  
  hideGroup(c("Seagrass points",
              "Coastal restoration sites",
              "`Great Northern Bog`",
              "`High woodland`")) %>%
  
  addResetMapButton() %>% 
  
  
  addLegend(group =  "Underemployment pre-pandemic",
            colors = LM3cols, 
            labels = c(
              # "less than 5.7%",
              # "5.7% - 8.3%",
              # "8.3% - 11.0",
              # "11.0% - 13.6%",
              # "Greater than 13.6%"
              "Very low",
              "Low",
              "Average",
              "High",
              "Very high"
            ),
            labFormat = labelFormat(suffix = "%%"),
            position = "topright",
            title="Underemployment <br>
            Sept 2019 (16-64)",
            opacity=1) 
  
  


#####LM4



plot <- leaflet(sd.map, options= leafletOptions(padding = 100, zoomSnap = 0.25, zoomDelta = 0.3, zoomControl = T, minZoom = 5.3)) %>%
  setView(lng =  -3.13,
          lat = 54.8,zoom = 6.3) %>% #setView gives centre coordinates and zoom level
  
  setMapWidgetStyle(list(background = "white")) %>%
  
  addProviderTiles(providers$CartoDB.Positron, providerTileOptions(opacity = 0.1),
                   options = providerTileOptions(minZoom = 9, maxZoom = 15)) %>%
  
  
  addCircleMarkers(data = sd.centroids, lng = ~lng, lat = ~lat, group =  "LM Challenge",
                   radius = 5,
                   weight = 0,
                   opacity = 0,
                   fillOpacity = 0,
                   fillColor = "blue"
  ) %>% 
  
  addPolygons( stroke = T, color = "white",
               group = "Underemployment change",
               fillColor = ~LM4cols,
               opacity = 0.5,
               fillOpacity = 1, weight = 0.25, label = labels4,
               highlight= highlightOptions(color="white", weight=2, bringToFront= T))  %>%
  
  
  
  
  
  
  addCircleMarkers(data = sd.seagrass, lng = ~lng, lat = ~lat, group =  "Seagrass points",
                   radius = 5,
                   weight = 0,
                   fillOpacity = 1,
                   fillColor = "#00946E",
  ) %>%
  
  addCircleMarkers(data = sd.coastal.resto, lng = ~lng, lat = ~lat, group =  "Coastal restoration sites",
                   radius = 5,
                   weight = 0,
                   fillOpacity = 1,
                   fillColor = "red"
  ) %>%
  
  
  addPolygons(data = GNbogs, stroke = T, color = "black",
              group = "`Great Northern Bog`",
              fillColor = "white",
              opacity = 0.5,
              fillOpacity = 0.5, weight = 1
  )  %>%
  
  
  addCircleMarkers(data = sd.GNbogs.hidden.points, lng = ~long, lat = ~lat, group =  "`Great Northern Bog`",
                   radius = 5,
                   weight = 0,
                   opacity = 0,
                   fillOpacity = 0,
                   fillColor = "blue"
  ) %>%
  
  
  
  
  
  addPolygons(data = woodland.pcon, stroke = T, color = "green",
              group = "`High woodland`",
              fillColor = "white",
              opacity = 0.5,
              fillOpacity = 0.5, weight = 1
  )  %>%
  
  
  addCircleMarkers(data = sd.woodland.pcon.hidden,
                   lng = ~long,
                   lat = ~lat,
                   group =  "`High woodland`",
                   radius = 5,
                   weight = 0,
                   opacity = 0,
                   fillOpacity = 0,
                   fillColor = "blue"
  ) %>%
  
  addLayersControl(overlayGroups = c("Seagrass points",
                                     "Coastal restoration sites",
                                     "`Great Northern Bog`",
                                     "`High woodland`"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  
  hideGroup(c("Seagrass points",
              "Coastal restoration sites",
              "`Great Northern Bog`",
              "`High woodland`" )) %>%
  
  addResetMapButton() %>% 
  
  
  
  addLegend(group = "Underemployment change",
            colors = LM4cols, 
            labels = c(
              "-30% - 0%",
              "0% - 30%",
              "30% - 60%",
              "60% - 90%",
              "90%+"
            ),
            labFormat = labelFormat(suffix = "%%"),
            position = "topright",
            title="Underemployment change<br>
            Sept 2019 to Sept 2020 (%)",
            opacity=1) 



plot
