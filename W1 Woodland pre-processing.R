#p 2 blue areas in this map here "https://ago-item-storage.s3.us-east-1.amazonaws.com/ff13ac8cb71c44d580097c9b4340956e/Woodlands_Methodology.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjEG8aCXVzLWVhc3QtMSJHMEUCIQCTjp%2FIpBd%2FNWkxIGygcYiYSKdV2CpcIfTi9T1EUaYF8wIgOFtGW4ov57zsJI9K%2BxbWW9yb9Vn4UClYDKd8Yqi07FMqvQMIp%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FARAAGgw2MDQ3NTgxMDI2NjUiDNQvTRCNDqVOOPc6WyqRA3J%2FaZwVqoBSr3zuTUPNgp9bXG5cvAR3d%2BDdyPkOzEPYGp83lqKh6mCfF8ovqEMTppLQus4SFnSpG4KE3pIiAsBdcQHffvEtyG9R%2B4E1Kt%2F%2FPDC9vrxtqbp8TPLcZ4E%2BwtZUDq4z7cOcywOyYxId9e%2FxvK3T10Bdq%2BKJzSD0H2vFyebZ05S9wSCzInPcHz7HGPLGkMnuF0DGJRB1fXcgI58oGwh0%2FkY%2FCtwPD4YWWdEVj61vMA7CfkrqAb3iR7N8evaNAL03%2BYdhX0v76jQczS8iETge%2FmdfmTzsKs7l32KoNGP9Ol0L3%2Fo4RVir%2BDey6qhxwp30DiG79GL9ja3enMCSTFWmZp4NKM8uzjUawk02TVfwjnq4UqohfguseYi591iUtgyQS027KtEHF7VTKSaLQaWcmjOr54P4evD3bXRLx0sy6RSG%2FnFI8kHeNYJ40PzJhJ%2Fl4K%2F9RYVodij66F2dkO84jwEOFExm%2B7KvzfZDmwwDAb8cz0aA88kkHIbL8ZS%2B61rCPfltfyGYqKeLmUAcMMLI4oIGOusBfJWBN3q4ehcuxXxAGXmytOraGb%2BEkYyTCGuuEl2S8mIgRsfVMmrjKbnZ6hh5eqNDpWWGfdS0Sq2%2FkRZaDGeuzmQi4H4K9rOV7Qod%2FMWOWEJqDCRO9hxdpJ%2FGFdQcHB7rdGgMY2qQMaT7ap9wtckWUBIndJmUf0g9Q3JRciMo%2FlhUsn4vh5ps8qy0sUjy967bhoKn4mrZmIZjDNj5ZmDMkqQDdzSNh88wo8Ew1C5WLFanvoVHeX6C%2BDza6qsecs3PUMlDd3wSGfNqg6VYxtBiQb0bDe6KcP03%2F%2Fu9d9FeYr3qXrgCtpGfgyrXGQ%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20210322T152407Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAYZTTEKKE6MLDDSTA%2F20210322%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=5def3462a895f80b8ac44f13aead686108245588110ade1d796df5588f5320ea"


library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(raster)
library(geojsonio)
library(leaflet)
library(rmapshaper)
library(rgeos)
library(maptools)
library(units)

# The input file geodatabase
fgdb <- "Woodland_opportunity_mineral.gdb"
# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

woodland.raw <- st_read("Woodland_opportunity_mineral.gdb")
#woodland.raw.gen <- woodland.raw %>% select(-Year_planted_CCCLow, -Year_planted_CCCMed, -Year_planted_CCCHigh, -Year_planted_GA, -Shape_Length, -Shape_Area) this doesn't work for some reason done old skool below
woodland.raw.gen <- woodland.raw[,c(1,2,3,4)]

woodland.raw.gen  <- st_transform(woodland.raw.gen,4326) #convert to WGS84 projection.
#simplify, strip out unnecessary attiribute data and split into smaller files if needed.

#not all the rows are MULTIPOLYGON we have 1865 MULTISURFACE so simplify won't work. 
#woodland.raw.gen <- st_cast(woodland.raw.gen, "GEOMETRYCOLLECTION") %>% st_collection_extract("MULTIPOLYGON")
#remove multisurface polygons
woodland.raw.gen <- woodland.raw.gen %>% filter(st_is(woodland.raw.gen, "MULTIPOLYGON"))
#saveRDS(woodland.raw.gen, file = "woodland.interim.RDS")

w.sp <- st_zm(woodland.raw.gen) %>% as('Spatial')
#saveRDS(w.sp, file = "w.sp.RDS")

#split into 10 smaller files
woodland.raw.gen1 <-woodland.raw.gen[1:25216,]
woodland.raw.gen2 <-woodland.raw.gen[25217:50432,]
woodland.raw.gen3 <-woodland.raw.gen[50433:75648,]
woodland.raw.gen4 <-woodland.raw.gen[75649:100864,]
woodland.raw.gen5 <-woodland.raw.gen[100865:126079,]
woodland.raw.gen6 <-woodland.raw.gen[126080:151294,]
woodland.raw.gen7 <-woodland.raw.gen[151295:176510,]
woodland.raw.gen8 <-woodland.raw.gen[176511:201726,]
woodland.raw.gen9 <-woodland.raw.gen[201727:226942,]
woodland.raw.gen10 <-woodland.raw.gen[226942:252159,]



woodland.raw.gen1 <- ms_simplify(woodland.raw.gen1, keep = 0.05) #keeps 5% of points
woodland.raw.gen2 <- ms_simplify(woodland.raw.gen2, keep = 0.05) 
woodland.raw.gen3 <- ms_simplify(woodland.raw.gen3, keep = 0.05) 
woodland.raw.gen4 <- ms_simplify(woodland.raw.gen4, keep = 0.05) 
woodland.raw.gen5 <- ms_simplify(woodland.raw.gen5, keep = 0.05) 
woodland.raw.gen6 <- ms_simplify(woodland.raw.gen6, keep = 0.05) 
woodland.raw.gen7 <- ms_simplify(woodland.raw.gen7, keep = 0.05) 
woodland.raw.gen8 <- ms_simplify(woodland.raw.gen8, keep = 0.05) 
woodland.raw.gen9 <- ms_simplify(woodland.raw.gen9, keep = 0.05) 
woodland.raw.gen10 <- ms_simplify(woodland.raw.gen10, keep = 0.05) 

wood.list <- list(woodland.raw.gen1,woodland.raw.gen2,woodland.raw.gen3,woodland.raw.gen4,woodland.raw.gen5,woodland.raw.gen6,
                  woodland.raw.gen7,woodland.raw.gen8,woodland.raw.gen9,woodland.raw.gen10)



woodland.raw.gen <- do.call(rbind,wood.list)


woodland.raw.gen <- st_make_valid(woodland.raw.gen)
#saveRDS(woodland.raw.gen, file = "woodland.interim.RDS")
#woodland.raw.gen <- readRDS("woodland.interim.RDS")




#union the woodlands into one massive polygon
woodland.union <- st_combine(woodland.raw.gen)
woodland.union <- st_sf(woodland.union)

FE <- "https://opendata.arcgis.com/datasets/26ea865b037249b7a1bbc3ad72275f24_0.geojson" #full extent
Gen <- "https://opendata.arcgis.com/datasets/937997590f724a398ccc0100dbd9feee_0.geojson" #generalised

#pcons <- geojson_sf(Gen)
#pcons <- st_make_valid(pcons)
#saveRDS(pcons, file = "pcons.RDS")
pcons <- readRDS("pcons.RDS")

pcons <- st_transform(pcons, 27700)
pcons <- rmapshaper::ms_simplify(pcons, keep = 0.2)
pcons <- st_transform(pcons, 4326)


p <- as(pcons, 'Spatial')
w <- as_Spatial(woodland.union)


#project to appropriate CRS
#woodland.union <- st_transform(woodland.union,27700)
pcons <- st_transform(pcons, 27700)
woodland.raw.gen <- st_transform(woodland.raw.gen,27700)
woodland.raw.gen <- st_simplify(woodland.raw.gen) 

#t1 <- st_buffer(woodland.union,0) %>%  st_set_precision(1e5) %>%  st_intersection(pcons[4,])
t5 <- st_buffer(woodland.raw.gen, 0) %>%  st_set_precision(1e5) %>%  st_intersection(pcons)

#saveRDS(t5,"woodland.by.pcon.RDS")
t5 <- readRDS("woodland.by.pcon.RDS")

#check map
ggplot2::ggplot()+ geom_sf(data = t5, aes(fill = "AREA", colour = "red")+ geom_sf(data = pcons))


t5$area.woodland <-set_units(st_area(t5),value = ha) #area in hectares - 10,000m2 to a hectare.


#aggregate area per Pcon.
pcon.woodland <- t5 %>% group_by(pcon19nm, pcon19cd) %>% summarise(area.woodland = sum(area.woodland, na.rm = T))





#calculate areas of the pcons
pcons$pcon.area.ha <- set_units(st_area(pcons),value = ha)
pcons <- st_drop_geometry(pcons)

pcon.woodland2 <- merge(pcon.woodland, pcons[,c(1,2,10)], by =c('pcon19cd','pcon19nm'), all.y = T)
pcon.woodland2$pc.woodland <- (pcon.woodland2$area.woodland/ pcon.woodland2$pcon.area.ha) *100 

pcons.areas <- pcon.woodland2
pcons.areas2 <- tibble(pcon19cd = pcons.areas$'pcon19cd', 
                       Constituency = pcons.areas$'pcon19nm', 
                       'Woodland area (ha)' = pcons.areas$'area.woodland', 
                       'Constituency area (ha)' = pcons.areas$'pcon.area.ha', 
                       'Woodland (%)' = pcons.areas$'pc.woodland')


pcons.areas2$`Woodland area (ha)` <- as.numeric(pcons.areas2$`Woodland area (ha)`)
pcons.areas2$`Woodland area (ha)` <- as.numeric(pcons.areas2$`Woodland area (ha)`)

pcons.areas2 <- pcons.areas2 %>% mutate(across(where(is.numeric),round,1))
pcons.areas2 <- pcons.areas2 %>% mutate(across(where(is.numeric),as.numeric))

write.csv(pcons.areas2, file = "Woodland within constituencies.csv", row.names = F)

pcons.areas <- st_transform(pcons.areas,4326)

pcons.areas <- pcons.areas %>% filter(st_is(pcons.areas, "MULTIPOLYGON"))
pcons.areas.mapping <- st_cast(pcons.areas, to = "MULTIPOLYGON")


#map it!
leaflet(pcons.areas) %>% addTiles() %>% 
  addPolygons(data = pcons.areas$geometry, color = "green")
