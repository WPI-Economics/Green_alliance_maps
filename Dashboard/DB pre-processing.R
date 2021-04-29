#preparation of LM dashboard data from original excel file (created by James Edgar)

library(tidyverse)
library(readxl)
library(sf)

#Constituency boundaries
pcons <- readRDS("pcons.RDS")

#Labour Market metrics
GA.raw <- read_excel("Green Alliance constituency model.xlsx", 
                     sheet = "Results (sorted)", skip = 1)
GA.1 <- GA.raw[,c(1,2,18,9,3,5)]

colnames(GA.1) <- c("Constituency",
                    "ONS Code",
                    "Relative labour market challenge score (100 = average constituency)",
                    "Forecast change in employments 2019-2025 (%)",
                    "Underemployment Sept 2019 (% 16-64)",
                    "Underemployment change Sept 2019 - Sept 2020 (%)")


#Seagrass locations
seagrass <- readRDS(file = "sg.all.corrected.RDS")
seagrass <- seagrass %>% filter(!(PlaceName == "The Fleet" & source == "Contemporary")) #remove broken entry for The Fleet
#remove NI
seagrass <- seagrass %>% filter(Area != "Northern Ireland")

#create flag for pcons with seagrass sites
pcons <- st_transform(pcons, 27700)
seagrass <- st_transform(seagrass, 27700)
seagrass.buff <- st_buffer(seagrass, dist = 1000) #distance in meters so 1km
seagrass.buff <-st_union(seagrass.buff)  #have to make one object else the matrix is stuuupid
pcons$seagrass.1000m.flag <- st_intersects(pcons$geometry, seagrass.buff, sparse = F)


#Woodland
Woodland <- readRDS(file = "Woodland.pcon.summary.RDS")
Woodland <- st_drop_geometry(Woodland)
Woodland <- Woodland[,c(1,11,12,13, 16)]


#Constituencies within 20 miles 
Bog <- read_csv(file = "Constituencies 20 miles from bog.csv")
Bog <- Bog[,c(-1,-3)]

#Coastal retoration areas counts  
Coastal.resto <- read_csv(file = "Coastal restoration by constituency.csv") #coastal areas to filter against
Coastal.resto$`Priority sites (count)`[is.na(Coastal.resto$`Priority sites (count)`)] <- 0
Coastal.resto$`All sites (count)`[is.na(Coastal.resto$`All sites (count)`)] <- 0


Coastal.resto$`Coastal resoration sites flag` <- "No"
Coastal.resto$`Coastal resoration sites flag`[Coastal.resto$`All sites (count)`> 0] <- "Yes"

Coastal.resto$`Coastal resoration priority sites flag` <- "No"
Coastal.resto$`Coastal resoration priority sites flag`[Coastal.resto$`Priority sites (count)`> 0] <- "Yes"

#### MERGE into 1 Pcons file.
pcons.merged <- pcons[,c(1,2,11)]
pcons.merged <- merge(pcons.merged, Woodland, by = "pcon19cd")
pcons.merged <- merge(pcons.merged,Coastal.resto[,c(1:3,5,6)] , by = "pcon19cd")
pcons.merged <- merge(pcons.merged,Bog , by = "pcon19cd")
pcons.merged <- merge(pcons.merged,GA.1[,c(2:6)], by.y = "ONS Code", by.x = "pcon19cd")

pcons.merged <- pcons.merged[,c(1,2,13,14,15,16,3:12)] #reorder

colnames(pcons.merged) <- c("pcon19cd",
                            "Constituency",
                            "Labour market challenge score (100 = average)",
                            "Forecast change in employments 2019-2025 (%)",
                            "Underemployment Sept 2019 (% 16-64)",
                            "Underemployment change Sept 2019 - Sept 2020 (%)",
                            "Known seagrass location within 1000m",
                            "Woodland area (ha)",
                            "Constituency area (ha)", 
                            "Woodland (%)",                  
                            "Share of woodland",
                            "Priority sites coastal restoration sites (count)",
                            "All coastal restoration sites (count)",
                            "Coastal restoration sites flag",
                            "Coastal restoration priority sites flag",
                            "Within 20 miles of Great North Bog" ,
                            "geometry" )

pcons.merged <- pcons.merged[,-c(9:10)]

pcons.merged$`Known seagrass location within 1000m`[pcons.merged$`Known seagrass location within 1000m` == T] <- "Yes"
pcons.merged$`Known seagrass location within 1000m`[pcons.merged$`Known seagrass location within 1000m` == F] <- "No"

pcons.merged$`Within 20 miles of Great North Bog`[pcons.merged$`Within 20 miles of Great North Bog` == T] <- "Yes"
pcons.merged$`Within 20 miles of Great North Bog`[pcons.merged$`Within 20 miles of Great North Bog` == F] <- "No"

pcons.merged$`Woodland area (ha)` <- round(pcons.merged$`Woodland area (ha)`,0)



#Adds LM challenge colours as variable
cols <- c("#f0cfc7",
          "#dca091",
          "#c4725f",
          "#a94330",
          "#8a0000")



LM2cols <- c("#8a0000",
          "#c98271",
          "#E0E0E0",
          "#adb8e6",
          "#7081e4")

LM4cols <- c("#adb8e6", #blue
          "#ebc3b9",
          "#d18978",
          "#b04f3b",
          "#8a0000")


meanval <- mean(pcons.merged$`Labour market challenge score (100 = average)`)
sdval <- sd(pcons.merged$`Labour market challenge score (100 = average)`)

pcons.merged$cols <- NA
pcons.merged$cols[pcons.merged$`Labour market challenge score (100 = average)` > meanval + (sdval*1.5)] <- cols[5] #8a0000 greater than 1.5 SD from mean VERY HIGH
pcons.merged$cols[pcons.merged$`Labour market challenge score (100 = average)` <= meanval + (sdval*1.5)] <- cols[4] #b04f3b between +0.5 and +1.5 SD of mean HIGH
pcons.merged$cols[pcons.merged$`Labour market challenge score (100 = average)` <= meanval + (sdval*0.5)] <- cols[3] #d18978 between -0.5 and +0.5 SD of mean AVERAGE              
pcons.merged$cols[pcons.merged$`Labour market challenge score (100 = average)` <= meanval - (sdval*0.5)] <- cols[2] #ebc3b9 between -1.5 and -0.5 SD from mean LOW
pcons.merged$cols[pcons.merged$`Labour market challenge score (100 = average)` <= meanval - (sdval*1.5)] <- cols[1] #ebc3b9 LT -1.5 SD from mean VERY LOW



#Add colour scales for other LM maps
meanval <- mean(pcons.merged$`Forecast change in employments 2019-2025 (%)`)
sdval <- sd(pcons.merged$`Forecast change in employments 2019-2025 (%)`)

pcons.merged$LM2cols <- NA
pcons.merged$LM2cols[pcons.merged$`Forecast change in employments 2019-2025 (%)` > meanval + (sdval*1.5)] <- LM2cols[5] #8a0000 greater than 1.5 SD from mean VERY HIGH
pcons.merged$LM2cols[pcons.merged$`Forecast change in employments 2019-2025 (%)` <= meanval + (sdval*1.5)] <- LM2cols[4] #b04f3b between +0.5 and +1.5 SD of mean HIGH
pcons.merged$LM2cols[pcons.merged$`Forecast change in employments 2019-2025 (%)` <= meanval + (sdval*0.5)] <- LM2cols[3] #d18978 between -0.5 and +0.5 SD of mean AVERAGE              
pcons.merged$LM2cols[pcons.merged$`Forecast change in employments 2019-2025 (%)` <= meanval - (sdval*0.5)] <- LM2cols[2] #ebc3b9 between -1.5 and -0.5 SD from mean LOW
pcons.merged$LM2cols[pcons.merged$`Forecast change in employments 2019-2025 (%)` <= meanval - (sdval*1.5)] <- LM2cols[1] #ebc3b9 LT -1.5 SD from mean VERY LOW

#LM3
sdval <- sd(pcons.merged$`Underemployment Sept 2019 (% 16-64)`) #standard deviation of the data
meanval <- mean(pcons.merged$`Underemployment Sept 2019 (% 16-64)`) 

pcons.merged$LM3cols <- NA
pcons.merged$LM3cols[pcons.merged$`Underemployment Sept 2019 (% 16-64)` > meanval + (sdval*1.5)] <- cols[5] #8a0000 greater than 1.5 SD from mean VERY HIGH
pcons.merged$LM3cols[pcons.merged$`Underemployment Sept 2019 (% 16-64)` < meanval + (sdval*1.5)] <- cols[4] #b04f3b between +0.5 and +1.5 SD of mean HIGH
pcons.merged$LM3cols[pcons.merged$`Underemployment Sept 2019 (% 16-64)` < meanval + (sdval*0.5)] <- cols[3] #d18978 between -0.5 and +0.5 SD of mean AVERAGE              
pcons.merged$LM3cols[pcons.merged$`Underemployment Sept 2019 (% 16-64)` < meanval - (sdval*0.5)] <- cols[2] #ebc3b9 between -1.5 and -0.5 SD from mean LOW
pcons.merged$LM3cols[pcons.merged$`Underemployment Sept 2019 (% 16-64)` < meanval - (sdval*1.5)] <- cols[1] #ebc3b9 LT -1.5 SD from mean VERY LOW


#LM4
sd <- sd(pcons.merged$`Underemployment change Sept 2019 - Sept 2020 (%)`) #standard deviation of the data
pcons.merged$LM4cols <- NA
pcons.merged$LM4cols[pcons.merged$`Underemployment change Sept 2019 - Sept 2020 (%)` < 0] <- LM4cols[1] #adb8e6 #blue
pcons.merged$LM4cols[pcons.merged$`Underemployment change Sept 2019 - Sept 2020 (%)` > 0] <- LM4cols[2] #ebc3b9
pcons.merged$LM4cols[pcons.merged$`Underemployment change Sept 2019 - Sept 2020 (%)` > sd] <- LM4cols[3] #d18978
pcons.merged$LM4cols[pcons.merged$`Underemployment change Sept 2019 - Sept 2020 (%)` > sd*2] <- LM4cols[4] #b04f3b
pcons.merged$LM4cols[pcons.merged$`Underemployment change Sept 2019 - Sept 2020 (%)` > sd*3] <- LM4cols[5] #8a0000


pcons.merged$`Labour market challenge score (100 = average)` <- round(pcons.merged$`Labour market challenge score (100 = average)`,0)
pcons.merged$`Forecast change in employments 2019-2025 (%)` <- round(pcons.merged$`Forecast change in employments 2019-2025 (%)`*100 ,1)
pcons.merged$`Underemployment Sept 2019 (% 16-64)` <- round(pcons.merged$`Underemployment Sept 2019 (% 16-64)`*100 ,1)
pcons.merged$`Underemployment change Sept 2019 - Sept 2020 (%)` <- round(pcons.merged$`Underemployment change Sept 2019 - Sept 2020 (%)`*100 ,1)


saveRDS(pcons.merged, file = "Dashboard/GA_DB.RDS")


## Create point locations with pcon19cd to link with on dashboard.
#create flag for pcons with seagrass sites

pcons <- st_transform(pcons, 27700)
seagrass <- st_transform(seagrass, 27700)

seagrass$rowlookup <- st_nearest_feature(seagrass,pcons$geometry)
pconslook <- pcons %>%  st_drop_geometry() %>%  as_tibble()
pconslook <- pconslook[,c(1,2)]
seagrass <- merge(seagrass, pconslook, by.x = "rowlookup", by.y = 0) #by.y = 0 matches against rownames
seagrass <- seagrass[,c(2,3,5,7,8)]

saveRDS(seagrass, file = "Dashboard/seagrass.points.RDS")


#Coastal restoration sites layer
Coastal.resto <-geojsonio::geojson_sf("https://opendata.arcgis.com/datasets/59fe52ba9f8744e88eca9cec371fae30_0.geojson")
Coastal.resto <- Coastal.resto[,c(4)]
Coastal.resto <- st_centroid(Coastal.resto)
Coastal.resto$lng <- st_coordinates(Coastal.resto)[,1]
Coastal.resto$lat <- st_coordinates(Coastal.resto)[,2]

Coastal.resto <- st_transform(Coastal.resto, 27700)
Coastal.resto$rowlookup <- st_nearest_feature(Coastal.resto,pcons$geometry)
Coastal.resto <- merge(Coastal.resto, pconslook, by.x = "rowlookup", by.y = 0) #by.y = 0 matches against rownames
Coastal.resto <- Coastal.resto[,c(2:6)]
Coastal.resto <- st_drop_geometry(Coastal.resto)
Coastal.resto<- Coastal.resto %>% filter(str_detect(pcon19cd, "N0", negate = T)) #drop NI






saveRDS(Coastal.resto, file = "Dashboard/Coastal.resto.RDS")






