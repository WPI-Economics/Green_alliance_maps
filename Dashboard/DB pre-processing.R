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
                    "Underemployment pre-pandemic (% 16-64)",
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


#Constituencies within 10k 
Bog <- read_csv(file = "Constituencies 10 miles from bog.csv")
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
pcons.merged <- merge(pcons.merged,GA.1[,c(2,3)], by.y = "ONS Code", by.x = "pcon19cd")
pcons.merged <- pcons.merged[,c(1,2,13,3:12,14)] #reorder

colnames(pcons.merged) <- c("pcon19cd",
                            "Constituency",
                            "Labour market challenge score (100 = average)",
                            "Seagrass location within 1000m",
                            "Woodland area (ha)",
                            "Constituency area (ha)", 
                            "Woodland (%)",                  
                            "Share of woodland (cumulative %)",
                            "Priority sites coastal restoration sites (count)",
                            "All coastal restoration sites (count)",
                            "Coastal resoration sites flag",
                            "Coastal resoration priority sites flag",
                            "Within 10k of great north bog" ,
                            "geometry" )

pcons.merged <- pcons.merged[,-c(6:7)]

pcons.merged$`Seagrass location within 1000m`[pcons.merged$`Seagrass location within 1000m` == T] <- "Yes"
pcons.merged$`Seagrass location within 1000m`[pcons.merged$`Seagrass location within 1000m` == F] <- "No"

pcons.merged$`Within 10k of great north bog`[pcons.merged$`Within 10k of great north bog` == T] <- "Yes"
pcons.merged$`Within 10k of great north bog`[pcons.merged$`Within 10k of great north bog` == F] <- "No"

pcons.merged$`Woodland area (ha)` <- round(pcons.merged$`Woodland area (ha)`,0)
pcons.merged$`Labour market challenge score (100 = average)` <- round(pcons.merged$`Labour market challenge score (100 = average)`,0)


#Adds LM challenge colours as variable
cols <- c("#f0cfc7",
          "#dca091",
          "#c4725f",
          "#a94330",
          "#8a0000")

meanval <- mean(pcons.merged$`Labour market challenge score (100 = average)`)
sdval <- sd(pcons.merged$`Labour market challenge score (100 = average)`)

pcons.merged$cols <- NA
pcons.merged$cols[pcons.merged$`Labour market challenge score (100 = average)` > meanval + (sdval*1.5)] <- cols[5] #8a0000 greater than 1.5 SD from mean VERY HIGH
pcons.merged$cols[pcons.merged$`Labour market challenge score (100 = average)` <= meanval + (sdval*1.5)] <- cols[4] #b04f3b between +0.5 and +1.5 SD of mean HIGH
pcons.merged$cols[pcons.merged$`Labour market challenge score (100 = average)` <= meanval + (sdval*0.5)] <- cols[3] #d18978 between -0.5 and +0.5 SD of mean AVERAGE              
pcons.merged$cols[pcons.merged$`Labour market challenge score (100 = average)` <= meanval - (sdval*0.5)] <- cols[2] #ebc3b9 between -1.5 and -0.5 SD from mean LOW
pcons.merged$cols[pcons.merged$`Labour market challenge score (100 = average)` <= meanval - (sdval*1.5)] <- cols[1] #ebc3b9 LT -1.5 SD from mean VERY LOW

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


saveRDS(pcons.merged, file = "Dashboard/GA_DB.RDS")

