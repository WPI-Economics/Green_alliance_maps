## Packages
library(knitr)
library(rmarkdown)
library(tidyverse)
library(knitr)
library(rmarkdown)
library(mapview)

#Map chart script pathways
map.files.print <- list.files(path = getwd(), pattern = "map.R|Map.R", recursive = T)
map.files.print <- sub('\\.R$', '', map.files.print)
map.files.print <- sub('\\.r$', '', map.files.print)
map.files.print <- map.files.print[c(2,3,4,5,6)]
#map.files.print <- map.files.print[c(4)]



for (i in map.files.print){
source(paste0(i,".R"), echo = FALSE)
  
plot <- browsable(
  tagList(
    list(
      tags$head(
        tags$style(
          ".leaflet .legend {
                 line-height: 25px;
                 font-size: 25px;
                 }",
          ".leaflet .legend i{
                width: 25px;
                height: 25px;
                 }"
        )
      ),
      plot)))
save_html(plot, file = paste0("Maps/",i,".html"))
}


#.PNG
webshot::webshot("file:///Users/guyweir/GIT/Green_alliance_maps/Maps/LM1%20Labour%20market%20challenge%20map.html", file = "Maps/LM1.png")
webshot::webshot("file:///Users/guyweir/GIT/Green_alliance_maps/Maps/LM2%20Forecast%20change%20in%20employments%20map.html", file = "Maps/LM2.png")
webshot::webshot("file:///Users/guyweir/GIT/Green_alliance_maps/Maps/LM3%20Underemployment%20map.html", file = "Maps/LM3.png")
webshot::webshot("file:///Users/guyweir/GIT/Green_alliance_maps/Maps/LM4%20Underemployment%20change%20map.html", file = "Maps/LM4.png")
webshot::webshot("file:///Users/guyweir/GIT/Green_alliance_maps/Maps/W1%20woodland%20constituency%20map.html", file = "Maps/W1.png")
webshot::webshot("file:///Users/guyweir/GIT/Green_alliance_maps/Maps/LM1_b%20Labour%20market%20challenge%20seagrass%20map.html", file = "Maps/LM1_b.png")

#.PDF
webshot::webshot("file:///Users/guyweir/GIT/Green_alliance_maps/Maps/LM1%20Labour%20market%20challenge%20map.html", file = "Maps/LM1.pdf")
webshot::webshot("file:///Users/guyweir/GIT/Green_alliance_maps/Maps/LM2%20Forecast%20change%20in%20employments%20map.html", file = "Maps/LM2.pdf")
webshot::webshot("file:///Users/guyweir/GIT/Green_alliance_maps/Maps/LM3%20Underemployment%20map.html", file = "Maps/LM3.pdf")
webshot::webshot("file:///Users/guyweir/GIT/Green_alliance_maps/Maps/LM4%20Underemployment%20change%20map.html", file = "Maps/LM4.pdf")
webshot::webshot("file:///Users/guyweir/GIT/Green_alliance_maps/Maps/W1%20woodland%20constituency%20map.html", file = "Maps/W1.pdf")

