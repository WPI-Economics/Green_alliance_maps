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
map.files.print <- map.files.print[c(1,2,3,4,6)]

#generagtes pdf maps from within the rmd code, htmls also created as byproduct
for (i in map.files.print){
  rmarkdown::render(input = "Print map compiler.Rmd",
                    output_file = i ,
                    output_dir = "Maps/")
  
  
}
