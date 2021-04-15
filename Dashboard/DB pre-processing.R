#preparation of LM dashboard data from original excel file (created by James Edgar)

library(tidyverse)
library(readxl)


GA.raw <- read_excel("Green Alliance constituency model.xlsx", 
                     sheet = "Results (sorted)", skip = 1)


GA.1 <- GA.raw[,c(1,2,18,9,3,5)]

colnames(GA.1) <- c("Constituency",
                    "ONS Code",
                    "Relative labour market challenge score (100 = average constituency)",
                    "Forecast change in employments 2019-2025 (%)",
                    "Underemployment pre-pandemic (% 16-64)",
                    "Underemployment change Sept 2019 - Sept 2020 (%)")
                    
