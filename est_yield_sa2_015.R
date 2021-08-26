if ((Sys.info()["nodename"]) == "sc-05-cdc") {
  dir_data <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data"
  dir_plot <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/figures"
} else if ((Sys.info()["nodename"]) == "MULLET-SL") {
  dir_data <- "X:/projects/yield_machinelearning/data"
  dir_plot <- "X:/projects/yield_machinelearning/figures"
}

library(tidyverse)
library(dplyr)
library(sf)
library(readr)
library(maptools)
library(rgdal)

### READING CSV GOV YIELD DATA
yield_data <- read_csv(file.path(dir_data, "yield_features", "yield_estimates_2015-16.csv"))
colnames(yield_data)<- yield_data[4,]
yield_data <- yield_data[-(1:4),] %>% 
  rename("com_des"="Commodity description") %>% 
  dplyr:: filter(str_detect(com_des, 'Wheat|Barley|Canola')) %>% 
  select(`Region label`, Estimate, com_des) %>% 
  rename("SA2_NAME16"= "Region label") %>% 
  mutate(SA2_NAME16 = as.factor(SA2_NAME16))

### TRANSFORMING GOV YIELD DATA SA2 TO SHAPEFILE
sa2_border <- st_read(file.path("X:/projects/yield_machinelearning/data/yield_features/sa2_borders_2016", 
                                "SA2_2016_AUST.shp")) 
estimated_shp <- sa2_border %>% 
  left_join(yield_data, by="SA2_NAME16") %>% # join by the region code the two files, left means the first file is the benchmark
  mutate(sa2ID = row_number()) %>% 
  na.omit() 

sf::st_write(estimated_shp, tempfile(fileext = ".shp")) #write shp from sf object

