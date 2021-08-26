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
library(raster)
library(rgdal)
library(tidyr)

### YIELD TRAINING ---- BLENDING POINTS FROM SAME PADDOCK AND CREATING SHP

df_out <- read_csv(file.path(dir_data, "Full", 'nps_full_ndvi.csv'))
df_out1 <- df_out %>% 
  filter(Crop == "Wheat"|Crop == "Canola"| Crop =="Barley") %>% 
  group_by(PaddID) %>% 
  mutate(longi=mean(Longitude), lati=mean(Latitude)) %>% 
  distinct(PaddID, .keep_all = TRUE) # KEEP UNIQUE VALUES

coordinates (df_out1) <- ~Longitude+Latitude
proj4string(df_out1) <- CRS("+init=epsg:4326")
df_out1 <- st_as_sf(df_out1)

st_write(df_out1, "yield_training4.shp")

### CROP TRAINING ---- BLENDING POINTS FROM SAME PADDOCK AND CREATING SHP

train_cropshp <- shapefile(file.path(dir_data, "Maps", "AUS_NAT_CT_YP_2015_final.shp"))
train_cropshp1 <- getSpPPolygonsLabptSlots(train_cropshp)
train_cropdf <- data.frame(train_cropshp1)
coordinates(train_cropdf) <- ~X1+X2
proj4string(train_cropdf) <- CRS("+init=epsg:4326")
train_cropshp2 <- st_as_sf(train_cropdf)
st_write(train_cropshp2, "crop_training.shp")

### CHANGE COMPSHP COORDINATE SYSTEM
comp_shp <- shapefile(file.path(dir_data, "yield_features", "comp_area.shp"))
spTransform
proj4string(comp_shp) <- CRS("+init=epsg:4326")
comp_shp <- st_as_sf(comp_shp)
st_write(comp_shp, "comp_shp.shp")


