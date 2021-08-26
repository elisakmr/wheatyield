library(raster)
library(sf)
library(tidyverse)

s <- shapefile("C:/Users/wal716/Documents/projects/digiscape/data/insitu_data/wa/Field_trip_data_all_in_one_cleaned.shp")
# field ID
# crop binary
# LC land cover text
# CODE of the class Numeric cf JECAM
# IRRIGATION
head(s@data)
cropList <- c("Pasture", "Canola", "Wheat",  "Barley",  "Lupins", "Oats", "Peas", "Lentils")

s@data <- s@data %>% 
  dplyr::select(-one_of("CBH_HOLDER")) %>% 
  rename(LC = Crop) %>% 
  mutate(ID = 1:nrow(s@data), IRRIGATION = 0, 
         CROP = ifelse(LC %in% cropList, 1, 0), 
         CODE = LC,
         CODE = as.numeric(recode(CODE, `Cereal` = 1,
                                  `Wheat` = 11,
                                  `Barley` = 15,
                                  `Oats` = 17,
                                  `Lupins` = 76,
                                  `Peas` = 77,
                                  `Lentils` = 75,
                                  `Canola` = 435,
                                  `Pasture` = 1000,
                                  `Fallow` = 1001,
                                  `Shrub` = 1002,
                                  `Shrubs` = 1002,
                                  `Water` = 1003,
                                  `Forest` = 1004,
                                  `Sand` = 1005,
                                  `Urban` = 1006,
                                  `Grassland` = 1007,
                                  `0GRASSLAND` = 1007,
                                  `Salt` = 1008)))

shapefile(s, "C:/Users/wal716/Documents/projects/digiscape/data/insitu_data/wa/wa_2017_wa_sen2agri.shp")

### 2015 ---------------
### 2015: YieldProphet ---------------
s <- shapefile("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/in_situ_data/2015/yieldprophet/AUS_NAT_CT_YP_2015.shp")
s@data <- s@data %>% 
  dplyr::select(-one_of("area", "Year", "Cultivar", "Yield", "HarvestTyp")) %>% 
  rename(LC = class) %>% 
  mutate(ID = 1:nrow(s@data), IRRIGATION = 0, 
         CODE = LC,
         CODE = as.numeric(recode(CODE, `Wheat` = 11,
                                  `Barley` = 15,
                                  `Oats` = 17,
                                  `Chickpea` = 73,
                                  `Canola` = 435,
                                  `Lupin` = 76,
                                  `Fieldpea` = 77,
                                  `Fababean` = 79,
                                  `Lentil` = 75,
                                  `Sorghum` = 14)))
s <- shapefile(s, "C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/in_situ_data/2015/yieldprophet/AUS_NAT_CT_YP_2015_final.shp")

### 2016 ---------------
### 2016: YieldProphet ---------------
s <- shapefile("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/in_situ_data/2016/yieldprophet/AUS_NAT_CT_YP_2016.shp")
s@data <- s@data %>% 
  dplyr::select(-one_of("area", "Year", "Cultivar", "Yield", "HarvestTyp")) %>% 
  rename(LC = class) %>% 
  mutate(ID = 1:nrow(s@data), IRRIGATION = 0, 
         CODE = LC,
         CODE = as.numeric(recode(CODE, `Wheat` = 11,
                                  `Barley` = 15,
                                  `Oats` = 17,
                                  `Chickpea` = 73,
                                  `Canola` = 435,
                                  `Lupin` = 76,
                                  `Fieldpea` = 77,
                                  `Fababean` = 79,
                                  `Lentil` = 75,
                                  `Sorghum` = 14)))
s <- shapefile(s, "C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/in_situ_data/2016/yieldprophet/AUS_NAT_CT_YP_2016_final.shp")


### 2017 ---------------
### 2017: YieldProphet ---------------
s <- shapefile("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/in_situ_data/2017/yieldprophet/AUS_NAT_CT_YP_2017.shp")
s@data <- s@data %>% 
  dplyr::select(-one_of("area", "Year", "Cultivar", "Yield", "HarvestTyp")) %>% 
  rename(LC = class) %>% 
  mutate(ID = 1:nrow(s@data), IRRIGATION = 0, 
         CODE = LC,
         CODE = as.numeric(recode(CODE, `Wheat` = 11,
                                  `Barley` = 15,
                                  `Oats` = 17,
                                  `Chickpea` = 73,
                                  `Canola` = 435,
                                  `Lupin` = 76,
                                  `Fieldpea` = 77,
                                  `Fababean` = 79,
                                  `Lentil` = 75,
                                  `Sorghum` = 14)))
s <- shapefile(s, "C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/in_situ_data/2017/yieldprophet/AUS_NAT_CT_YP_2017_final.shp")

### 2017: Vic Zvi ---------------
s <- shapefile("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/in_situ_data/2017/vic_zvi/AUS_VIC_LC_2017_s2agri_final_reproj.shp")
s <- s[!which(s$LC %in% c("Forest", "Forest - s", "Water", "Urban", "Other Crop", "Bare soil")), ]
s@data <- s@data %>% 
  mutate(CODE = as.numeric(recode(LC, `Wheat` = 11,
                                  `Barley` = 15,
                                  `Oats` = 17,
                                  `Chickpeas` = 73,
                                  `Field Peas` = 77,
                                  `Lentils` = 75,
                                  `Canola` = 435,
                                  `Pasture` = 1000,
                                  `Fava Beans` = 79,
                                  `Vetch` = 70, 
                                  `Fallow - B` = 2000,
                                  `Fallow - S` = 2000,
                                  `Fallow - W` = 2000))) %>% 
  dplyr::select(-one_of("CROP"))
shapefile(s, "C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/in_situ_data/2017/vic_zvi/AUS_VIC_LC_2017_s2agri_final.shp", overwrite = T)

### 2017: SA Yang ---------------
s <- shapefile("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/in_situ_data/2017/sa_yang/sa_yang.shp")
s <- s[which(! s$LC %in% c("Forest", "Forest - s", "Water", "Urban", "Other Crop", "Bare soil")), ]
s@data <- s@data %>% 
  mutate(CODE = as.numeric(recode(LC, `Wheat` = 11,
                                  `Barley` = 15,
                                  `Oats` = 17,
                                  `Chickpeas` = 73,
                                  `Field Peas` = 77,
                                  `Lentils` = 75,
                                  `Canola` = 435,
                                  `Pasture` = 1000,
                                  `Fava Beans` = 79,
                                  `Vetch` = 70, 
                                  `Fallow - B` = 2000,
                                  `Fallow - S` = 2000,
                                  `Fallow - W` = 2000))) %>% 
  dplyr::select(-one_of("CROP"))
shapefile(s, "C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/in_situ_data/2017/sa_yang/sa_yang_final.shp", overwrite = T)



### 2017: QLD Winter ---------------
s <- shapefile("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/in_situ_data/2017/qld/AUS_QLD_LC_combined_2017.shp")
s <- s[which(! s$LC %in% c("Forest", "Other winter crops", "Water", "Urban", "Other Crop", "Bare", "Wetland")), ]
s@data <- s@data %>% 
  mutate(CODE = as.numeric(recode(LC, `Wheat` = 11,
                                  `Barley` = 15,
                                  `Oats` = 17,
                                  `Chickpea` = 73,
                                  `Canola` = 435,
                                  `Pasture` = 1000,
                                  `Vetch` = 70,
                                  `Fallow - bare` = 2000,
                                  `Fallow - stubble` = 2000))) %>% 
  dplyr::select(-one_of("CROP"))
shapefile(s, "C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/in_situ_data/2017/sa_yang/AUS_QLD_LC_combined_2017_final.shp", overwrite = T)


### 2017: QLD SUMMER ---------------
s <- shapefile("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/in_situ_data/2017/qld/AUS_QLD_LC_IS_20180116_sen2agri.shp")
s <- s[which(!s$LC %in% c("Other crops")), ]
s@data <- s@data %>% 
  mutate(CODE = as.numeric(recode(LC, `Sorghum` = 14,
                                  `Maize` = 12,
                                  `Cotton` = 921,
                                  `Pasture` = 1000,
                                  `Other crops` = 8,
                                  `Fallow - bare` = 2000,
                                  `Fallow - stubble` = 2000,
                                  `Fallow - weedy` = 2000))) %>% 
  dplyr::select(-one_of("CROP"))
shapefile(s, "C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/in_situ_data/2017/sa_yang/AUS_QLD_LC_IS_20180116_final.shp", overwrite = T)

