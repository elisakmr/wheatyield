if ((Sys.info()["nodename"]) == "sc-05-cdc") {
  dir_data <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data"
  dir_plot <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/figures"
} else if ((Sys.info()["nodename"]) == "MULLET-SL") {
  dir_data <- "X:/projects/yield_machinelearning/data"
  dir_plot <- "X:/projects/yield_machinelearning/figures"
} else if ((Sys.info()["nodename"]) == "ARGON-SL") {
  dir_data <- "W:/projects/yield_machinelearning/data"
  dir_plot <- "W:/projects/yield_machinelearning/figures"
}

library(tidyverse)
library(dplyr)
library(sf)
library(readr)
library(raster)
library(rgdal)

### JOINING ESTIMATION AND PREDICTION AT SA2 SCALE  -----------------------------------------------------------------------

## READING CSV GOV YIELD DATA
yield_data <- read_csv(file.path(dir_data, "yield_features", "yield_estimates_2015-16.csv"))
colnames(yield_data)<- yield_data[4,]
yield_data <- yield_data[-(1:4),] %>% 
  rename("com_des"="Commodity description") %>% 
  dplyr::filter(str_detect(com_des, 'Wheat|Barley|Canola')) %>% 
  dplyr::select(`Region label`, Estimate, com_des) %>% 
  rename("SA2_NAME16"= "Region label") %>% 
  mutate(SA2_NAME16 = as.factor(SA2_NAME16))

#yield_data <- read_csv(file.path(dir_data, "yield_features", "yield_sa2_2015_est.csv")) %>% 
#  dplyr::select(`Region label`, Estimate) %>% 
#  rename("SA2_NAME16"= "Region label") %>% 
#  mutate(SA2_NAME16 = as.factor(SA2_NAME16))

## TRANSFORMING GOV YIELD DATA SA2 TO SHAPEFILE
sa2_border <- st_read(file.path(dir_data, "/yield_features/sa2_borders_2016", "SA2_2016_AUST.shp")) 
estimated_sp <- sa2_border %>% 
  left_join(yield_data, by="SA2_NAME16") %>% # join by the region code the two files, left means the first file is the benchmark
  #rename("yield"="Estimate") %>% 
  mutate(sa2ID = row_number()) %>% 
  na.omit() 

## RASTERIZE PREDICTIONS
predicted_raster <- raster(file.path(dir_data, "yield_features","yield_predicted_wheat_svmr_2015.tif")) #%>% 
#r_sa2 <- rasterize(x=estimated_sp, predicted_raster , field ="sa2ID")

## EXTRACTING YIELD PREDICTION AT EACH WHEAT SOWN LOCATION

mask <- shapefile(file.path(dir_data, "yield_features", "MOD13Q1_croptype_AUS_2015_wheat_wgs84_v2.shp")) # shp of wheat sown point
yield_pred_mask <- extract(predicted_raster, mask) #return vector of values
mask$yield_pred <- yield_pred_mask # add predicted values to shp of wheat pixels
### add area per sa2
estimated_sp <- shapefile(file.path(dir_data, "yield_features", "estim_sa2", "est_sa2_area.shp"))
estimated_sf <- st_as_sf(estimated_sp)
estimated_sf <- estimated_sf %>% 
  filter(grepl("Wheat",com_des))

# match coordinate systems
# requires first to convert sf into sp
# mask1 <- sf:::as_Spatial(mask)
# estimated_sp1 <- sf:::as_Spatial(estimated_sp2) #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

map_wgs84 <- spTransform(mask, CRS("+proj=longlat +ellps=GRS80 +no_defs"))

# back to sp file to implement st_join afterward
mask <- st_as_sf(map_wgs84)

## JOINING THE TWO SHAPEFILES
comp_shp <- st_join(mask, estimated_sf, left=TRUE) # join two sf file by location => pred_yield + est_yield + sa2_id gathered for wheat sown area
#comp_shp <- comp_shp %>% 
#rename("yield_obs"="Estimate") 
class(comp_shp)
st_write(comp_shp, file.path(dir_data, "Maps", "comp_shp3_v2.shp")) 

## FILTERING ON YIELD AND REMOVING SA2 WITH SMALL WHEAT SOWN AREA
comp_shp <- st_read(file.path(dir_data, "Maps", "comp_shp3_v2.shp"))
comp_area <- comp_shp %>% 
  dplyr::select(SA2_NAME16, STE_NAME16, Estimate, com_des) %>% 
  filter(com_des == "Broadacre crops - Cereal crops - Wheat for grain - Area (ha)") %>% 
  mutate(Estimate = as.numeric(as.character(Estimate))) %>% 
  filter(Estimate > 21000) %>% 
  filter(STE_NAME16 != "Tasmania")

sa2_select <- unique(comp_area$SA2_NAME16) # list of sa2 selected
comp_yield <- as.data.frame(comp_shp) %>%
  filter(SA2_NAME16 %in% sa2_select) %>% 
  filter(grepl("Yield", com_des)) %>% 
  group_by(SA2_NAME16) 

comp_yield$Estimate <- as.numeric(as.character(comp_yield$Estimate))  
#na.omit() %>% 
comp_yield <- comp_yield %>% 
  summarise(yield_pred_av=mean(yield_pred, na.rm = T), yield_obs_av=mean(Estimate, na.rm = T)) 
write_csv(comp_yield, file.path(dir_data, "data_figures", "comp_015_filt14000_v2.csv"))

## RATIO OF SURFACE COVERED ACCORDING TO THERSHOLD SELECTED ---------
comp_filtered1 <- as.data.frame(comp_shp) %>% 
  dplyr::select(Estimate, com_des, STE_NAME16, SA2_NAME16) %>%
  mutate(Estimate = as.numeric(as.character(Estimate))) %>%
  filter(com_des == "Broadacre crops - Cereal crops - Wheat for grain - Area (ha)") %>% 
  distinct()%>% 
  filter(STE_NAME16 != "Tasmania")

total_area <- sum(comp_filtered1$Estimate)
surface_ratio <- data.frame()
for (y in c(1400, 7000, 14000, 21000, 28000)){
  comp_filtered <- comp_filtered1 %>% 
    mutate(Estimate = as.numeric(as.character(Estimate))) %>% 
    filter(Estimate > y)
  
  sa2_select <- unique(comp_filtered$SA2_NAME16) #list of sa2 selected
  #comp_yield <- as.data.frame(comp_shp) %>%
  #  filter(SA2_NAME16 %in% sa2_select) %>% 
  #  mutate(Estimate = as.numeric(as.character(Estimate))) %>% 
  #  filter(com_des == "Broadacre crops - Cereal crops - Wheat for grain - Area (ha)")  
  out <- c(y, sum(comp_filtered$Estimate))
  surface_ratio <- rbind(surface_ratio, out)
}
colnames(surface_ratio) <- c("threshold", "ratio")
surface_ratio$ratio <- surface_ratio$ratio/total_area
-----------------------------------------------------------------------
  
### PLOTTING OBS VS PRED -----------------------------------------------------------------------
library(lattice)
library(ggplot2)
library(scales)
library(viridis)
mydata <- read_csv(file.path(dir_data, "data_figures", "comp_015_filt14000_v2.csv")) %>% 
  left_join(comp_filtered1)

bias <- paste0(round(mean( mydata$yield_obs_av - mydata$yield_pred_av), 2), 0)

p <- ggplot(data=mydata, aes (x= yield_pred_av, y=yield_obs_av, colour = as.numeric(Estimate)))+
  geom_abline(intercept = 0, slope = 1, color = "grey70")+
  geom_point()+ 
  scale_colour_viridis("Wheat Area (kha)", limits= c(200000, 400000),
                       breaks = seq(from=200000, to= 400000, length.out = 5),
                       labels = c("< 200", "250", "300", "350", "> 400"), oob = squish) +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0,4)) + 
  scale_y_continuous(expand = c(0.01,0.01),  limits = c(0,4)) +
  annotate("text", x=0.66, y = 3.5, label = paste0("italic(R) ^ 2 == ", round(cor(mydata$yield_pred_av, mydata$yield_obs_av)**2, 2)), parse = T)+
  annotate("text", x=0.514, y = 3.3, label = paste0("italic(RMSE) == ", round(sqrt(mean((mydata$yield_obs_av - mydata$yield_pred_av)^2)), 2), "0"), parse = T)+
  annotate("text", x=0.705, y = 3.1, label = "italic(b)  == -0.41", parse = T)+
  labs(x="Predicted yield (t/ha)", y="Observed yield (t/ha)")+
  theme_minimal()+
  theme(axis.title = element_text(size=11), axis.text= element_text(size=11),
        legend.position = "bottom", legend.key.width = unit(2, "cm"))+
  guides(colour = guide_colourbar(title.position="top",title.hjust = 0.5))
p
ggsave(file.path("W:/projects/yield_machinelearning/figures", "sa2_comparison_wheat_yield_estimates_2015_v2.png"),
       width = 5, height = 5.5, dpi = 400)

