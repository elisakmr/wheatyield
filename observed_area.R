#install.packages("tidyverse")
#install.packages("randomForest")
#install.packages("dplyr")
library(tidyverse)



dir_data <- "X:/projects/yield_machinelearning/data"
dir_plot <- "//OSM/CBR/AF_DIGI_RS/work/digiscape/Yield_prediction/figures"

df_all <- read_csv(file.path(dir_data, "nps_full_ndvi.csv")) %>% 
  unite("paddock_year", c("PaddID","Year"), sep = "_", remove = FALSE) %>% #create new columns
  unite("coordinate", c("Longitude","Latitude"), sep = "_", remove = FALSE) %>% 
  mutate(coordinate2=Longitude*Latitude)
#calculation of the observed area
as.character(df_all$coordinate)
obs_number<-length(unique(df_all$coordinate))
surface<- obs_number*62500
surface

