install.packages("tidyverse")
install.packages("FlexParamCurve")
library(tidyverse)
library(FlexParamCurve)
library(dplyr)
source("metrics2.R")
if ((Sys.info()["nodename"]) == "sc-05-cdc") {
  dir_data <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data"
  dir_plot <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/figures"
} else if ((Sys.info()["nodename"]) == "MULLET-SL") {
  dir_data <- "X:/projects/yield_machinelearning/data"
  dir_plot <- "X:/projects/yield_machinelearning/figures "
}

df_out <- read_csv(file.path(dir_data, "Full", 'nps_full_ndvi.csv'))
### SMOOTHING AND CURVE PARAMETERS EXTRACTION

# Days serie designing
df_colnames <- colnames(df_out[, grep("^NDVI", colnames(df_out))])
days<-lapply(strsplit(df_colnames, "_"), function(x) x[2])
t_in<-as.numeric(as.character(days))
# Logistic function parameters extraction
df_par <- t(apply(df_out[, grep("^NDVI", colnames(df_out))],1, function(x, t_in) extractMetrics(x, t_in), t_in = t_in))
df_out <-cbind(df_out[,1:8],df_par)

save(df_out, file = file.path(dir_data, "df_transformed.Rdata"))


# Plot yield ~ max slope NDVI 
ggplot(as.data.frame(df_out), aes(x = NDVIm_max, y = Yield..t.ha.))  +
  geom_point()+
  facet_wrap(~Crop)
# Plot yield ~ min slope NDVI 
ggplot(df_out, aes(x = slope_min, y = Yield..t.ha.))  +
  geom_point()+
  facet_wrap(~Crop)
# Plot yield ~ day min slope NDVI 
ggplot(df_out, aes(x = day_slope_min, y = Yield..t.ha.))  +
  geom_point()+
  facet_wrap(~Crop)
# Plot yield ~ day max slope NDVI 
ggplot(df_out, aes(x = day_slope_max, y = Yield..t.ha.))  +
  geom_point()+
  facet_wrap(~Crop)



