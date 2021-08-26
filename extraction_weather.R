install.packages(c("rgdal", "sp"))
install.packages("dplyr")
library(ncdf4)
library(rgdal)
library(sp)
library(raster)
library(dplyr)
library(readr)
if ((Sys.info()["nodename"]) == "sc-05-cdc") {
  dir_weather <- "//OSM/CBR/AF_DIGI_RS/work/projects/ndvi_forecasting/gridded_climate/16-day"
  dir_all <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data"
  file = "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data/nps_full_ndvi.csv"
  no_cores <- 10
} else if ((Sys.info()["nodename"]) == "MULLET-SL") {
  dir_weather <- "X:/projects/ndvi_forecasting/gridded_climate/16-day"
  dir_all <- "X:/projects/yield_machinelearning/data"
  file = "X:/projects/yield_machinelearning/data/nps_full_ndvi.csv"
  no_cores <- 2
}

load()
MyData <- read_csv(file.path(dir_all, "Full", "df_transformed.csv"))
plot.yields <- SpatialPointsDataFrame(MyData[,5:6],  MyData) # spatial coordinates 
dim(MyData)

df_out <- NULL
for (y in unique(plot.yields$Year)){ # each pixel each year
  print(y)
  # rainfall and tmax files extraction
  r_names <- grep(y,list.files(file.path(dir_weather), "rain", full.names = T), value = T)
  t_names <- grep(y, list.files(file.path(dir_weather), "tmax", full.names = T), value = T)
  s1 <- stack(r_names) # stack = reading method
  s2 <- stack(t_names)
  index_y <-  which(plot.yields$Year == y) # rows of year y
  dfr_extract <- extract(s1, plot.yields[index_y, ], df = T) # rain rows corresponding to pixel-year extracted in new df
  dft_extract <- extract(s2, plot.yields[index_y, ], df = T) # tmax rows corresponding to pixel-year
  colnames(dfr_extract) <- c("index", paste0("CUMRAIN_", seq(1,353,16))) # create col names 
  colnames(dft_extract) <- c("index", paste0("TMAX_", seq(1,353,16)))

  df_ready <- bind_cols(plot.yields@data[index_y, ], dfr_extract[,2:24], dft_extract[,2:24]) # create df binding (ndvi+yields) + weather info
  
  if(is.null(df_out)){
    df_out <- df_ready
  } else {
    df_out <- bind_rows(df_out, df_ready)
  }
}
write_csv(df_out,file.path(dir_all, 'df_transformed.csv')) # NDVI serie + yield + weather data for the whole observed area
