install.packages(c("rgdal", "sp"))
install.packages("dplyr")
library(rgdal)
library(sp)
library(raster)
library(dplyr)

MyData1 <- read.csv(file="X:/projects/yield_machinelearning/data/Full/Gonz_YMap_summarised_20160429.txt", header=TRUE, sep=",")
MyData2 <- read.csv(file="X:/projects/yield_machinelearning/data/Full/NPSYMap_summarised_20160414.txt", header=TRUE, sep=",")
plot.yields1 <- SpatialPointsDataFrame(MyData1[,5:6],  MyData1) # spatial coordinates 
plot.yields2 <- SpatialPointsDataFrame(MyData2[,5:6],  MyData2)
dim(MyData1)
dim(MyData2)
myfulldata<-bind_rows(MyData1,MyData2)
dim(myfulldata)
summary(myfulldata)
plot.yields <- SpatialPointsDataFrame(myfulldata[,5:6],  myfulldata)
plot.yields

root_dir <- "X:/digiscape/data/modis"

df_out <- NULL
for (y in unique(plot.yields$Year)){ # each pixel each year
  print(y)
  r_names <- list.files(file.path(root_dir, y), "linint.tif$", full.names = T)
  s <- stack(r_names)
  
  index_y <-  which(plot.yields$Year == y)
  df_extract <- extract(s, plot.yields[index_y, ], df = T) # NDVI rows corresponding to pixel
  colnames(df_extract)
  str(colnames(df_extract))
  new_colnames <- lapply(strsplit(colnames(df_extract), '_'), function(x) paste0(x[3],"_",x[5]))
  colnames(df_extract) <- new_colnames
  df_extract$NA_NA <- NULL
  df_ready <- bind_cols(plot.yields@data[index_y, ], df_extract) # create df binding ndvi+yields info
  
  if(is.null(df_out)){
    df_out <- df_ready
  } else {
    df_out <- bind_rows(df_out, df_ready)
  }
}
write_csv(df_out,file.path(dir_data, 'nps_full_ndvi.csv')) # NDVI serie + yield for the whole observed area
    
