if ((Sys.info()["nodename"]) == "sc-05-cdc") {
  dir_modis <- "//OSM/CBR/AF_DIGI_RS/work/digiscape/data/modis"
  dir_plot <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/figures"
  dir_mdl <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data/models"
  no_cores <- 16
  rasterOptions(tmpdir= "//OSM/CBR/AF_DIGI_RS/work/tmp", progress = "text")
} else if ((Sys.info()["nodename"]) == "MULLET-SL") {
  dir_modis <- "X:/digiscape/data/modis"
  dir_plot <- "X:/projects/yield_machinelearning/figures"
  dir_mdl <- "X:/projects/yield_machinelearning/data/models"
  no_cores <- 2
}
library(caret)
library(raster)

### crop 1
## for every year, load raster and implement algorithm
for y in #(2014:2017) {
## load algorithm
load(file=file.path(dir_data, "models", "gp_optimised_wheat.RData"))
myFiles <- list.files(path=file.path(dir_modis, "2014"), pattern=".tif",full.names=TRUE) # gather all the rasters 
myFiles <- myFiles[-grep("linint", myFiles)]

myFiles_rain <- stack(file.path())


bandnames <- c("NDVI_1", "NDVI_17", "NDVI_33","NDVI_49", "NDVI_65", "NDVI_81", "NDVI_97",
              "NDVI_113", "NDVI_129", "NDVI_145", "NDVI_161", "NDVI_177", "NDVI_193", "NDVI_209",
              "NDVI_225" , "NDVI_241", "NDVI_257", "NDVI_273", "NDVI_289", "NDVI_305", "NDVI_321",
              "NDVI_337", "NDVI_353", "NDVIm_amp", "NDVIm_day_slope_max.sos", "NDVIm_day_slope_min.eos",
              "NDVIm_gs", "NDVIm_intpost", "NDVIm_intpre",   "NDVIm_int",  "NDVIm_max", "NDVIm_mean", "NDVIm_min",  
              "NDVIm_slope_max.rsp",  "NDVIm_slope_min.rau")
rainnames <- c("CUMRAIN_1", "CUMRAIN_17", "CUMRAIN_33", "CUMRAIN_49", "CUMRAIN_65", "CUMRAIN_81", 
               "CUMRAIN_97", "CUMRAIN_113", "CUMRAIN_129",  "CUMRAIN_145", "CUMRAIN_161", "CUMRAIN_177", 
               "CUMRAIN_193", "CUMRAIN_209", "CUMRAIN_225", "CUMRAIN_241", "CUMRAIN_257", "CUMRAIN_273",             
               "CUMRAIN_289", "CUMRAIN_305", "CUMRAIN_321", "CUMRAIN_337", "CUMRAIN_353")
tempnames <- c("TMAX_1", "TMAX_17", "TMAX_33", "TMAX_49", "TMAX_65", "TMAX_81", "TMAX_97", "TMAX_113", "TMAX_129", 
               "TMAX_145", "TMAX_161", "TMAX_177", "TMAX_193", "TMAX_209", "TMAX_225", "TMAX_241", "TMAX_257", 
               "TMAX_273", "TMAX_289", "TMAX_305", "TMAX_321")


  data <- stack(myFiles)
  playground <- stack(file.path(dir_modis, "yield_prediction_playground.tif"))
  names(data) <- bandnames # set metrics names of layer to enable predict to work
  cropped <- crop (data, playground)
  names(cropped) <- bandnames # set metrics names of layer to enable predict to work
  
  beginCluster(no_cores)
  yield <- clusterR(cropped, predict, args=list("model"=gp_gridsearch))
  endCluster()

  writeRaster(yield, file.path(dir_data, "yield_features", paste0("yield_predicted_","2014",".tif")))
  plot(yield)
}
