## Smoothing and metrics deriving function

metricExtraction <- function(x, date) calc(x, fun = function(y){
  integrale <- function(x, start, end){
    maxi <- which(x == max(x))
    if (start=="start" & end=="max") {
      out <-  sum(x[1:maxi])
    } else if  (start=="max"& end=="end") {
      out <- sum(x[maxi:length(x)])
    } else if  (start=="start"& end=="end") {
      out <- sum(x)
    } 
    out
  }
  y <- as.numeric(y)
  if((y[1] == 2)){ # to prevent non-land area processing
    y <- y[2:length(y)]
    #Days serie
    t_new <- 1:max(date)
    interp <- approx(date, y, t_new)
    NDVI <- interp$y
    # smoothing and metrics deriving 
    p <- try(phenopix::FitDoubleLogBeck(NDVI, probs=c(0.01, 0.99)))
    
    if(is(p,"try-error")) {
      out <- rep(0, 35)
    } else {
      out1 <- as.numeric(p$predicted)
      mini <- min(out1)
      border <- mini*1.1
      out2 <- out1-border
      out2[out2 <=0 ] <- 0
      metrics <- c(NDVIm_min=min(out1, na.rm = TRUE),
                   NDVIm_max = max(out1, na.rm = TRUE), 
                   NDVIm_amp = max(out2, na.rm = TRUE),
                   NDVIm_day_slope_max=p$params[3],
                   NDVIm_slope_max=p$params[4],
                   NDVIm_day_slope_min=p$params[5],
                   NDVIm_slope_min=p$params[6],
                   NDVIm_gs = sum(out2!=0),
                   NDVIm_mean = mean(out2[out2 != 0]),
                   NDVIm_intpre=integrale(out2, start = "start", end = "max"),
                   NDVIm_intpost=integrale(out2, start = "max", end = "end"),
                   NDVIm_int=integrale(out2, start = "start", end = "end"))
      
      ndvi_out <- round(out1[date])
      names(ndvi_out) <- paste0("NDVI_", date)
      out <- c(ndvi_out, metrics)
    }
  } else {
    y <- y[2:length(y)]
    out <- rep(0, 35)
  }
  
  return(out)
})

library(raster)
library(tools)

### APPLYING FUNCTION ON RASTER FILES



if ((Sys.info()["nodename"]) == "sc-05-cdc") {
  path <- "//OSM/CBR/AF_DIGI_RS/work/digiscape/data/modis"
  ncpus <- 16
  rasterOptions(tmpdir= "//OSM/CBR/AF_DIGI_RS/work/tmp", progress = "text")
} else if ((Sys.info()["nodename"]) == "MULLET-SL") {
  dir_data <- "X:/projects/yield_machinelearning/data"
  dir_plot <- "X:/projects/yield_machinelearning/figures"
  ncpus <- 2
}


r.mask <- stack("//OSM/CBR/AF_DIGI_RS/work/digiscape/data/GFSAD30AUNZCNMOCE_2015_australia_modis.tif")
# s.in <- crop(s.in, extent(117.6038, 117.65, -32.53875, -32.49))



myFiles <- rep(2015, 3)
for (myFile in myFiles){
  print(paste("Processing year", myFile))
  rootDir <- file.path(path, myFile)
  setwd(rootDir)
  
  r.in.names <- list.files(".", "MOD13Q1.*linint.tif$")
  s.in <- stack(r.in.names)
  s.in <- stack(r.mask, s.in)
  date.list.n <- unlist(lapply(strsplit(file_path_sans_ext(basename(r.in.names)), '[_]'), function(x) x[length(x)-1]))
  date.list <- as.numeric(date.list.n)
  date.list <- (as.numeric(date.list - date.list[1]))+1
  
  r.out.list <- file.path(rootDir, paste0('yMOD13Q1_006_NDVI_AUS_', 
                                            c(paste0(date.list.n ,'_NDVIfilter.tif'), 
                                              c("NDVImin.tif", "NDVImax.tif", "NDVIamp.tif", "NDVIdayslopemax.tif", "NDVIslopemax.tif",
                                                "NDVIdayslopemin.tif", "NDVIslopemin.tif", "NDVIgs.tif", "NDVImean.tif", "NDVIintpre.tif",
                                                "NDVIintpost.tif", "NDVIint.tif"))))
  

  if (file.exists(r.out.list[1]) == F){
    #s.in.crop <- crop(s.in, extent(111.9988, 154.0012, -25, -9.99875))
    
    beginCluster(ncpus)
    s.metrics <- NULL
    try(s.metrics <- clusterR(s.in, metricExtraction, args=list(date = date.list)))
    endCluster()
    
    try(writeRaster(s.metrics, filename = r.out.list, bylayer=TRUE, options=c("COMPRESS=LZW", "BIGTIFF=YES", 'TILED=YES'), overwrite=T))
    
    removeTmpFiles(h=0)
  }
}
