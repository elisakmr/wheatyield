#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).", call.=FALSE)
}


# functions

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
    
    # Additional checks
    min1_check <- mean(y[1:5])
    min2_check <- mean(y[22:23])
    max_check <- max(y)
    amp_check <- max_check - min1_check
    
    if (min1_check < 3200 & min2_check < 3200){
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
  } else {
    y <- y[2:length(y)]
    out <- rep(0, 35)
  }
  
  return(out)
})


# packages
library(raster)
library(tools)



# path <- "C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/2015/chip_2015"
# outpath <- "C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/2015/outchip_2015"
# ncpus <- 7
# rasterOptions(tmpdir= "C:/Users/wal716/Documents/tmp", progress = "text")

path <- "//OSM/CBR/AF_DIGI_RS/work/digiscape/data/chip_2015"
outpath <- "//OSM/CBR/AF_DIGI_RS/work/digiscape/data/outchip_2015"
ncpus <- 8
rasterOptions(tmpdir= "//OSM/CBR/AF_DIGI_RS/work/tmp", progress = "text")


i     <- as.numeric(args[1])


myFiles <- list.files(path, ".tif$", full.names = T)[i]

beginCluster(ncpus)
for (myFile in myFiles){
  print(paste("Processing year", myFile))
  
  s.in <- stack(myFile)

  #date.list.n <- unlist(lapply(strsplit(file_path_sans_ext(basename(r.in.names)), '[_]'), function(x) x[length(x)-1]))
  #date.list <- as.numeric(date.list.n)
  #date.list <- (as.numeric(date.list - date.list[1]))+1
  
  date.list <- seq(1, 353, 16)
  
  r.out.list <- file.path(outpath, paste0('yMOD13Q1_006_NDVI_AUS_', 
                                          c(paste0(sprintf("%03d", date.list) ,'_NDVIfilter.tif'), 
                                            c("NDVImin.tif", "NDVImax.tif", "NDVIamp.tif", "NDVIdayslopemax.tif", "NDVIslopemax.tif",
                                              "NDVIdayslopemin.tif", "NDVIslopemin.tif", "NDVIgs.tif", "NDVImean.tif", "NDVIintpre.tif",
                                              "NDVIintpost.tif", "NDVIint.tif"))))
  chipid <- gsub("stack_2015_", "", basename(myFile))
  
  
  
  r.out.list <-paste0(file_path_sans_ext(r.out.list), chipid)
  
  if (file.exists(r.out.list[1]) == F){
    #s.in.crop <- crop(s.in, extent(111.9988, 154.0012, -25, -9.99875))
    
    
    s.metrics <- NULL
    try(s.metrics <- clusterR(s.in, metricExtraction, args=list(date = date.list)))
    
    
    try(writeRaster(s.metrics, filename = r.out.list, bylayer=TRUE, options=c("COMPRESS=LZW", "BIGTIFF=YES", 'TILED=YES'), overwrite=T))
    
    removeTmpFiles(h=0)
  }
}
endCluster()