# interpolate missing values function 

intLinear <- function(x, date) calc(x, fun = function(y){
  n <- length(y)
  y[y < 0] <- NA
  
  inna <- which(!is.na(y))
  
  if (length(inna) == 0){
    out<-rep(0,n)
  } else if (length(inna) == n) {
    out <- y
  } else {
    # if first and last values are missing, copy them from first and observed values
    if(is.na(y[1])){y[1]<-y[inna[1]]}
    if(is.na(y[n])){y[n]<-y[tail(inna,1)]}
    inna <- which(!is.na(y))
    x.init <- date[inna]
    # remove all the NA for building the model
    interp <- approx(x.init,y[inna],date)
    out <- interp$y
  } 
  return(out)
})

library(raster)
library(tools)

### RASTER FILES INTERPOLATION

rasterOptions(tmpdir= "//OSM/CBR/AF_DIGI_RS/work/tmp", progress = "text")
ncpus <- 10
path <- "//OSM/CBR/AF_DIGI_RS/work/digiscape/data/modis"

myFiles <- list.files(path, pattern="2009")
for (myFile in myFiles){
  print(paste("Processing year", myFile))
  rootDir <- file.path(path, myFile)
  setwd(rootDir)
  
  r.in.names <- list.files(".", "MOD13Q1.006_NDVI_aus.*.flt$")
  s.in <- stack(r.in.names)
  
  date.list.n <- unlist(lapply(strsplit(file_path_sans_ext(basename(r.in.names)), '[.]'), function(x) x[length(x)]))
  date.list <- as.numeric(date.list.n)
  date.list <- as.numeric(date.list - date.list[1])
  
  r.out.list <- file.path(rootDir, paste0('MOD13Q1_006_NDVI_AUS_', date.list.n,'_linint.tif'))
  
  
  if (file.exists(r.out.list[1]) == F){
    beginCluster(ncpus)
    s.tinterp <- NULL
    try(s.tinterp <- clusterR(s.in, intLinear, args=list(date = date.list)))
    endCluster()
    
    try(writeRaster(s.tinterp, filename = r.out.list, bylayer=TRUE, options=c("COMPRESS=LZW", "BIGTIFF=YES", 'TILED=YES'), datatype='INT4S',overwrite=T))
    
    removeTmpFiles(h=0)
  }
}



