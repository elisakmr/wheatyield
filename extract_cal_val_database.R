
pol2raster <- function(s_in_name, r_tgt_name, att, r_out_name = "default", out = T){
  
  r_tgt <- raster(r_tgt_name)
  #get the extent and change to minx, miny, maxx, maxy order for use
  #in gdal_rasterize. Explanation below
  ext<-raster::extent(r_tgt)
  ext<-paste(ext[1], ext[3], ext[2], ext[4])
  
  #get the resolution of the raster. will be used in gdal_rasterize
  #for target resolution which should be the same as the source resolution.
  #Specifying makes it run faster (?)
  res<-paste(raster::res(r_tgt)[1], raster::res(r_tgt)[2])
  
  lyrName <- tools::file_path_sans_ext(basename(s_in_name))
  
  if( r_out_name == "default"){
    r_out_name <- paste0(tools::file_path_sans_ext(s_in_name), "_", att, ".tif")
  }
  
  #Gdal_rasterize
  message("Creating raster")
  command<-'gdal_rasterize'
  #Speed-up with more cache (avice: max 1/3 of your total RAM)
  command<-paste(command, paste0("--config GDAL_CACHEMAX ", 4000))
  command<-paste(command, "-l", lyrName)
  #Identifies an attribute field on the features to be used for a burn
  #in value. The value will be burned into all output bands.
  command<-paste(command, "-a", att) 
  #(GDAL >= 1.8.0) set georeferenced extents. The values must be expressed
  #in georeferenced units. If not specified, the extent of the output file
  #will be the extent of the vector layers.
  command<-paste(command, "-te", as.character(ext))
  #(GDAL >= 1.8.0) set target resolution. The values must be expressed in
  #georeferenced units. Both must be positive values.
  command<-paste(command, "-a_nodata 0")
  command<-paste(command, "-tr", res)
  command<-paste(command, s_in_name)
  command<-paste(command, r_out_name)
  command<-paste(command, "-co COMPRESS=LZW -co TILED=YES") 
  
  system(command)
  
  if(isTRUE(out)){
    return(raster(r_out_name))
  }
}

RF_raster_predict_prob <- function(inraster,rfModel,...)
{
  
  # We need to load randomForest (note this is only
  # loaded once per worker on most cluster setups):
  require(randomForest)
  
  # First, preserve the names:
  band_names <- dimnames(inraster)[3][[1]]
  print(band_names)
  
  inraster_matrix <- inraster
  dim(inraster_matrix) <- c(dim(inraster)[1]*dim(inraster)[2],dim(inraster)[3])
  
  inraster.df <- as.data.frame(inraster_matrix)
  
  # We need to re-set the names because randomForest requires it:
  names(inraster.df) <- band_names
  
  # Now we can use this in a predict statement:
  out_predictions <- predict(rfModel,inraster.df, type = "p")
  out_predictions <- apply(out_predictions,1,function(x) max(x)   )
  out_predictions_array <- array(as.double((as.character(out_predictions))),dim=c(dim(inraster)[1:2],ncol(out_predictions)))
  return(out_predictions_array)
}

library(raster)
library(sf)
library(randomForest)
library(tidyverse)
library(caret)

r_tgt_name <- "C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/2015/MOD13Q1_006_HARM_AUS_2015.tif"

for(year in 2015:2015){
  s_in_name <- paste0("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/in_situ_data/",year,"/yieldprophet/AUS_NAT_CT_YP_",year,"_final.shp")
  
  att <- "CODE"
  
  r_code <- pol2raster(s_in_name, r_tgt_name, att="CODE", out = T)
  names(r_code) <- "CODE"
  r_id <- pol2raster(s_in_name, r_tgt_name, att="ID", out = T)
  names(r_id) <- "field_ID"
  
  s_name <- paste0("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/",year,"/MOD13Q1_006_HARM_AUS_",year,".tif")
  s_year <- stack(s_name)
  names(s_year) <- paste0("HARM_", 1:nlayers(s_year))
  
  s <- stack(stack(r_code, r_id), s_year)
  
  rp <- rasterToPoints(r_id, spatial=T)
  df_xt <- raster::extract(s, rp, df = T)
  df_xt$year <- year
  
  
}



for(ctype in c("wheat", "canola")){
  
  df <- df_xt %>% 
    dplyr::select(starts_with("HARM"), one_of("CODE")) %>% 
    mutate(CODE = as.factor(CODE))
  if(ctype == "canola"){
    levels(df$CODE)[levels(df$CODE) == "435"] <- '1'
    df$CODE <- as.factor(as.numeric(as.character(df$CODE)))
  }
  
  rf_mdl <- randomForest(CODE~., df, trees = 500, mtry = 5)
  
  beginCluster(6)
  preds_rf<- clusterR(s_year, predict, args = list(model = rf_mdl, type = "prob"))
  endCluster()
  writeRaster(preds_rf, paste0("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/",year,"/MOD13Q1_croptype_AUS_",year,"_prob_", ctype,"_v2.tif"), options = c("COMPRESS=LZW", "TILED=YES"))
  
  r_mask <- raster("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/2015/GFSAD30AUNZCNMOCE_2015_australia_modis.tif")
  r_mask <- r_mask == 2
  preds_rf[r_mask!=1] <- NA
  writeRaster(preds_rf, paste0("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/",year,"/MOD13Q1_croptype_AUS_",year,"_prob_", ctype,"_masked_v2.tif"), options = c("COMPRESS=LZW", "TILED=YES"))
  
  s_wheat <- rasterToPoints(preds_rf, spatial=T)
}




####

raster_info_uncertainty <- function(inStk, c_name, mdl, ncpus=3, filename='',scale=100,...){
  require(doParallel)
  require(spatial.tools)
  raster_info_uncertainty <- function(inraster,mdl,...){ 
    EDI_c <- function(x, c_name){
      p_c <- x[ colnames(x) == c_name]
      EDI <- log(p_c) - (1-p_c)^(-1)  * sum( x[ x > 0 & x != p_c] * log(x[ x > 0 & x != p_c]))
      return(EDI)
    }
    # We need to load randomForest (note this is only
    # loaded once per worker on most cluster setups):
    
    # First, preserve the names:
    band_names <- dimnames(inraster)[3][[1]]
    print(band_names)
    
    inraster_matrix <- inraster
    dim(inraster_matrix) <- c(dim(inraster)[1]*dim(inraster)[2],dim(inraster)[3])
    
    inraster.df <- as.data.frame(inraster_matrix)
    inraster.df <- inraster.df/rowSums(inraster.df)
    # We need to re-set the names because randomForest requires it:
    names(inraster.df) <- band_names
    
    # Now we can use this in a predict statement:
    out_predictions <- apply(inraster.df,1,function(x)  ifelse(is.na((EDI_c(x, c_name))), NA, EDI_c(x, c_name)))
    out_predictions_array <- array(as.double((as.character(out_predictions))),dim=c(dim(inraster)[1:2],1))
    return(out_predictions_array)
  }
  
  
  
  if(is.na(ncpus)){
    ncpus <- detectCores()-1
  }
  
  # This can be modified to fit a specific cluster/multicore computer setup
  cl <- makeCluster(spec = ncpus)
  # Register the cluster with foreach:
  registerDoParallel(cl)
  
  r_unc <- rasterEngine(
    # Match the variable name in the function to the raster:
    inraster=inStk,
    # Assign the function:
    fun=raster_info_uncertainty,
    setMinMax = FALSE,
    compileFunction = TRUE,
    verbose = TRUE
  )
  
  stopCluster(cl) # Stops the cluster
  registerDoSEQ() # (Optional but avoids a warning) registers a sequential backend with foreach.
  
  if(filename!=''){
    writeRaster(r_unc,filename,overwrite=TRUE,options=c("COMPRESS=LZW"),...)
  }
}

r_unc <- raster_info_uncertainty(s_year, 11,rf_mdl, ncpus=6, filename=paste0("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/",year,"/MOD13Q1_croptype_AUS_",year,"_unc_v2.tif"))

