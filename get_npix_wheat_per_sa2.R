library(raster)
library(tools)
library(sf)


# Input parameters ----
year <- 2015
ctype <- "wheat"
r_wgs_name <- paste0("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/",year,"/MOD13Q1_croptype_AUS_",year,"_prob_", ctype,"_masked_albers_v2.tif")
r_wgsmasked_name <- paste0("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/",year,"/MOD13Q1_croptype_AUS_",year,"_prob_", ctype,"_masked_v2.tif")
s_name <- "C:/Users/wal716/Documents/projects/digiscape/data/yield_machine_learning/sa2_area/est_sa2_015.shp"
r_proj <- paste0(file_path_sans_ext(r_wgs_name), "_albers.tif")

# Reproject to Australia Albers Equal Area Conic ----
r <- raster(r_wgs_name)
r_mask <- raster("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/2015/GFSAD30AUNZCNMOCE_2015_australia_modis.tif")
r_mask[r_mask < 2] <- NA
r_masked <- mask(r, r_mask, filename = r_wgsmasked_name)



system(paste0('gdalwarp ', r_wgsmasked_name, ' ', r_proj, ' -t_srs "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" -co "COMPRESS=LZW" -co "TILED=YES" -overwrite'))

# Transform to points----
r <- raster(r_proj)
names(r) <- "prob"
pix_area <- res(r)[1]*res(r)[2]/10000
rp <- rasterToPoints(r, spatial=T)
rp$id <- 1:nrow(rp@data)

# read SA2 shapefile----
s_sa2 <- st_read(s_name)
s_sa2_albers <- st_transform(s_sa2, projection(rp)) %>% 
  filter(str_detect(com_des, 'Wheat for grain - Area'))


# loop through SA2 shapefile and create a list of all the point index that will be kept ----
sel_list <- list()
for (sa2_name in unique(s_sa2_albers$SA2_NAME16)){
  print(sa2_name)
  s_sa2_sel <- s_sa2_albers  %>% 
    filter(SA2_NAME16 == sa2_name)
  npix_sa2 <- round(as.numeric(as.character(s_sa2_sel$Estimate))/pix_area)
  
  s_sa2_sel <- as(s_sa2_sel, "Spatial")
  p_in <- rp[s_sa2_sel, ]
  p_t <- min(sort(p_in$prob, decreasing = T)[1:ifelse( nrow(p_in@data) > npix_sa2, npix_sa2, nrow(p_in@data))])
  sel_i <- p_in$id[which(p_in$prob >= p_t)]
  sel_list[[sa2_name]] <- sel_i
}
rp_sel <- rp[unlist(sel_list), ]
shapefile(rp_sel, paste0("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/",year,"/MOD13Q1_croptype_AUS_",year,"_wheat_v2.shp"))
rp_sel_gsr80wgs84 <- spTransform(rp_sel, CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
shapefile(rp_sel, paste0("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/",year,"/MOD13Q1_croptype_AUS_",year,"_wheat_wgs84GSR80_v2.shp"))
rp_sel_wgs84 <- spTransform(rp_sel, CRS("+proj=longlat +ellps=WGS84 +no_defs"))
shapefile(rp_sel, paste0("C:/Users/wal716/Documents/projects/digiscape/data/MODIS_crop_mapping/",year,"/MOD13Q1_croptype_AUS_",year,"_wheat_wgs84_v2.shp"))
