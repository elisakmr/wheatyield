library(tidyverse)
library(groupdata2)
library(tools)

if ((Sys.info()["nodename"]) == "sc-05-cdc") {
  dir_data <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data"
  dir_plot <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/figures"
} else if ((Sys.info()["nodename"]) == "MULLET-SL") {
  dir_data <- "X:/projects/yield_machinelearning/data"
  dir_plot <- "X:/projects/yield_machinelearning/figures "
}


# Crop splitting 
df_transformed <- read.csv(file.path(dir_data, "Full", "df_transformed.csv"))
df_wheat<-subset(df_transformed,Crop=="Wheat")
df_barley<-subset(df_transformed,Crop=="Barley")
df_canola<-subset(df_transformed,Crop=="Canola")

write_csv(df_wheat, file.path(dir_data, 'wheat.csv'))
write_csv(df_barley, file.path(dir_data, 'barley.csv'))
write_csv(df_canola, file.path(dir_data, 'canola.csv'))

# Calibration and validation splitting
myFiles <- c(file.path(dir_data, 'wheat.csv'),
             file.path(dir_data, 'barley.csv'),
             file.path(dir_data, 'canola.csv'))

cal_set<-list()
val_set<-list()
for (myFile in myFiles){
  print(myFile)
  
  # Save crop name
  df_crop <- read_csv(myFile)
  nom <- file_path_sans_ext(basename(myFile)) # extract crop name
  
  for (i in 1:100){
    print(i)
    set.seed(i)
    # Create new ID= paddock_year and create calibration + validation data sets using paddock-year as categorical variable
    df_crop_i <- df_crop %>% 
      unite("paddock_year", c("PaddID","Year"), sep = "_", remove = FALSE) %>% 
      partition(p = 0.2, id_col = "paddock_year")
    
    val_set[[i]] <- df_crop_i[[1]]
    cal_set[[i]] <- df_crop_i[[2]]
    
    #df_crop_i %>% kable()
    # Show test_set
    #cal_set %>% kable()
  }
  val_set[1]
  save(val_set, file = file.path(dir_data, "Validation", paste0("validation_", nom, ".RData")))
  save(cal_set, file = file.path(dir_data, "Calibration",paste0("calibration_", nom, ".RData")))
}
