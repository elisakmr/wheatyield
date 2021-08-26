if ((Sys.info()["nodename"]) == "sc-05-cdc") {
  dir_data <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data"
  dir_plot <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/figures"
} else if ((Sys.info()["nodename"]) == "MULLET-SL") {
  dir_data <- "X:/projects/yield_machinelearning/data"
  dir_plot <- "X:/projects/yield_machinelearning/figures "
}

library(raster)
library(readr)
library(ggplot2)
### CREATING DATA FRAME OF RAW NDVI + FITTED NDVI SERIES

data <- read_csv(file = file.path(dir_data, "Full","nps_full_ndvi.csv")) # load df containing all ndvi series
serie <- data.frame(t(data[3289,10:32]/10000)) # extract one row: 1 ndvi serie for wheat in 2015
ndvi_day <- unlist(rownames(serie))
serie$Day <- unlist(lapply(strsplit(ndvi_day, "_"), function(x) x[length(x)]))
p <-phenopix::FitDoubleLogBeck(serie[,1], probs=c(0.01, 0.99)) # smoothing
serie$fitted <- as.numeric(as.character(p$predicted)) # add fitted values to df with row values 
colnames(serie)<- c("NDVI", "Day", "fitted")

### PLOTTING NDVI CURVE + SMOOTHED CURVE

plot  <- ggplot(serie, aes(x = Day))+
  geom_line(aes(y = fitted), colour="red", group = 1, lwd = 0.7)+
  geom_point(aes(y = NDVI), colour="#00CC66", size = 2.2)+
  theme_minimal()+
  theme(axis.title.y=element_text(angle = 0, margin=margin(10)), axis.title.x=element_text(hjust=0.99))

plot + geom_vline(aes(xintercept = 15), color = "black", linetype = "dashed") +
  geom_vline(aes(xintercept = 11.5), color = "black", linetype = "dashed") +
  geom_vline(aes(xintercept = 17.7), color = "black", linetype = "dashed") +
  annotate(geom = "text", label = "Smax", x = 10.5, y = 0.52, color = "black") +
  annotate(geom = "text", label = "Peak", x = 14.3, y = 0.85, color = "black") +
  annotate(geom = "text", label = "Smin", x = 16.7, y = 0.52, color = "black") +
  ylab("NDVI")
  ylim(0,1)
ggsave("X:/projects/yield_machinelearning/figures/NDVI_smooth2.png",
       width = 6.84, height = 4.6, dpi = 300)

