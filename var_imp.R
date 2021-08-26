if ((Sys.info()["nodename"]) == "sc-05-cdc") {
  dir_data <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data"
  dir_plot <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/figures"
} else if ((Sys.info()["nodename"]) == "MULLET-SL") {
  dir_data <- "X:/projects/yield_machinelearning/data"
  dir_plot <- "X:/projects/yield_machinelearning/figures"
} else if ((Sys.info()["nodename"]) == "ARGON-SL") {
  dir_data <- "W:/projects/yield_machinelearning/data"
  dir_plot <- "W:/projects/yield_machinelearning/figures"
}

library(lattice)
library(caret)
library(dplyr)
library(ggplot2)
library(ggpubr)


crop_list <- c("barley", "canola", "wheat")
p_list <- list()
for (crop in crop_list){
  load(file=file.path(dir_data, "models", paste0("svmr_optimised_", crop, ".RData")))
      data <-  varImp(svmr_gridsearch, useModel=F, nonpara = T)
      col_index <- varImp(svmr_gridsearch, useModel=F, nonpara = T)$importance %>% 
        mutate(names=row.names(.)) %>%
        arrange(-Overall) 
      # selecting 12 best features
      imp_names <- col_index$names[1:14] 
      imp_val <- col_index$Overall[1:14]
      imp_names_clean <- imp_names
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="NDVI_273","NDVI 30/09") 
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="NDVI_289","NDVI 16/10") 
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="NDVI_257", "NDVI 14/09")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="NDVI_241", "NDVI 29/08")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="NDVI_17", "NDVI 17/01")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="NDVI_33", "NDVI 02/02")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="NDVI_225", "NDVI 13/08")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="NDVI_305", "NDVI 01/11")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="NDVI_305", "NDVI 17/11")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="NDVI_209", "NDVI 28/07")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="CUMRAIN_321", "Cum. rain 17/11")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="CUMRAIN_305", "Cum. rain  01/11")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="CUMRAIN_337", "Cum. rain 03/12")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="CUMRAIN_289", "Cum. rain 16/10")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="CUMRAIN_273", "Cum. rain 30/09")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="CUMRAIN_353", "Cum. rain 19/12")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="NDVIm_int", "INT")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="NDVIm_day_slope_min.eos", "TSmin")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="NDVIm_intpost", "INTa")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="NDVIm_intpre", "INTb")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="NDVIm_mean", "Mean")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="NDVIm_max", "Peak")
      imp_names_clean <- replace(imp_names_clean, imp_names_clean=="TMAX_289", "Tmax 16/10")
      
      # creating code column for color setting
      ndvi <- grep("NDVI_", imp_names)
      metric <- grep("m_", imp_names)
      weather <- c(grep("TMAX", imp_names), grep("CUMRAIN", imp_names))
      
      #creating data frame to be plotted
      best_var <- data.frame("feature"= imp_names_clean, "importance"=imp_val)
      best_var$cat_code <- 0
      best_var$cat_code[ndvi] <- 1
      best_var$cat_code[metric] <- 2
      best_var$cat_code[weather] <- 3
      best_var$cat_code <- factor(best_var$cat_code, levels = c(1, 2, 3)) 
      #install.packages("wesanderson")
      library(wesanderson)
      
      p_list[[crop]] <- ggplot(best_var, aes(x = reorder(feature, importance), y = importance, color = cat_code)) +
        geom_point(size=2) +
        scale_color_manual(breaks = c( "1", "2", "3"),
                          labels = c("NDVI value", "NDVI metric", "Climate variable"),
                          values=c("seagreen2", "hotpink", "dodgerblue1")) + 
        labs(y = "Relative importance (%)", x="") +
        scale_y_continuous(breaks = seq(65, 105, 5))+ scale_x_discrete(expand = c(0.05, 0.0)) +
        coord_flip() +
        theme_minimal() +
        theme(axis.text = element_text(color = "black"), axis.ticks = element_blank(), axis.ticks.y = element_blank(),
            legend.position = "bottom", legend.title=element_blank(), panel.grid.major.x = element_blank(), 
            panel.border = element_blank(), axis.text.y=element_text(size=10), axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=12), plot.margin = margin(t = 40, r = 10, b = 0, l = 0))
      if(crop == "wheat"){
        ggplot(best_var, aes(x = reorder(feature, importance), y = importance, color = cat_code)) +
          geom_point(size=2) +
          scale_color_manual(breaks = c( "1", "2", "3"),
                             labels = c("NDVI value", "NDVI metric", "Climate variable"),
                             values=c("#00A9CE", "#71CC98", "#00313C")) + 
          labs(y = "Relative importance (%)", x="") +
          scale_y_continuous(breaks = seq(75, 100, 5), expand = c(0.05, 0.05))+ scale_x_discrete(expand = c(0.05, 0.0)) +
          coord_flip() +
          theme_minimal() +
          theme(axis.text = element_text(color = "black"), axis.ticks = element_blank(), axis.ticks.y = element_blank(),
                legend.position = "bottom", legend.title=element_blank(),
                panel.border = element_blank(), axis.text.y=element_text(size=10), axis.text.x=element_text(size=10),
                axis.title.x=element_text(size=12), plot.margin = margin(t = 40, r = 10, b = 0, l = 0))
        ggsave("W:/projects/yield_machinelearning/figures/varimp_svr2_wheat.png",
               width = 5.5, height = 5, dpi = 500)
        
      }
}

      p_all <- ggarrange(p_list$barley, p_list$canola, p_list$wheat,
                         labels=c("Barley", "Canola", "Wheat"),
                         font.label = list(size = 10, color = "black", face = "bold"),
                         vjust = 3,
                         label.x = 0.5,
                         ncol = 3, nrow = 1,
                         common.legend = TRUE, 
                         legend = "bottom")
                         
      p_all 
      
      ggsave("X:/projects/yield_machinelearning/figures/varimp_svr2.png",
             width = 10.06, height = 3.89, dpi = 300)
