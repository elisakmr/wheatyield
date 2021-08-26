if ((Sys.info()["nodename"]) == "sc-05-cdc") {
  dir_in <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data/out_pearcy"
  dir_data <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data"
  dir_plot <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/figures"
  no_cores <- 10
} else if ((Sys.info()["nodename"]) == "MULLET-SL") {
  dir_in <- "X:/projects/yield_machinelearning/data/out_pearcy"
  dir_data <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data"
  dir_plot <- "X:/projects/yield_machinelearning/figures"
  no_cores <- 2
}else if ((Sys.info()["nodename"]) == "ARGON-SL") {
  dir_in <- "W:/projects/yield_machinelearning/data/out_pearcy"
  dir_plot <- "W:/projects/yield_machinelearning/figures"
  no_cores <- 5
}

# Function: Get upper triangle of the correlation matrix

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}


library(dplyr)
library(tidyverse)
library(readr)
library(reshape2)
library(ggplot2)
library(viridis)
library(ggpubr)
crop_list <- c("canola", "wheat", "canola")

p_list <- list()
for (crop in crop_list) {
  files <- list.files(dir_in, pattern = paste0("performance_", crop), full.names = T)
  
  ### PERFORMANCES------------------------------
  
  ### With weather inputs
  
  filesw <- files[-grep("now", files)]
  # read time series
  df1 <- filesw %>%
    map(read_csv) %>%    # read in all the files individually, using the function read_csv() from the readr package
    reduce(rbind)  # reduce with rbind into one dataframe
  write_csv(df1, paste0("sum_performance_", crop, ".csv")) #save dataframe binding all the iterations
  
  df2 <- df1 %>%  
    group_by(Algorithm, Accuracy) %>% 
    select(Algorithm, Accuracy, Value, iteration) %>% 
    summarise(Mean=mean(Value)) %>% 
    spread(Algorithm, Mean)
  write_csv(df2,  paste0("average_performance_", crop, ".csv")) # save summarized performance
} 
  ### Without weather inputs
  
  filesnow <- files[grep("now", files)]
  
  # read time series
  df1nw <- filesnow %>%
    map(read_csv) %>%    # read in all the files individually, using the function read_csv() from the readr package
    reduce(rbind)  # reduce with rbind into one dataframe
  write.csv(df1nw, file = paste0("sum_performance_now_", crop, ".csv")) #save dataframe binding all the iterations
  
  df2nw <- df1nw %>%  
    group_by(Algorithm, Accuracy) %>% 
    select(Algorithm, Accuracy, Value, iteration) %>% 
    summarise(Mean=mean(Value)) %>% 
    spread(Algorithm, Mean)
  #save(df, file = file.path(dir_data, "Performance", paste0("sum_performance_", crop_i, ".csv")))
  write.csv(df2nw, file = paste0("average_performance_now_", crop, ".csv")) # save summarized performance
}
  
  ### CORRELATION MATRIX---------------------------
  
 
  crop_list<- c("barley", "canola", "wheat")
  for (crop in crop_list){
    myCor <- list.files(dir_in, pattern = paste0("cor_", crop), full.names = T )
    
    ### WITH WEATHER inputs
    
    myCorw <- myCor[-grep("now", myCor)]
    df_corw <- myCorw %>%
      map(read_csv) %>%    # read in all the files individually, using the function read_csv() from the readr package
      reduce(rbind) 
    
    df_corw_ag <- aggregate(df_corw[-1], df_corw[1], mean, na.rm=TRUE)
    df_corw_ag <- df_corw_ag[order(match(df_corw_ag$X1,colnames(df_corw_ag)[2:ncol(df_corw_ag)])), ] # reorder rows
    df_corw_up <- cbind(X1 = df_corw_ag$X1, get_lower_tri((df_corw_ag[,2:10])))  # delete duplicates correlations
    

    df_corw_up <- df_corw_up %>% 
      gather(variable, value, -X1) %>%
      mutate(X1=recode(X1, `predicted_rf` = "RF",
                       `predicted_cub` = "CUB", 
                       `predicted_xb` = "XB",
                       `predicted_svm` = "SVMl",
                       `predicted_svmr`= "SVMr",
                       `predicted_nn` = "MLP",
                       `predicted_ea` = "MARS",
                       `predicted_gp` = "GP",
                       `predicted_knn`= "kNN" )) %>% 
      mutate(variable=recode(variable, `predicted_rf` = "RF",
                             `predicted_cub` = "CUB", 
                             `predicted_xb` = "XB",
                             `predicted_svm` = "SVMl",
                             `predicted_svmr`= "SVMr",
                             `predicted_nn` = "MLP",
                             `predicted_ea` = "MARS",
                             `predicted_gp` = "GP",
                             `predicted_knn`= "kNN" ))
    
    
    df_corw_up <- transform(df_corw_up,X1=factor(X1,levels=unique(X1)), 
                            variable=factor(variable,levels=unique(variable))) # keep axis label order same as in data frame
    
    p <- ggplot(data = na.omit(df_corw_up), aes(x=X1, y=variable, fill=value)) + 
      geom_tile(color = "white")+
      coord_fixed() +
      scale_x_discrete("", expand = c(0,0)) + scale_y_discrete("", expand = c(0,0)) +
      scale_fill_viridis(name="Correlation", begin = 0, end = 1, limit = c(0.4, 1), na.value = "white") +
      theme_minimal()+ 
      theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))+
      guides(fill = guide_colourbar(title.position="top",title.hjust = 0.5))
    
    if(crop == "wheat"){
      print(p)
      ggsave("W:/projects/yield_machinelearning/figures/cor_map_wheat.png",
             width = 5, height = 6, dpi = 400)
    }
    
    p_list[[crop]] <- p 
  }
  
}

p_all <- ggarrange(p_list$barley, p_list$canola, p_list$wheat,
                   labels=c("Barley", "Canola", "Wheat"),
                   font.label = list(size = 11, color = "black", face = "bold"),
                   vjust = 2,
                   hjust = - 3.5,
                   ncol = 3, nrow = 1,
                   common.legend = TRUE, legend = "bottom")
p_all

ggsave("X:/projects/yield_machinelearning/figures/algorithm_training/cor_map.png",
      width = 10.86, height = 4.95, dpi = 400)
