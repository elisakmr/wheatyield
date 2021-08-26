getAccuracyMethod <- function(method, predicted, observed) {
  library(epiR)
  rmse_val <- rmse(observed,predicted)
  mape_val <- mape(observed,predicted)
  lm_mdl  <- lm(y~x, data.frame(x = observed, y = predicted))
  rsq_val <- summary(lm_mdl)$r.square
  ccc <- as.numeric(epi.ccc(observed, predicted)$rho.c[1])
  return(data.frame(Algorithm = method, Accuracy = c("RMSE", "MAPE", "Rsquare", "CCC"), Value = c(rmse_val, mape_val, rsq_val, ccc)))
}

if ((Sys.info()["nodename"]) == "sc-05-cdc") {
  dir_data <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data"
  dir_plot <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/figures"
  no_cores <- 10
  machine <- "server"
} else if ((Sys.info()["nodename"]) == "MULLET-SL") {
  dir_data <- "X:/projects/yield_machinelearning/data"
  dir_plot <- "X:/projects/yield_machinelearning/figures"
  no_cores <- 2
  machine <- "computer"
}

library(magrittr)
library(lattice)
library(sp)
library(raster)
library(Cubist)
library(randomForest)
library(xgboost)
library(e1071)
library(neuralnet)
library(kernlab)
library(caret)
library(dplyr)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)
library(tools)
library(tidyverse)
source("metrics.R")

myFiles <- list()
myFiles[["calibration"]] <- list.files(path=file.path(dir_data, "Calibration"), pattern="calibration",full.names=TRUE) # gather all the calibration data
myFiles[["validation"]] <- list.files(path=file.path(dir_data, "Validation"), pattern="validation",full.names=TRUE) # gather all the validation data

crop_list <- sapply(myFiles$calibration, function(x) strsplit(basename(file_path_sans_ext(x)), "_")[[1]][2])
df_perf <- data.frame()



cl <- makeCluster(no_cores)
registerDoParallel(cl)

for (crop_i in crop_list){ # crop_i = calibration data frames per crop
  print(crop_i)
  load(grep(crop_i, myFiles$calibration, value = T))
  load(grep(crop_i, myFiles$validation, value = T))
  
  df_performance <- foreach (i = 1:100, .combine = rbind, .packages=c( "randomForest", "Cubist", "lattice", "sp", "raster", "xgboost","kernlab", "e1071", "neuralnet", "caret","tidyverse"))  %dopar%  { # cal_set = one calibration data frame (among the 100 generated) length(cal_set)
    
    df_cal_i <- cal_set[[i]] %>% 
      dplyr::rename(yield = Yield..t.ha.) %>% 
      dplyr::select(one_of("yield"), starts_with("NDVI")) %>% # subset data frame that contain only the variables needed for algorithms training
      na.omit()
    

    ### Setting parameters grids
    
    # randomForest
    gridRF <- expand.grid(mtry=c(8,10))
    # xgboost
    gridXB = expand.grid(nrounds = 1000, eta = c(0.01, 0.19),max_depth = c(2, 10),gamma = 0, colsample_bytree = 0.8, min_child_weight = 1, subsample = 1)
    # cubist
    gridCUB <- expand.grid(committees = c(1, 5), neighbors = c(1, 7))
    # svm
    gridSVM<- expand.grid( C = c(0.001, 1))
    gridSVM
    # neural network
    gridNN <- expand.grid(size=c(10, 12), decay=c(0.1, 0.2))
    
    
    ### Training algorithms
    
    rf_gridsearch <- caret::train(yield~., data = df_cal_i, method="rf", preProcess = c('center', 'scale'), tuneGrid=gridRF)
    
    cub_gridsearch <- caret::train(yield~., data = df_cal_i, method = "cubist",preProcess = c('center', 'scale'), tuneGrid = gridCUB)
    
    xb_gridsearch <- caret::train(yield~., data = df_cal_i, method = "xgbTree", preProcess = c('center', 'scale'), tuneGrid = gridXB)
    
    svm_gridsearch <- caret::train(yield~., data = df_cal_i, method = "svmLinear", preProcess = c('center', 'scale'), tuneGrid = gridSVM)
    
    nn_gridsearch <- caret::train(yield~., data = df_cal_i, method = 'nnet', preProcess = c('center', 'scale'), tuneGrid=gridNN)

    test_reg_none_model <- caret::train(x = trainX, y = trainY, 
                                        method = "mlpML", 
                                        preProc =  c('center', 'scale'),
                                        trControl = trainControl(method = "cv", number = 3),
                                        tuneGrid = expand.grid(layer1 = 3:10, layer2 = 3:10, layer3 = 3:10))
    
    ### Validation 
    df_val_i <- val_set[[i]] %>% 
      dplyr::rename(yield = Yield..t.ha.) %>% 
      dplyr::select(one_of("yield"), starts_with("NDVI")) %>% 
      na.omit()
    
    ### Calculating and extracting performance metrics 
    
    predicted_rf <- c(predict(rf_gridsearch, df_val_i))

    predicted_cub <- c(predict(cub_gridsearch, df_val_i))
    
    predicted_xb <- c(predict(xb_gridsearch, df_val_i))
    
    predicted_svm <- c(predict(svm_gridsearch, df_val_i))
    
    predicted_nn <- c(predict(nn_gridsearch, df_val_i))
    
    
    perf <- rbind(getAccuracyMethod("Random Forest", predicted_rf, df_val_i$yield),
                  getAccuracyMethod("Cubist", predicted_cub, df_val_i$yield),
                  getAccuracyMethod("XgBoost", predicted_xb, df_val_i$yield),
                  getAccuracyMethod("Support Vector Machine", predicted_svm, df_val_i$yield),
                  getAccuracyMethod("Neural Network", predicted_nn, df_val_i$yield))
    data.frame(perf, iteration = i)
  }
  
  save(df_performance, file = file.path(dir_data, "Performance", paste0("performance_", crop_i, "_", machine, ".RData")))
}
stopCluster(cl)
    






