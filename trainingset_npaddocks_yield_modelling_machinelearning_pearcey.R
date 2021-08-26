#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).", call.=FALSE)
}




### Functions -----
library(magrittr)
library(Cubist)
library(randomForest)
library(xgboost)
library(e1071)
library(RSNNS)
library(kernlab)
library(caret)
library(dplyr)
library(rsq)
library(tools)
library(groupdata2)
library(dplyr)
library(tidyr)
library(caretEnsemble)
library(devtools)
library(mlbench)
library(earth)

#### path ----
i     <- as.numeric(args[1])
dir_data <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data"
dir_plot <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/figures"
dir_mdl <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data/models"
dir_out <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data/out_pearcy"


print(i)

### Functions -----

rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2))
}


mape <- function(actual, predicted){
  mean(abs((actual-predicted)/actual) * 100)
}

rmspe <- function(actual, predicted){
  sqrt(mean( ((actual - predicted) / actual)^2))
}




getAccuracyMethod <- function(method, predicted, observed) {
  library(epiR)
  rmse_val <- rmse(observed,predicted)
  rmspe_val <- rmspe(observed,predicted)
  mape_val <- mape(observed,predicted)
  lm_mdl  <- lm(y~x, data.frame(x = observed, y = predicted))
  rsq_val <- summary(lm_mdl)$r.square
  ccc <- as.numeric(epi.ccc(observed, predicted)$rho.c[1])
  return(data.frame(Algorithm = method, Accuracy = c("RMSE", "RMSPE", "MAPE", "Rsquare", "CCC"), Value = c(rmse_val, rmspe_val,  mape_val, rsq_val, ccc)))
}

getSDPrediction <- function(Xtr, Ytr, Xts, method, grid, pp,  k = 25){
  list_out <- list()
  for(i in 1:k) {
    # Generate a bootstrap resample of x rows
    set.seed(i)
    cal_rows <- sample(nrow(Xtr), round((0.95*nrow(Xtr))), replace = TRUE)
    
    # Fit a model
    mdl_fit_boot <- caret::train(x = Xtr[cal_rows, ],
                                 y = Ytr[cal_rows],
                                 method = method,
                                 trControl = trainControl(method = "none"),
                                 tuneGrid = grid,
                                 preProcess = pp)
    
    # Predict onto validation samples
    list_out[[i]] <- predict(mdl_fit_boot, newdata = Xts)
    
  }
  names(list_out) <- paste0("it", 1:k)
  out <- dplyr::bind_rows(list_out)
  out <- apply(out, 1, sd)
  return(out)
}

getSDweight <- function (sd_algo, sd_sum){
  out <- 1/(sd_algo^2)/sd_sum
}


### Start code -----

myFiles <- list()
myFiles[["calibration"]] <- list.files(path=file.path(dir_data, "Calibration"), pattern="calibration_",full.names=TRUE) # gather all the calibration data
myFiles[["validation"]] <- list.files(path=file.path(dir_data, "Validation"), pattern="validation_",full.names=TRUE) # gather all the validation data
crop_list <- sapply(myFiles$calibration, function(x) strsplit(basename(file_path_sans_ext(x)), "_")[[1]][2])
df_perf <- data.frame()

for (crop_i in c("wheat")){ # crop_i = calibration data frames per crop
  ### Load cal/val data ---------------
  print(crop_i)
  load(grep(paste0("calibration_", crop_i, ".RData"), myFiles$calibration, value = T))
  load(grep(paste0("validation_", crop_i, ".RData"), myFiles$validation, value = T))
  
  ### load learners -------------------
  load(file.path(dir_mdl, paste0("rf_optimised_",crop_i, ".RData" )))
  load(file.path(dir_mdl, paste0("cub_optimised_",crop_i, ".RData" )))
  load(file.path(dir_mdl, paste0("xb_optimised_",crop_i, ".RData" )))
  load(file.path(dir_mdl, paste0("svm_optimised_",crop_i, ".RData" )))
  load(file.path(dir_mdl, paste0("svmr_optimised_",crop_i, ".RData" )))
  load(file.path(dir_mdl, paste0("nn_optimised_",crop_i, ".RData" )))
  load(file.path(dir_mdl, paste0("gp_optimised_",crop_i, ".RData" )))
  load(file.path(dir_mdl, paste0("ea_optimised_",crop_i, ".RData" )))
  load(file.path(dir_mdl, paste0("knn_optimised_",crop_i, ".RData" )))
  
  
  outlist <- list()
  for (j in seq(25, 125, by = 5)){
    print(paste("Number of paddocks = ", j))
    ### CROSS VALIDATION i --------------
    # cal_set = one calibration data frame (among the 100 generated) length(cal_set)
    paddock_sel <- unique(cal_set[[i]]$PaddID)[sample(1:length(unique(cal_set[[i]]$PaddID)), j)]
    
    df_cal_i <- cal_set[[i]] %>% 
      filter(PaddID %in% paddock_sel) %>% 
      na.omit()
    
    
    ### Algorithms training
    # Setup CV Folds returnData=FALSE saves some space
    folds = 10
    repeats = 1
    list_folds <- groupdata2::fold(df_cal_i, k = folds, id_col = "paddock_year", starts_col = NULL,
                                   method = "n_rand")
    index_folds <- lapply(1:folds, function(i, x) which(x$.folds == i), x = list_folds)
    names(index_folds) <- paste0("Fold", (1:folds))
    
    trCtrl_base <- trainControl(method = "cv", number = folds,  
                                returnResamp = "none", returnData = FALSE, savePredictions = TRUE, verboseIter = FALSE, 
                                allowParallel = FALSE, index = index_folds) #df_cal_i ~ Y[train]
    
    df_cal_i <- df_cal_i %>% 
      dplyr::rename(yield = Yield..t.ha.) %>% 
      dplyr::select(one_of("yield"), starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")) %>% # subset data frame that contain only the variables needed for algorithms training
      na.omit()
    
    print("Train level 0 learners")
    df_cal_i <- df_cal_i %>% 
      mutate_all(funs(as.numeric), which(sapply(., is.integer))) 
    
    rf_trained <- caret::train(yield~., data = df_cal_i, method = "rf", preProcess = c('center', 'scale'), tuneGrid=rf_gridsearch$bestTune, tuneLength = 1, trControl = trCtrl_base)
    
    cub_trained <- caret::train(yield~., data = df_cal_i, method = "cubist",preProcess = c('center', 'scale'), tuneGrid=cub_gridsearch$bestTune, tuneLength = 1, trControl = trCtrl_base)
    
    xb_trained <- caret::train(yield~., data = df_cal_i, method = "xgbTree", preProcess = c('center', 'scale'), tuneGrid=xb_gridsearch$bestTune, tuneLength = 1, trControl = trCtrl_base)
    
    svm_trained <- caret::train(yield~., data = df_cal_i, method = "svmLinear", preProcess = c('center', 'scale'), tuneGrid=svm_gridsearch$bestTune, tuneLength = 1, trControl = trCtrl_base)
    
    svmr_trained <- caret::train(yield~., data = df_cal_i, method = "svmRadial", preProcess = c('center', 'scale'), tuneGrid=svmr_gridsearch$bestTune, tuneLength = 1, trControl = trCtrl_base)
    
    nn_trained <- caret::train(yield~., data = df_cal_i, method = "mlpML", preProc =  c('center', 'scale'),tuneGrid=nn_gridsearch$bestTune, tuneLength = 1, trControl = trCtrl_base)
    
    gp_trained <- caret::train(yield~., data = df_cal_i, method = "gaussprRadial", preProcess = c('center', 'scale'), tuneGrid=gp_gridsearch$bestTune, tuneLength = 1,trControl = trCtrl_base)
    
    ea_trained <- caret::train(yield~., data = df_cal_i, method = "earth", preProcess = c('center', 'scale'), tuneGrid=ea_gridsearch$bestTune, tuneLength = 1,  trControl = trCtrl_base)
    
    knn_trained <- caret::train(yield~., data = df_cal_i, method = "knn", preProcess = c('center', 'scale'), tuneGrid=knn_gridsearch$bestTune, tuneLength = 1,  trControl = trCtrl_base)
    
    # pred
    
    df_val_i <- val_set[[i]] %>% 
      dplyr::rename(yield = Yield..t.ha.) %>% 
      dplyr::select(one_of("yield"), starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")) %>% # subset data frame that contain only the variables needed for algorithms training
      na.omit()
    
    predicted_rf <- c(predict(rf_trained, df_val_i))
    
    predicted_cub <- c(predict(cub_trained, df_val_i))
    
    predicted_xb <- c(predict(xb_trained, df_val_i))
    
    predicted_svm <- c(predict(svm_trained, df_val_i))
    
    predicted_svmr <- c(predict(svmr_trained, df_val_i))
    
    predicted_nn <- c(predict(nn_trained, df_val_i))
    
    predicted_ea <- c(predict(ea_trained, df_val_i))
    
    predicted_gp <- c(predict(gp_trained, df_val_i))
    
    predicted_knn <- c(predict(knn_trained, df_val_i))
    
    
    preds_all <- cbind(predicted_rf, predicted_cub, predicted_xb, predicted_svm, predicted_svmr, predicted_nn, 
                       predicted_ea, predicted_gp, predicted_knn) # dataframe of predictions for each algorithm
    
    #sort(sqrt(colMeans((preds - df_val_i$yield)^2))
    perf <- rbind(getAccuracyMethod("Random Forest", predicted_rf, df_val_i$yield),
                  getAccuracyMethod("Cubist", predicted_cub, df_val_i$yield),
                  getAccuracyMethod("XgBoost", predicted_xb, df_val_i$yield),
                  getAccuracyMethod("Support Vector Machine linear", predicted_svm, df_val_i$yield),
                  getAccuracyMethod("Support Vector Machine rbf", predicted_svmr, df_val_i$yield),
                  getAccuracyMethod("Neural Network", predicted_nn, df_val_i$yield),
                  getAccuracyMethod("Earth", predicted_ea, df_val_i$yield),
                  getAccuracyMethod("Gaussian Processes", predicted_gp, df_val_i$yield),
                  getAccuracyMethod("Nearest neighbours", predicted_knn, df_val_i$yield))
    
    df_performance <- data.frame(perf, iteration = i, size = j)
    
    outlist[[j]] <- df_performance
  }
  df_performance_all <- bind_rows(outlist)
  
  write.csv(df_performance_all, file = file.path(dir_out, paste0("npaddock_samplesize_performance_", crop_i, "_", i, "_", j,".csv")))
  print(paste(crop_i, "is finished"))
}
