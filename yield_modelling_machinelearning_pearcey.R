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

for (crop_i in crop_list){ # crop_i = calibration data frames per crop
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
  
  ### CROSS VALIDATION i --------------
  # cal_set = one calibration data frame (among the 100 generated) length(cal_set)
  df_cal_i <- cal_set[[i]] %>% 
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
  
  df_cal_i <- cal_set[[i]] %>% 
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
  
  # ENSEMBLING ------
  svm_trained$pred$pred <- as.numeric(svm_trained$pred$pred)
  svmr_trained$pred$pred <- as.numeric(svmr_trained$pred$pred)
  print("Train level 1 learners")
  all.models <- list(rf_trained,  cub_trained , xb_trained, svm_trained, svmr_trained, 
                     nn_trained, gp_trained, ea_trained, knn_trained)
  names(all.models) <- sapply(all.models, function(x) x$method)
  sort(sapply(all.models, function(x) min(x$results$RMSE)))
  class(all.models) <- "caretList"
  
  trCtrl_ens <- trainControl(method = "cv", number = folds) #  , index = index_folds)
  
  # Make a linear regression ensemble
  linear <- caretStack(all.models, method = "glm", trControl = trCtrl_ens)
  summary(linear$ens_model$finalModel)
  linear_lasso <- caretStack(all.models, method = "lasso", trControl = trCtrl_ens)
  
  
  # Make a non-linear regression ensemble
  nlinear <- caretStack(all.models, method = "xgbTree", trControl =  trCtrl_ens)
  summary(nlinear$ens_model$finalModel)
  
  df_val_i <- val_set[[i]] %>% 
    dplyr::rename(yield = Yield..t.ha.) %>% 
    dplyr::select(one_of("yield"), starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")) %>% # subset data frame that contain only the variables needed for algorithms training
    na.omit()
 
  # Ensembling based on SD
  # SD calculation
  print("Get SD")
  sd_rf <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                           method = "rf", grid = rf_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)
  
  sd_cub <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                            method = "cubist", grid = cub_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)
  
  sd_xb <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                           method = "xgbTree", grid = xb_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)
  
  sd_svm <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                            method = "svmLinear", grid = svm_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)
  
  sd_svmr <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                             method = "svmRadial", grid = svmr_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)
  
  sd_nn <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                           method = "mlpML", grid = nn_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)
  
  sd_gp <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                           method = "gaussprRadial", grid = gp_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)
  
  sd_ea <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                           method = "earth", grid = ea_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)
  
  sd_knn <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                            method = "knn", grid = knn_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)
  
  
  # Define weights for BFE
  df_var <- cbind(rf = 1/(sd_rf^2), cub = 1/(sd_cub^2), xb = 1/(sd_xb^2),
                  svm = 1/(sd_svm^2), svmr = 1/(sd_svmr^2), nn = 1/(sd_nn^2),
                  ea = 1/(sd_ea^2), gp = 1/(sd_gp^2),  knn = 1/(sd_knn^2))
  sum_var <- apply(df_var, 1, function(x) sum(x))
  BFE <- as.matrix(df_var/sum_var)
  
  # weighted sum with Rsquared
  w <- c(rf_trained$results$Rsquared, cub_trained$results$Rsquared, xb_trained$results$Rsquared,
         svm_trained$results$Rsquared, svmr_trained$results$Rsquared, nn_trained$results$Rsquared, 
         ea_trained$results$Rsquared, gp_trained$results$Rsquared, knn_trained$results$Rsquared)
  w_scaled <- w/sum(w)
  
  
  # Define weights for BFE 3
  df_var3 <- cbind(rf = 1/(sd_rf^2), svmr = 1/(sd_svmr^2), gp = 1/(sd_gp^2))
  sum_var3 <- apply(df_var3, 1, function(x) sum(x))
  BFE3 <- as.matrix(df_var3/sum_var3)
  
  # pred
  predicted_rf <- c(predict(rf_trained, df_val_i))
  
  predicted_cub <- c(predict(cub_trained, df_val_i))
  
  predicted_xb <- c(predict(xb_trained, df_val_i))
  
  predicted_svm <- c(predict(svm_trained, df_val_i))
  
  predicted_svmr <- c(predict(svmr_trained, df_val_i))
  
  predicted_nn <- c(predict(nn_trained, df_val_i))
  
  predicted_ea <- c(predict(ea_trained, df_val_i))
  
  predicted_gp <- c(predict(gp_trained, df_val_i))
  
  predicted_knn <- c(predict(knn_trained, df_val_i))
  
  preds_ENS_linear <- c(predict(linear, df_val_i))
  
  preds_ENS_lasso <- c(predict(linear_lasso, df_val_i))
  
  preds_ENS_nlinear <- c(predict(nlinear, df_val_i))
  
  preds_ENS_mean <- apply(sapply(all.models, function (m,x) predict(m, x), x = df_val_i), 1, mean)
  
  preds_all <- cbind(predicted_rf, predicted_cub, predicted_xb, predicted_svm, predicted_svmr, predicted_nn, 
                     predicted_ea, predicted_gp, predicted_knn) # dataframe of predictions for each algorithm
  preds_weighted <- as.data.frame(BFE*preds_all) # dataframe of weighted predictions for each algorithm
  preds_ENS_weight <- apply(preds_weighted, 1,  function(x) sum(x)) # vector of weight ensemble prediction (each row = different algorithms)
  preds_weighted_r2 <- as.data.frame(w_scaled*preds_all) # dataframe of weighted predictions for each algorithm
  preds_ENS_weightr2 <- apply(preds_weighted_r2, 1,  function(x) sum(x)) # vector of weight ensemble prediction (each row = different algorithms)
  
  preds_3 <- cbind(predicted_rf, predicted_svmr, predicted_gp) # dataframe of predictions for each algorithm
  preds_weighted3 <- as.data.frame(BFE3*preds_3) # dataframe of weighted predictions for each algorithm
  preds_ENS_weight3 <- apply(preds_weighted3, 1,  function(x) sum(x)) # vector of weight ensemble prediction (each row = different algorithms)
  
  
  df_cor <- cor(preds_all)
  
  
  #sort(sqrt(colMeans((preds - df_val_i$yield)^2))
  perf <- rbind(getAccuracyMethod("Random Forest", predicted_rf, df_val_i$yield),
                getAccuracyMethod("Cubist", predicted_cub, df_val_i$yield),
                getAccuracyMethod("XgBoost", predicted_xb, df_val_i$yield),
                getAccuracyMethod("Support Vector Machine linear", predicted_svm, df_val_i$yield),
                getAccuracyMethod("Support Vector Machine rbf", predicted_svmr, df_val_i$yield),
                getAccuracyMethod("Neural Network", predicted_nn, df_val_i$yield),
                getAccuracyMethod("Earth", predicted_ea, df_val_i$yield),
                getAccuracyMethod("Gaussian Processes", predicted_gp, df_val_i$yield),
                getAccuracyMethod("Nearest neighbours", predicted_knn, df_val_i$yield),
                getAccuracyMethod("Ensemble linear", preds_ENS_linear, df_val_i$yield),
                getAccuracyMethod("Ensemble lasso", preds_ENS_lasso, df_val_i$yield),
                getAccuracyMethod("Ensemble nlinear", preds_ENS_nlinear, df_val_i$yield),
                getAccuracyMethod("Ensemble mean", preds_ENS_mean, df_val_i$yield),
                getAccuracyMethod("Ensemble weight sd", preds_ENS_weight, df_val_i$yield),
                getAccuracyMethod("Ensemble weight r2", preds_ENS_weightr2, df_val_i$yield), 
                getAccuracyMethod("Ensemble weight sd reduced", preds_ENS_weight3, df_val_i$yield))
  
  df_performance <- data.frame(perf, iteration = i)
  write.csv(df_performance, file = file.path(dir_out, paste0("performance_", crop_i, "_", i,".csv")))
  write.csv(df_cor, file = file.path(dir_out, paste0("cor_", crop_i, "_", i,".csv")))
  print(paste(crop_i, "is finished"))
}
