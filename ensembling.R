
getAccuracyMethod <- function(method, predicted, observed) {
  library(epiR)
  rmse_val <- rmse(observed,predicted)
  mape_val <- mape(observed,predicted)
  lm_mdl  <- lm(y~x, data.frame(x = observed, y = predicted))
  rsq_val <- summary(lm_mdl)$r.square
  ccc <- as.numeric(epi.ccc(observed, predicted)$rho.c[1])
  return(data.frame(Algorithm = method, Accuracy = c("RMSE", "MAPE", "Rsquare", "CCC"), Value = c(rmse_val, mape_val, rsq_val, ccc)))
}

# getWeightMethod <- function(algo, linear, nlinear) {
#   weight_lin <- linear$ens_model$finalModel$coefficients[algo]
#   weight_nlin <- nlinear$ens_model$finalModel$coefficients[algo]
#   return(data.frame(Algorithm = algo, Accuracy = c("Weight Lin", "Weight nLin"), Value = c(weight_lin, weight_nlin)))
# }
  
getSDPrediction <- function(Xtr, Ytr, Xts, method, grid, pp,  k = 25){
  list_out <- list()
  for(i in 1:k) {
    # Generate a bootstrap resample of x rows
    set.seed(i)
    cal_rows <- sample(nrow(Xtr), round((1*nrow(Xtr))), replace = TRUE)
    
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

set.seed(40)
if ((Sys.info()["nodename"]) == "sc-05-cdc") {
  dir_data <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data"
  dir_plot <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/figures"
  dir_mdl <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data/models"
  no_cores <- 10
} else if ((Sys.info()["nodename"]) == "MULLET-SL") {
  dir_data <- "X:/projects/yield_machinelearning/data"
  dir_plot <- "X:/projects/yield_machinelearning/figures"
  dir_mdl <- "X:/projects/yield_machinelearning/data/models"
  no_cores <- 2
}
install.packages("dplyr")
install.packages("purrr")
install.packages("xgboost")
install.packages("RcppRoll")
install.packages("tidyr")


library(magrittr)
library(lattice)
library(sp)
library(raster)
library(Cubist)
library(randomForest)
library(xgboost)
library(e1071)
library(RSNNS)
library(kernlab)
library(caret)
library(dplyr)
library(rsq)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)
library(tools)
library(groupdata2)
library(dplyr)
library(purrr)
library(tidyr)
library(caretEnsemble)
library(devtools)
library(mlbench)
library(earth)

source("metrics2.R")



cl <- makeCluster(no_cores)
registerDoParallel(cl)

myFiles <- list()
myFiles[["calibration"]] <- list.files(path=file.path(dir_data, "Calibration"), pattern="calibration",full.names=TRUE) # gather all the calibration data
myFiles[["validation"]] <- list.files(path=file.path(dir_data, "Validation"), pattern="validation",full.names=TRUE) # gather all the validation data
crop_list <- sapply(myFiles$calibration, function(x) strsplit(basename(file_path_sans_ext(x)), "_")[[1]][2])
df_perf <- data.frame()

for (crop_i in crop_list){ # crop_i = calibration data frames per crop
  
  print(crop_i)

  mydata <- read.csv(paste0(crop_i, ".csv")) %>% 
    unite("paddock_year", c("PaddID","Year"), sep = "_", remove = FALSE)
 
  folds_h <- 10
  list_folds <- groupdata2::fold(mydata, k = folds_h, id_col = "paddock_year", starts_col = NULL,
                                 method = "n_rand")
  index_folds <- lapply(1:folds_h, function(i, x) which(x$.folds == i), x = list_folds)
  names(index_folds) <- paste0("Fold", (1:folds_h))
  
  mydata <- mydata %>%     
    dplyr::rename(yield = Yield..t.ha.) %>% 
    dplyr::select(one_of("yield"), starts_with("NDVI"),starts_with("CUMRAIN"),starts_with("TMAX")) %>% # subset data frame that contain only the variables needed for algorithms training
    na.omit()
  
  ### Setting parameters grids ----
  # randomForest
  gridRF <- expand.grid(mtry=c(10,20,40,60))
  # xgboost
  gridXB = expand.grid(nrounds = 500, eta = c(0.005, 0.01, 0.05),max_depth = c(3: 8), gamma = 0, colsample_bytree = 0.8, min_child_weight = 1, subsample = 1)
  # cubist
  gridCUB <- expand.grid(committees = c(3:6), neighbors = c(2:8))
  # svm
  gridSVM<- expand.grid( C = c(1:10))
  gridSVMr <- expand.grid(C = c(1:5), sigma = seq(0.001, 0.05, by = 0.005))
  # neural network
  gridNN <- expand.grid(layer1 = seq(6, 12, 2), layer2 = seq(8, 14, 2), layer3 = seq(2, 8, 2))
  # gaussian
  gridGP <- expand.grid(sigma = seq(0.0001,0.03,by=0.0025))
  # earth
  gridEA <- expand.grid(degree = seq(1, 10, 2), nprune = seq(2, 12, 2))
  # knn
  gridkNN <- expand.grid(k = seq(1, 30, 3))
  
  
  ### Hyperparameters optimization ------
  pp_hyper <- c('center', 'scale')
  trCtrl_hyper <- trainControl(method = "cv", number = folds_h,  allowParallel = TRUE, index = index_folds) #df_cal_i ~ Y[train]
  
  rf_gridsearch <- caret::train(yield~., data = mydata, method="rf", preProcess = pp_hyper, tuneGrid=gridRF, trControl = trCtrl_hyper)
  save(rf_gridsearch, file = file.path(dir_mdl, paste0("rf_optimised_",crop_i, ".RData" )))
  
  cub_gridsearch <- caret::train(yield~., data = mydata, method = "cubist",preProcess = pp_hyper, tuneGrid = gridCUB, trControl = trCtrl_hyper)
  save(cub_gridsearch, file = file.path(dir_mdl, paste0("cub_optimised_",crop_i, ".RData" )))
  
  xb_gridsearch <- caret::train(yield~., data = mydata, method = "xgbTree", preProcess = pp_hyper, tuneGrid = gridXB, trControl = trCtrl_hyper)
  save(xb_gridsearch, file = file.path(dir_mdl, paste0("xb_optimised_",crop_i, ".RData" )))
  
  svm_gridsearch <- caret::train(yield~., data = mydata, method = "svmLinear", preProcess = pp_hyper, tuneGrid = gridSVM, trControl = trCtrl_hyper)
  save(svm_gridsearch, file = file.path(dir_mdl, paste0("svm_optimised_",crop_i, ".RData" )))
  
  svmr_gridsearch <- caret::train(yield~., data = mydata, method = "svmRadial", preProcess = pp_hyper, tuneGrid = gridSVMr, trControl = trCtrl_hyper)
  save(svmr_gridsearch, file = file.path(dir_mdl, paste0("svmr_optimised_",crop_i, ".RData" )))
  
  nn_gridsearch <- caret::train(yield~., data = mydata, method = "mlpML", preProc =  pp_hyper,tuneGrid = gridNN, trControl = trCtrl_hyper)
  save(nn_gridsearch, file = file.path(dir_mdl, paste0("nn_optimised_",crop_i, ".RData" )))
  
  gp_gridsearch <- caret::train(yield~., data = mydata, method = "gaussprRadial", pp_hyper, tuneGrid = gridGP, trControl = trCtrl_hyper)
  save(gp_gridsearch, file = file.path(dir_mdl, paste0("gp_optimised_",crop_i, ".RData" )))
  
  ea_gridsearch <- caret::train(yield~., data = mydata, method = "earth", preProcess = pp_hyper, tuneGrid = gridEA, trControl = trCtrl_hyper)
  save(ea_gridsearch, file = file.path(dir_mdl, paste0("ea_optimised_",crop_i, ".RData" )))
  
  knn_gridsearch <- caret::train(yield~., data = mydata, method = "knn", preProcess = pp_hyper, tuneGrid = gridkNN, trControl = trCtrl_hyper)
  save(knn_gridsearch, file = file.path(dir_mdl, paste0("knn_optimised_",crop_i, ".RData" )))
    
  load(grep(crop_i, myFiles$calibration, value = T))
  load(grep(crop_i, myFiles$validation, value = T))

  ### CROSS VALIDATION --------------
  df_performance <- foreach (i = 1:3, .combine = rbind,
                             .packages=c( "randomForest", "Cubist", "lattice", "xgboost","kernlab",
                                          "e1071", "RSNNS", "caret","dplyr","groupdata2", "earth", "elasticnet", "caretEnsemble", "class", "glm2", "glmnet"))  %dopar%  { # cal_set = one calibration data frame (among the 100 generated) length(cal_set)
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
  
  rf_trained <- caret::train(yield~., data = df_cal_i, method="rf", preProcess = c('center', 'scale'), tuneGrid=rf_gridsearch$bestTune, tuneLength = 1, trControl = trCtrl_base)
  
  cub_trained <- caret::train(yield~., data = df_cal_i, method = "cubist",preProcess = c('center', 'scale'), tuneGrid=cub_gridsearch$bestTune, tuneLength = 1, trControl = trCtrl_base)
  
  xb_trained <- caret::train(yield~., data = df_cal_i, method = "xgbTree", preProcess = c('center', 'scale'), tuneGrid=xb_gridsearch$bestTune, tuneLength = 1, trControl = trCtrl_base)
  
  svm_trained <- caret::train(yield~., data = df_cal_i, method = "svmLinear", preProcess = c('center', 'scale'), tuneGrid=svm_gridsearch$bestTune, tuneLength = 1, trControl = trCtrl_base)
  
  svmr_trained <- caret::train(yield~., data = df_cal_i, method = "svmRadial", preProcess = c('center', 'scale'), tuneGrid=svmr_gridsearch$bestTune, tuneLength = 1, trControl = trCtrl_base)
  
  nn_trained <- caret::train(yield~., data = df_cal_i, method = "mlpML", preProc =  c('center', 'scale'),tuneGrid=nn_gridsearch$bestTune, tuneLength = 1, trControl = trCtrl_base)
  
  gp_trained <- caret::train(yield~., data = df_cal_i, method = "gaussprRadial", preProcess = c('center', 'scale'), tuneGrid=gp_gridsearch$bestTune, tuneLength = 1,trControl = trCtrl_base)
  
  ea_trained <- caret::train(yield~., data = df_cal_i, method = "earth", preProcess = c('center', 'scale'), tuneGrid=ea_gridsearch$bestTune, tuneLength = 1,  trControl = trCtrl_base)
  
  knn_trained <- caret::train(yield~., data = df_cal_i, method = "knn", preProcess = c('center', 'scale'), tuneGrid=knn_gridsearch$bestTune, tuneLength = 1,  trControl = trCtrl_base)
  
  
  all.models <- list(rf_trained,  cub_trained, xb_trained, svm_trained, svmr_trained, 
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
  df_cal_i <- cal_set[[i]] %>% 
   na.omit()
  
  # Ensembling based on SD
    # SD calculation
  
  sd_rf <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                             method = "rf", grid = rf_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)

  sd_cub <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                             method = "cubist", grid = cub_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)
  
  sd_xb <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                             method = "xgbTree", grid = xb_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)
  
  sd_svm <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                             method = "svmLinear", grid = svm_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)
  
  sd_svmr <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                             method = "svmRadial", grid = svm_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)
  
  sd_nn <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                             method = "mlpML", grid = svmr_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)
  
  sd_gp <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                             method = "gaussprRadial", grid = gp_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)
  
  sd_ea <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                             method = "earth", grid = ea_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)
  
  sd_knn <- getSDPrediction(select(df_cal_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), df_cal_i$yield, select(df_val_i,starts_with("NDVI"), starts_with("CUMRAIN"), starts_with("TMAX")), 
                             method = "knn", grid = knn_gridsearch$bestTune, pp = c('center', 'scale'),  k = 25)
  
  # Linear model implementing
  
  DF <- cbind(rf = 1/(sd_rf^2), xb = 1/(sd_xb^2))
  
  SOMME <- apply(DF, 1, function(x) sum(x))
  
  BFE <- as.matrix(DF/WDF)
  
  # preds
  
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
  preds_all <- as.matrix(cbind(predicted_rf, predicted_xb))
  preds_weighted <- as.data.frame(BFE*preds_all) # dataframe of weighted predictions for each algorithm
  preds_ENS_weight <- apply(preds_weighted, 1,  function(x) sum(x)) # vector of weight ensemble prediction (each row = different algorithms)


  #sort(sqrt(colMeans((preds - df_val_i$yield)^2))
  perf <- rbind(getAccuracyMethod("Random Forest", predicted_rf, df_val_i$yield),
                getAccuracyMethod("Cubist", predicted_cub, df_val_i$yield),
                getAccuracyMethod("XgBoost", predicted_xb, df_val_i$yield),
                getAccuracyMethod("Support Vector Machine linear", predicted_svm, df_val_i$yield),
                getAccuracyMethod("Support Vector Machine rbf", predicted_svmr, df_val_i$yield),
                getAccuracyMethod("Neural Network", predicted_nn, df_val_i$yield),
                getAccuracyMethod("Earth", predicted_ea, df_val_i$yield),
                getAccuracyMethod("Gaussian", predicted_gp, df_val_i$yield),
                getAccuracyMethod("Nearest neighbours", predicted_knn, df_val_i$yield),
                getAccuracyMethod("Average", preds_ENS_mean, df_val_i$yield),
                getAccuracyMethod("Ensemble linear", preds_ENS_linear, df_val_i$yield),
                getAccuracyMethod("Ensemble lasso", preds_ENS_lasso, df_val_i$yield),
                getAccuracyMethod("Ensemble nlinear", preds_ENS_nlinear, df_val_i$yield),
                getAccuracyMethod("Ensemble mean", preds_ENS_mean, df_val_i$yield),
                getAccuracyMethod("Ensemble weight", preds_ENS_weight, df_val_i$yield))
  
  data.frame(perf, iteration = i)
}

save(df_performance, file = file.path(dir_data, "Performance", paste0("performance_", crop_i, ".RData")))
}
stopCluster(cl)
#ggplot(perf, aes(x = Algorithm, y = Value, fill = Algorithm))+geom_col()+facet_wrap(~Accuracy, scales =  "free_y")