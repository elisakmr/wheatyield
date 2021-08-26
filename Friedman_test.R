
if ((Sys.info()["nodename"]) == "sc-05-cdc") {
  dir_data <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data"
  dir_plot <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/figures"
} else if ((Sys.info()["nodename"]) == "MULLET-SL") {
  dir_data <- "X:/projects/yield_machinelearning/data"
  dir_plot <- "X:/projects/yield_machinelearning/figures"
}

if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(lattice)){install.packages("lattice")}
if(!require(BSDA)){install.packages("BSDA")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(PMCMR)){install.packages("PMCMR")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(scmamp)){install.packages("scmamp")}

library(plyr)
library(tools)
library(dplyr)
library(readr)
library(tidyverse)
library(data.table)
library(scmamp)
library(ggplot2)

### FRIEDMAN'S TEST assessing differences in algorithms performance

crop_list <- c("barley", "canola", "wheat")
df_stat <- data.frame()
for (crop in crop_list){
  df <- read_csv(file.path(dir_data, "Performance", paste0("sum_performance_", crop, ".csv")))
  df_rmse <- df %>%
    filter(Accuracy == "RMSE") %>%
    group_by(Algorithm, Accuracy) %>%
    spread (Algorithm, Value) %>% 
    select(-X1)
df_rmse <- unique(setDT(df_rmse)[, lapply(.SD, na.omit), by = iteration])
  f <- friedman.test(as.matrix(df_rmse[,3:16]))
  if (f$p.value < 1e-5) {
    out <- data.frame("< 1e-5", crop)
  } else {
    out <- c(unlist(f$p.value), crop)
    out <- t(data.frame (out))
  }
  colnames(out) <- c("p value","Crop")
  df_stat <- rbind(df_stat,out)
  colnames(df_stat) <- c("p value","Crop")
  
}

df_friedman <- write.csv(df_stat, "Friedman_test.csv", row.names=F, col.names=T)






