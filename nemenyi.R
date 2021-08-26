### CUSTOMIZING NEMENYI PLOT
library(Matrix)
library(scmamp)
myPlotCD <- function (results.matrix, alpha = 0.05, cex = 0.75, ...) 
{
  k <- dim(results.matrix)[2]
  N <- dim(results.matrix)[1]
  cd <- getNemenyiCD(alpha = alpha, num.alg = k, num.problems = N)
  mean.rank <- sort(colMeans(rankMatrix(results.matrix, ...)))
  lp <- round(k/2)
  left.algs <- mean.rank[1:lp]
  right.algs <- mean.rank[(lp + 1):k]
  max.rows <- ceiling(k/2)
  char.size <- 0.001
  line.spacing <- 0.25
  m <- floor(min(mean.rank))
  M <- ceiling(max(mean.rank))
  max.char <- max(sapply(colnames(results.matrix), FUN = nchar))
  text.width <- (max.char + 4) * char.size
  w <- (M - m) + 2 * text.width
  h.up <- 2.5 * line.spacing
  h.down <- (max.rows + 2.25) * line.spacing
  tick.h <- 0.25 * line.spacing
  label.displacement <- 0.1
  line.displacement <- 0.025
  plot(0, 0, type = "n", xlim = c(m - w/(M - m), M + w/(M - 
                                                          m)), ylim = c(-h.down, h.up), xaxt = "n", yaxt = "n", 
       xlab = "", ylab = "", bty = "n")
  lines(c(m, M), c(0, 0))
  dk <- sapply(m:M, FUN = function(x) {
    lines(c(x, x), c(0, tick.h))
    text(x, 3 * tick.h, labels = x, cex = cex)
  })
  lines(c(m+0.1, m + cd-0.1), c(1.75 * line.spacing, 1.75 * line.spacing), col = "red", lwd = 2.5)
  text(m + cd/2, 2.25 * line.spacing, "Critical Distance", cex = cex, col = "red")
  text(0.3, 0.3, "Wheat", cex = cex, col = "black")
  #lines(c(m, m), c(1.75 * line.spacing - tick.h/4, 1.75 * 
                     #line.spacing + tick.h/4), col = "red")
  #lines(c(m + cd, m + cd), c(1.75 * line.spacing - tick.h/4, 
                             #1.75 * line.spacing + tick.h/4), col = "red")
  dk <- sapply(1:length(left.algs), FUN = function(x) {
    line.h <- -line.spacing * (x + 2)
    text(x = m - label.displacement, y = line.h, labels = names(left.algs)[x], 
         cex = cex, adj = 1)
    lines(c(m, left.algs[x]), c(line.h, line.h))
    lines(c(left.algs[x], left.algs[x]), c(line.h, 0))
  })
  dk <- sapply(1:length(right.algs), FUN = function(x) {
    line.h <- -line.spacing * (x + 2)
    text(x = M + label.displacement, y = line.h, labels = names(right.algs)[x], 
         cex = cex, adj = 0)
    lines(c(M, right.algs[x]), c(line.h, line.h))
    lines(c(right.algs[x], right.algs[x]), c(line.h, 0))
  })
  getInterval <- function(x) {
    from <- mean.rank[x]
    diff <- mean.rank - from
    ls <- which(diff > 0 & diff < cd)
    if (length(ls) > 0) {
      c(from, mean.rank[max(ls)])
    }
  }
  intervals <- mapply(1:k, FUN = getInterval)
  aux <- do.call(rbind, intervals)
  to.join <- aux[1, ]
  if (nrow(aux) > 1) {
    for (r in 2:nrow(aux)) {
      if (aux[r - 1, 2] < aux[r, 2]) {
        to.join <- rbind(to.join, aux[r, ])
      }
    }
  }
  row <- c(1)
  if (!is.matrix(to.join)) {
    to.join <- t(as.matrix(to.join))
  }
  nlines <- dim(to.join)[1]
  for (r in 1:nlines) {
    id <- which(to.join[r, 1] > to.join[, 2])
    if (length(id) == 0) {
      row <- c(row, tail(row, 1) + 1)
    }
    else {
      row <- c(row, min(row[id]))
    }
  }
  step <- max(row)/2
  dk <- sapply(1:nlines, FUN = function(x) {
    y <- -line.spacing * (0.5 + row[x]/step)
    lines(c(to.join[x, 1] - line.displacement, to.join[x, 
                                                       2] + line.displacement), c(y, y), lwd = 3, col = "red")
  })
}
getNemenyiCD <- function (alpha = 0.05, num.alg, num.problems) {
  # Auxiliar function to compute the critical difference for Nemenyi test
  # Args:
  #   alpha:        Alpha for the test
  #   num.alg:      Number of algorithms tested
  #   num.problems: Number of problems where the algorithms have been tested
  #
  # Returns:
  #   Corresponding critical difference
  #
   df <- num.alg * (num.problems - 1)
   qa <- qtukey(p=1 - alpha, nmeans=num.alg, df=df)/sqrt(2)
   cd <- qa * sqrt((num.alg * (num.alg + 1)) / (6 * num.problems))
   return(cd)
}


### cODE --------------------------------------------------------------------------




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

### FRIEDMAN'S nemenyi TEST carrying pairwise performance differences between algorithm 

### create data
library(tidyverse)
crop_list <- c("barley", "canola", "wheat")
for (crop in crop_list){
  df <- read_csv(file.path(dir_data, "Performance", paste0("sum_performance_", crop, ".csv")))
  df_rmspe <- df %>% 
    select(-X1) %>%
    filter(Accuracy == "RMSPE") %>%
    group_by(Algorithm, Accuracy, iteration) %>% 
    spread (key = Algorithm, value = Value) 
  write.csv(df_rmspe, file.path(dir_data, "data_figures", paste0("df_nemenyi_", crop, ".csv")))
}

### create nemenyi figure
library(readr)
library(dplyr)
library(data.table)
library(scmamp)
crop_list <- c("barley", "canola", "wheat")
for (crop in crop_list){
  df_rmspe <- read_csv(file.path(dir_data, "data_figures", paste0("df_nemenyi_", crop, ".csv"))) 
  df_rmspe <-  df_rmspe %>% 
    select(-X1)
  df_rmspe <- unique(setDT(df_rmspe)[, lapply(.SD, na.omit), by = iteration])
  df_rmspe <- df_rmspe %>% 
    rename("RF"="Random Forest", "GP"="Gaussian Processes", "Ens Lasso"="Ensemble lasso", 
           "Ens xgBoost"="Ensemble nlinear", "kNN"="Nearest neighbours", 
           "SVMl"="Support Vector Machine linear", "ens.lin"="Ensemble linear", 
           "ens.BDF"="Ensemble weight sd", "Weight3 ens"="Ensemble weight r2", 
           "Weight2 ens"= "Ensemble weight sd reduced", 
           "SVMr"="Support Vector Machine rbf", "MLP"= "Neural Network", "XB"= "XgBoost", 
           "ens.mean"="Ensemble mean", "MARS" = "Earth", "CUB"="Cubist") %>% 
    dplyr::select(-one_of("Weight2 ens", "Weight3 ens", "Ens Lasso", "Ens xgBoost", "ens.lin"))
  
  pdf(file.path(dir_plot, paste0("nemenyif_", crop,".pdf")),width=7,height=5)
  myPlotCD((1-as.matrix(df_rmspe[,3:ncol(df_rmspe)])), alpha=0.05, cex=0.8)#+
  #  title(main = "Wheat", cex.main=0.8)
  dev.off()
}
