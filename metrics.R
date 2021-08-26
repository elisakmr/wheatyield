### INPUT METRICS

## Interpolate missing values: 
intLinear <- function(x, date) calc(x, fun = function(y){
  n <- length(y)
  y[y < 0] <- NA
  
  inna <- which(!is.na(y))
  
  if (length(inna) == 0){
    out<-rep(0,n)
  } else if (length(inna) == n) {
    out <- y
  } else {
    # if first and last values are missing, copy them from first and observed values
    if(is.na(y[1])){y[1]<-y[inna[1]]}
    if(is.na(y[n])){y[n]<-y[tail(inna,1)]}
    inna <- which(!is.na(y))
    x.init <- date[inna]
    # remove all the NA for building the model
    interp <- approx(x.init,y[inna],date)

## Smooth data and derive metrics
    extractMetrics <- function(x, t_in) {
      integrale <- function(x, start, end){
        maxi <- which(x == max(x))
        if (start=="start" & end=="max") {
          out <-  sum(x[1:maxi])
        } else if  (start=="max"& end=="end") {
          out <- sum(x[maxi:length(x)])
        } else if  (start=="start"& end=="end") {
          out <- sum(x)
        } 
        out
      }
      
      #Days serie
      t_new <- 1:max(t_in)
      interp <- approx(t_in, x, t_new)
      NDVI <- interp$y
      #Parameters estimation - fit per row and not on the entire dataframe
      p <-phenopix::FitDoubleLogBeck(NDVI, probs=c(0.01, 0.99))
      out1 <- as.numeric(p$predicted)
      mini <- min(out1)
      border <- mini*1.1
      out2 <- out1-border
      out2[out2 <=0 ] <- 0
      
      metrics <- c(NDVIm_min=min(out1, na.rm = TRUE),
                NDVIm_max = max(out1, na.rm = TRUE), 
                NDVIm_amp = max(out2, na.rm = TRUE), 
                NDVIm_day_slope_max=p$params[3], 
                NDVIm_slope_max=p$params[4], 
                NDVIm_day_slope_min=p$params[5], 
                NDVIm_slope_min=p$params[6], 
                NDVIm_gs = sum(out2!=0), 
                NDVIm_mean = mean(out2[out2 != 0]), 
                NDVIm_intpre=integrale(out2, start = "start", end = "max"),
                NDVIm_intpost=integrale(out2, start = "max", end = "end"),
                NDVIm_int=integrale(out2, start = "start", end = "end"))
      ndvi_out <- round(out1[t_in])
      names(ndvi_out) <- paste0("NDVI_", t_in)
      out <- c(ndvi_out, metrics)
     
    }
    out <- interp$y
  } 
  return(out)
})
 
## Integrale function


### PERFORMANCE METRICS

rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2))
}

rmspe <- function(actual, predicted){
  sqrt(mean(100*abs(((actual-predicted)/actual)^2)))
}

mape <- function(actual, predicted){
  mean(abs((actual-predicted)/actual) * 100)
}

    
### Performance analysis
load(file.path(dir_data,"Performance","performance_ensemble_barley.RData"))

