library(dplyr)
library(lubridate)
library(powstreams)
library(sbtools)
library(tibble)
login_sb('abb30@duke.edu')

#######################################3
getdtwin <- function(x, mins){
  # get even date-time windows
  # x is a vector of date times
  # mins is the minutes per window
  xx <- as.POSIXct(date(x))
  rng <- seq(min(xx), max(xx), by=mins*60)
  list(dts <- as.POSIXct(cut(x,rng)), brks=rng)
}

getccf <- function(tsdf, xvar, yvar, window, plt=TRUE){
  # Check for differences in time windows - need to re-window for cross correlation
  # NOTE: Slow! Also, it might be overkill...
  if( (length(which(diff(tsdf$DateTime)==window))/nrow(tsdf) < 0.9) ){ # 90% of time windows are 15 minutes
  #if(all( diff(ts$DateTime) != 0 )){ # all time windows are 15 minutes - more restrictive
    windows <- getdtwin(tsdf$DateTime, window)
    tsdf$DateTime <- windows$dts
    # select data columns, average by time windows (in case some duplicates)
    tsdf <- tsdf %>% group_by(DateTime) %>% summarise_each(funs(mean))
    # merge with date time windows
    tsdf <- left_join(data.frame(DateTime=windows$brks), tsdf, by="DateTime")
  }
  xv <- tsdf %>% select_(.dots=xvar)
  yv <- tsdf %>% select_(.dots=yvar)
  ccf(xv, yv, na.action=na.pass, plot=plt, lag.max=12*60/window, type="correlation") # maximum lag is 12 hours either direction
}

getmaxlag <- function(cc, window){
  data.frame(maxlag=cc$lag[which.max(cc$acf)]/(60/window), # lag with highest correlation
    maxcor=max(cc$acf))
}

v <- c('doobs_nwis','disch_nwis','wtr_nwis','par_calcLat') # the variables
ss <- list_sites(v) # sites with the variables

maxlags <- data.frame(maxlag=NULL,maxcor=NULL)

pb <- txtProgressBar(max=length(ss), initial=1, style=3)
for(s in ss){
  # get time series
  ts <- unitted::v(get_ts(v, s))
  # compare any variables
  cc <- getccf(tsdf=ts, xvar="wtr", yvar="par", window=15, plt=T)
  # get time lags
  # + is x following y
  # - is x leading y
  lag <- getmaxlag(cc, window=15)
  maxlags <- rbind(maxlags,lag)
  setTxtProgressBar(pb, which(ss==s))
}
close(pb)
####################################3
