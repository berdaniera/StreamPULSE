library(dplyr)
library(lubridate)
library(powstreams)
library(sbtools)
library(tibble)
login_sb()

#######################################3
getdtwin <- function(x, mins){
  # get even date-time windows
  # x is a vector of date times
  # mins is the minutes per window
  xx <- as.POSIXct(date(x))
  rng <- seq(min(xx), max(xx), by=mins*60)
  list(dts=as.POSIXct(cut(x,rng)), brks=rng)
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

getcv <- function(ts){
  # Average daily coefficient of variation in discharge for hydropeaking
  # see, e.g., Dibble et al. 2015 EcolAppl
  ts <- as_tibble(ts)
  cv <- ts %>% group_by(day=date(ts$DateTime)) %>%
    summarise(cv = sd(disch)/mean(disch)) %>%
    ungroup() %>% summarise(mean(cv,na.rm=T))
  as.numeric(cv)
}

getmetrics <- function(cc, window){
  data.frame(maxlag=cc$lag[which.max(cc$acf)]/(60/window), # lag with highest correlation
    maxcor=max(cc$acf),
    disch=exp(mean(log(ts$disch),na.rm=T)),
    cvdisch=getcv(ts),
    negdisch=length(which(ts$disch<0))/nrow(ts))
}

v <- c('doobs_nwis','disch_nwis','wtr_nwis','par_calcLat') # the variables
ss <- list_sites(v) # sites with the variables

output <- data.frame(maxlag=NULL,maxcor=NULL,disch=NULL,cvdisch=NULL,negdisch=NULL)

pb <- txtProgressBar(max=length(ss), initial=1, style=3)
for(s in ss){
  # get time series
  ts <- unitted::v(get_ts(v, s))
  # compare any variables
  # HERE, change xvar to "doobs" to look at correlations with DO
  cc <- getccf(tsdf=ts, xvar="wtr", yvar="par", window=15, plt=T)
  # get time lags and other measures
  # FOR LAGS: (+) is x following y, (-) is x leading y
  met <- getmetrics(cc, window=15)
  output <- rbind(output,met)
  setTxtProgressBar(pb, which(ss==s))
}
close(pb)
####################################3

#png("maxcorplt.png",width=600,height=600)
# PLOT 1
plot(output$maxlag,output$maxcor,
  main=paste("Lag with maximum correlation, n =",nrow(output)),
  xlab="Lag (hours), temperature following light",
  ylab="Maximum correlation / Density",
  bty="n", las=1, cex=log(output$disch)/5)
abline(h=0,col="grey")
abline(v=0,col="grey")
hst <- hist(output$maxlag,plot=F,breaks="fd")
lines(hst$density~hst$mids,type="s")
#dev.off()

# PLOT 2
plot(output$maxcor,output$cvdisch,
  xlab="Maximum correlation bw temperature and light",
  ylab="Average daily CV in discharge, mean( sd(x_t)/mean(x_t) )",
  main="Discharge CV ~ hydropeaking with high CV",
  ylim=c(0,0.5), bty="n", cex=log(output$disch)/5)
