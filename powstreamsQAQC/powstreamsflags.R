library(dplyr)
library(lubridate)
library(powstreams)
library(sbtools)
library(tibble)
login_sb('abb30@duke.edu')

#######################################3
# Snap time stamps to even interval
snap = function(x, freq, nearest=FALSE){
  # x is a date time object to be snapped
  # freq is the frequency of observations
  # nearest is logical to snap to floor (default) or to nearest time cut
  re = regexec("([0-9]+)([A-Z])",freq)[[1]]
  if(-1%in%re){
    stop("Please enter a correct string")
  }else{
    ml = attr(re,"match.length")
    nn = as.numeric(substr(freq, re[2], ml[2]))
    uu = substr(freq, re[3], ml[1])
    if(uu=="D"){td = 24*60*60*nn
    }else if(uu=="H"){td = 60*60*nn
    }else if(uu=="M"){td = 60*nn
    }else if(uu=="S"){td = nn
    }else{stop("Please enter a correct string")}
  }
  if(nearest){ # round to closest interval
    as.POSIXct(round(as.double(x)/td)*td,origin="1970-01-01")
  }else{ # round to floor
    as.POSIXct(floor(as.double(x)/td)*td,origin="1970-01-01")
  }
}

getccf <- function(tsdf, xvar, yvar, window, plt=TRUE){
  # snap times to 15M intervals
  windows <- snap(tsdf$DateTime,"15M")
  if(any(duplicated(tsdf$DateTime))){
    # cut out duplicate dates - only one value per timestamp
    tsdf <- tsdf %>% slice(match(DateTime,unique(DateTime))) %>% arrange(DateTime)
    # Alternative: average by datetime
    #tsdf <- tsdf %>% group_by(DateTime) %>% summarise_each(funs(mean)) %>% ungroup()
  }
  drng <- range(as.Date(windows))
  cuts <- seq(min(windows),max(windows),by=window*60)
  tsdf <- left_join(data.frame(DateTime=cuts), tsdf, by="DateTime")

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
