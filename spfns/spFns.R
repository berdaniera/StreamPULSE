### FUNCTIONS FOR PRE-PROCESSING STREAMPULSE DATA ###
if(!require(dplyr)) install.packages("dplyr")
if(!require(readr)) install.packages("readr")
if(!require(httr)) install.packages("httr")
library(dplyr)
library(readr)

x
### DATA CHECKING
check_ts = function(x, samp_freq=NULL){
  # x is a vector of timestamps
  # function checks for date order, gaps in data, and missing days
  dx = diff(unique(x))
  units(dx) = "secs"
  if(is.null(samp_freq)){
    # samp_freq = mode
    mdx = as.numeric(names(sort(table(dx),decreasing=T)[1]))
  }else{
    re = regexec("([0-9]+)([A-Z])",samp_freq)[[1]]
    if(-1%in%re){
      stop("Please enter a correct string")
    }else{
      ml = attr(re,"match.length")
      nn = as.numeric(substr(samp_freq, re[2], ml[2]))
      uu = substr(samp_freq, re[3], ml[1])
      if(uu=="D"){ mdx = 24*60*60*nn
      }else if(uu=="H"){ mdx = 60*60*nn
      }else if(uu=="M"){ mdx = 60*nn
      }else if(uu=="S"){ mdx = nn
      }else{stop("Please enter a correct string")}
    }
  }
  gaps = tibble(t1=x[which(dx != mdx)],t2=x[which(dx != mdx)+1])
  numbergaps = nrow(gaps)
  wrongorder = length(which(dx < 0)) # n DateTime out of order
  missingdays = sum(as.numeric(gaps$t2-gaps$t1,units="secs"))/86400
  missingdays = as.numeric(missingdays) # number of missing days
  cat(" Sampling frequency:",mdx/60,"minutes\n",
    "Date-times out of order:",wrongorder,"\n",
    "Gaps in data:",numbergaps,"\n",
    "Missing days:",missingdays,"\n")
}


ff
f = ff[10]


### DATA LOADING
# Read Hobo data .csv
read_hobo = function(f){
  f1 = read_csv(f, skip=1, col_types = cols())
  f1 = f1[,-grep("Coupler|File|Stopped",colnames(f1))]
  # parse column names
  m = regexpr("\\,.*",colnames(f1),perl=T)
  uu = sapply(strsplit(regmatches(colnames(f1),m)," "), function(x) x[2]) # get units
  uu = gsub("°(.*)","\\1",uu)[-1] # get rid of degree symbol -- annoying
  cc = unlist(lapply(strsplit(colnames(f1),","),function(x) x[1]))[-c(1:2)]
  colnames(f1) = gsub("\\ |\\/","",c("N","DateTimeUTC",paste(cc,uu)))
  # fix time zones
  tzoff = ifelse(grepl("-",uu[1]), sub("-","+",uu[1]), sub("+","-",uu[1])) # switch time zone to match R conventions
  dada = sub("^(.*\\ )12(:.{2}:.{2}\\ )AM(.*)$", "\\10\\2AM\\3", paste(f1$DateTimeUTC,tzoff)) # convert 12AM to 0AM (midnight)
  dada = sub("^(.*\\ )12(:.{2}:.{2}\\ )PM(.*)$", "\\112\\2AM\\3", dada) # convert 12PM to 12AM (noon)
  f1$DateTimeUTC = parse_datetime(dada,"%D %T %p %Z")
  # change column names
  if(grepl("_HW",f)) f1 = rename(f1, water_kPa=AbsPreskPa, water_temp=TempC) # water pressure file
  if(grepl("_HA",f)) f1 = rename(f1, air_kPa=AbsPreskPa, air_temp=TempC) # air pressure file
  if(grepl("_HD",f)) f1 = rename(f1, DO_temp=TempC) # DO file
  if(grepl("_HP",f)) f1 = rename(f1, light_temp=TempC) # DO file
  f1 %>% filter(apply(f1,1,function(x) all(!is.na(x)))) %>% select(-N)
}

# Read Campbell Scientific data .dat
read_csci = function(f, gmtoff){
  if(is.null(gmtoff)) stop("Time zone offset not defined. \n Please include it and try again.")
  tzoff = -gmtoff # need to be negative because R does ts offsets backwards from intuition
  hh = read_csv(f, skip=1, n_max=2, col_types=cols()) # load header row
  f1 = read_csv(f, skip=4, col_names=colnames(hh),col_types=cols(TIMESTAMP = "c")) # load data
  f1$DateTimeUTC = parse_datetime(paste(f1$TIMESTAMP,tzoff),"%F %T %Z")
  ccn = colnames(f1)
  f1 %>% select_(.dots=c("DateTimeUTC",ccn[-grep("DateTimeUTC|TIMESTAMP",ccn)]))
}

# Load streampulse files - based on data logger type
load_file = function(f, gmtoff, logger){
  cat(paste0(f,"\n"))
  if(logger == "CS"){ # cs data logger
    read_csci(f, gmtoff)
  }else if(grepl("H",logger)){ # hobo data logger
    read_hobo(f)
  }else{ # other data - must have just one header row
    read_csv(f, col_types=cols())
  }
}

# stack data files from the same data logger but different dates
load_stack_file = function(files, gmtoff, logger){
  dates = sub(".*_(.*)_.*\\..*", "\\1", files) # get all dates
  xx = lapply(dates, function(x) load_file(grep(x,files,value=TRUE), gmtoff$offs[which(gmtoff$dnld_date==x)], logger) ) # load data for each date
  xx = Reduce(function(df1,df2) bind_rows(df1,df2), xx) # stack them up
  arrange(xx, DateTimeUTC)
}


# Read and munge files for a site and date
sp_in = function(site, dnld_date, gmtoff=NULL){
  sitedate = paste0(site,"_",dnld_date,"_")
  if(length(sitedate)>1) sitedate = paste(sitedate, collapse="|")
  ff = grep(sitedate, list.files(), value=TRUE) # only get files that are from dataloggers
  if(length(ff)==0) stop("No files found matching that site-date combination...")
  logger = unique(sub("(.*_)(.*)\\..*", "\\2", ff)) # which data loggers are represented?
  if(length(ff)==1){ # only one file, load it regularly
    xx = load_file(ff, gmtoff$offs, logger)
    x = wash_ts(xx, dup_action="average", samp_freq="15M")
  }else{ # multiple files
    x = lapply(logger,function(l){ # files from each logger
      f = grep(l, ff, value=TRUE)
      if(length(f)>1){ # load and stack files
        xx = load_stack_file(f, gmtoff, l)
      }else{ # load the file
        offs = gmtoff$offs[which(gmtoff$dnld_date==sub(".*_(.*)_.*\\..*", "\\1", f))]
        xx = load_file(f, offs, l)
      }
      wash_ts(xx, dup_action="average", samp_freq="15M")
    })
  }
  fold_ts(x)
}


get_gmtoff = function(lat, lng, dnld_date, dst=TRUE){
#  obs_date = sub("(.*_)(.*)", "\\2", sitedate)
  ts = as.numeric(as.POSIXct(dnld_date,format="%Y-%m-%d",origin="1970-01-01"))
  ur = paste0("https://maps.googleapis.com/maps/api/timezone/json?timestamp=",ts,"&location=",lat,",",lng)
  offs = as.numeric(sapply(ur,function(ur){
    res = httr::GET(ur)
    if(httr::status_code(res)!=200) stop("Error with API. \n You'll need to try again or get the offset somewhere else.")
    out = httr::content(res)
    offs = out$rawOffset
    if(dst) offs = offs+out$dstOffset
    offs = offs/3600
    offs
  }))
  cat("Google got the timezone offsets for you:\n",paste0(dnld_date,": ",offs," hours\n"))
  tibble(dnld_date,offs)
}


### DATA MANAGEMENT
# Snap timestamps to the closest interval
snap_ts = function(x, samp_freq, nearest=FALSE){
  # x is a date-time vector to be snapped
  # freq is the frequency of observations as a string
  #   containing the number of units and the unit (S,M,H,D)
  #   e.g., '15M', '1H', '3D', '66S'
  # nearest is logical to snap to floor (default) or to nearest time cut
  re = regexec("([0-9]+)([A-Z])",samp_freq)[[1]]
  if(-1%in%re){
    stop("Please enter a correct string")
  }else{
    ml = attr(re,"match.length")
    nn = as.numeric(substr(samp_freq, re[2], ml[2]))
    uu = substr(samp_freq, re[3], ml[1])
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

# Spread dates out to equal intervals, adding spaces for empty timestamps
# spread_ts() - add option to fill, interpolate, or NA

# Wash the data
wash_ts = function(x, dup_action=c("average","drop"), samp_freq=NULL, dt_colname=NULL, ...){
  if(is.null(dt_colname)){
    cx = colnames(x)[which(sapply(x, function(x) inherits(x, "POSIXct")))]  # find column with date-time formatting
    if(length(cx) == 0) stop("No date-time columns.")
    if(length(cx) > 1) stop("Too many date-time columns. Remove duplicates or specify `dt.colname`.")
  }else{
    cx = dt_colname
  }
  if(colnames(x)[1] != cx){  # move date-time column to first column
    x = x[,c(cx,colnames(x)[-which(colnames(x)==cx)])]
  }
  colnames(x)[1] = "DateTime"

  # check data
  check_ts(x$DateTime, samp_freq=samp_freq)

  # sort ascending by date-time
  x = arrange(x, DateTime)

  # snap to date interval if samp_freq is defined
  if(!is.null(samp_freq)){
    x$DateTime = snap_ts(x$DateTime, samp_freq, nearest=FALSE)
  }

  # Remove duplicates
  if(any(duplicated(x$DateTime))){
    if(dup_action=="average"){
      x = x %>% group_by(DateTime) %>% summarise_each(funs(mean(.,na.rm=T))) %>% ungroup()
    }else if(dup_action=="drop"){
      x = slice(x, match(DateTime, unique(DateTime)))
    }else{
      cat("Duplicates found but no action taken.\n")
    }
  }

  x
}

# Fold the data together into one data frame
fold_ts = function(...){
  if(!is.list(...)){ ll = list(...) }else{ ll = (...) }
  if(!all(sapply(ll,function(x) colnames(x)[1]=="DateTime"))){
    print("Please clean all data sets before running merge_ts()")
  }
  cat("Your data are cleaned.\n")
  Reduce(function(df1,df2) full_join(df1,df2,by="DateTime"), ll)
}


### DATA TRANSFORMATIONS
# Pressure (kPa) to water depth (m)
kPa2depth = function(df, depth_offset, water_kPa=NULL, air_kPa=NULL, air_temp=NULL){
  # If not defined, get the parameters from the dataframe (df)
  if(is.null(water_kPa)) water_kPa = df$water_kPa
  if(is.null(air_kPa)) air_kPa = df$air_kPa
  if(is.null(air_temp)) air_temp = df$air_temp
  dkpa = water_kPa - air_kPa # g/(m*s^2)
  p = (999.83952 + 16.945176*air_temp -
      7.9870401e-03*air_temp^2 - 46.170461e-06*air_temp^3 +
      105.56302e-09*air_temp^4 - 280.54253e-12*air_temp^5)/
      (1+16.879850e-03*air_temp) # kg/m^3
  g = 9.80655 # m/s^2
  depth_offset + dkpa*1000/(p*g) # m
}

# mV from Cyclops 7 to turbidity (NTU)
mV2turb = function(mV, turb_offset){
  # Turbidity = ((Scale factor of sensors)*(Voltage from sensor)) + offset
  600*(mV/1000) + turb_offset
}

# mV from Cyclops 7 to fDOM
mV2fdom = function(mV, fdom_offset){
  # fDOM = ((Scale factor of sensors)*(Voltage from sensor)) + offset
  500*(mV/1000) + fdom_offset
}

# mV to CO2 (ppm)
# mV2CO2 = function(){}



### IN THE FUTURE, make a transformation pipeline function


### DATA DOWNLOAD
save_SPcsv = function(getvars){
  dataoutput = data %>% select_(.dots=c("DateTime",getvars))
  write_csv(dataoutput, paste0(sitedate,".csv"))
  cat(paste0("The merged data were saved as ",sitedate,".csv\n"))
}



# NOTES FOR AARON:
# HAVE AN OPTION TO CONVERT IF YOU DIDN"T DO IT IN THE CAMPBELL CODE
# NEED TO STANDARDIZE UNITS
# 1. Getting data into the right format
 # - R script
# 2. Data cleaning and QAQC
 # - web interface

# 3. Upload onto CUAHSI (us) - we've worked out the format already
 # - Dropbox to web interface

 # QUESTIONS FOR DATA MEETING
 # Do we retain the samples or the averages?
 # Units - function documentation that Aaron does - what units go in...
 # Convo about comfort with R
 # Overview of date formats
