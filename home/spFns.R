### FUNCTIONS FOR PRE-PROCESSING STREAMPULSE DATA ###
checkpkg = function(pkg){
  if(!pkg %in% rownames(installed.packages())) install.packages(pkg)
  library(pkg, character.only=TRUE)
}
checkpkg("dplyr")
checkpkg("readr")
checkpkg("httr")
checkpkg("tibble")

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

# f = grep("NC_Eno_.*",list.files("/home/aaron/Documents/Data/toup"),value=T)
# fx = paste0("/home/aaron/Documents/Data/toup/",f)
# f = f[1]
# setwd("/home/aaron/Documents/Data/toup")

### DATA LOADING
# Read Hobo data .csv
read_hobo = function(f, fnm){
  f1 = read_csv(f, skip=1, col_types = cols())
  if(any(grepl("Coupler|File|Host|Connected|Attached|Stopped",colnames(f1)))){
    f1 = f1[,-grep("Coupler|File|Host|Connected|Attached|Stopped",colnames(f1))]
  }
  # f1 = f1[,!grepl("Coupler|File|Stopped",colnames(f1))]
  # parse column names
  m = regexpr("\\,.*",colnames(f1),perl=T)
  uu = sapply(strsplit(regmatches(colnames(f1),m)," "), function(x) x[2]) # get units
  tzoff = ifelse(grepl("-",uu[1]), sub("-","+",uu[1]), sub("+","-",uu[1]))
  uu = gsub("°(.*)","\\1",uu)[-1] # get rid of degree symbol -- annoying
  cc = unlist(lapply(strsplit(colnames(f1),","),function(x) x[1]))[-c(1:2)]
  colnames(f1) = gsub("\\ |\\/","",c("N","DateTimeUTC",paste(cc,uu)))
  # fix time zones
  dada = sub("^(.*\\ )12(:.{2}:.{2}\\ )AM(.*)$", "\\10\\2AM\\3", paste(f1$DateTimeUTC,tzoff)) # convert 12AM to 0AM (midnight)
  dada = sub("^(.*\\ )12(:.{2}:.{2}\\ )PM(.*)$", "\\112\\2AM\\3", dada) # convert 12PM to 12AM (noon)
  f1$DateTimeUTC = parse_datetime(dada,"%D %T %p %Z")
  # change column names
  if(grepl("_HW",fnm)) f1 = rename(f1, water_kPa=AbsPreskPa, water_temp=TempC) # water pressure file
  if(grepl("_HA",fnm)) f1 = rename(f1, air_kPa=AbsPreskPa, air_temp=TempC) # air pressure file
  if(grepl("_HD",fnm)) f1 = rename(f1, DO_temp=TempC) # DO file
  if(grepl("_HP",fnm)) f1 = rename(f1, light_temp=TempC) # DO file
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
load_file = function(f, gmtoff, logger, fnm){
  if(logger == "CS"){ # cs data logger
    read_csci(f, gmtoff)
  }else if(grepl("H",logger)){ # hobo data logger
    read_hobo(f, fnm)
  }else{ # other data - must have just one header row
    xtm = read_csv(f, col_types=cols())
    colnames(xtm)[1] = "DateTimeUTC"
    xtm
  }
}

# stack data files from the same data logger but different dates
load_stack_file = function(files, gmtoff, logger){
  dates = sub(".*_(.*)_.*\\..*", "\\1", files$name) # get all dates
  xx = lapply(1:nrow(files), function(x) load_file(files$datapath[x], gmtoff$offs[which(gmtoff$dnld_date==dates[x])], logger, files$name[x]) ) # load data for each file
  # xx = lapply(dates, function(x) load_file(files$datapath[grepl(x,files$name,value=TRUE), gmtoff$offs[which(gmtoff$dnld_date==x)], logger) ) # load data for each date
  xx = Reduce(function(df1,df2) bind_rows(df1,df2), xx) # stack them up
  arrange(xx, DateTimeUTC)
}

# Read and munge files for a site and date
sp_in = function(ff, gmtoff=NULL){
  logger = unique(sub("(.*_)(.*)\\..*", "\\2", ff$name)) # which data loggers are represented?
  if(length(ff$name)==1){ # only one file, load it regularly
    cat(paste0(ff$datapath,"\n"))
    xx = load_file(ff$datapath, gmtoff$offs, logger, ff$name)
    x = list(wash_ts(xx, dup_action="average", samp_freq="15M"))
  }else{ # multiple files
    x = lapply(logger,function(l){ # files from each logger
      f = grep(l, ff$name, value=TRUE)
      cat(paste0(f,"\n"))
      if(length(f)>1){ # load and stack files
        xx = load_stack_file(ff[which(ff$name%in%f),], gmtoff, l)
      }else{ # load the file
        offs = gmtoff$offs[which(gmtoff$dnld_date==sub(".*_(.*)_.*\\..*", "\\1", f))]
        xx = load_file(ff$datapath[which(ff$name==f)], offs, l, f)
      }
      wash_ts(xx, dup_action="average", samp_freq="15M")
    })
  }
  fold_ts(x)
}

sp_in_lev = function(ff){ # single file...
  cat(paste0(ff$datapath,"\n"))
  xx = read_csv(ff$datapath)
  wash_ts(xx, dup_action="average", samp_freq="15M")
}

get_gmtoff = function(lat, lng, dnld_date, dst=TRUE){
#  obs_date = sub("(.*_)(.*)", "\\2", sitedate)
  ts = as.numeric(as.POSIXct(dnld_date,format="%Y-%m-%d",origin="1970-01-01"))
  ur = paste0("https://maps.googleapis.com/maps/api/timezone/json?timestamp=",ts,"&location=",lat,",",lng)
  offs = as.numeric(sapply(ur,function(ur){
    res = GET(ur)
    if(status_code(res)!=200) stop("Error with API. \n You'll need to try again or get the offset somewhere else.")
    out = content(res)
    offs = out$rawOffset
    if(dst) offs = offs+out$dstOffset
    offs = offs/3600
    offs
  }))
  # cat("Google got the timezone offsets for you:\n",paste0(dnld_date,": ",offs," hours\n"))
  tibble(dnld_date,offs)
}


### DATA MANAGEMENT
# Snap timestamps to the closest interval
snap_ts = function(x, samp_freq, nearest=FALSE){
  # x is a POSIX date-time vector to be snapped
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
    as.POSIXct(round(as.double(x)/td)*td,origin="1970-01-01",tz="UTC")
  }else{ # round to floor
    as.POSIXct(floor(as.double(x)/td)*td,origin="1970-01-01",tz="UTC")
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
  colnames(x)[1] = "DateTime_UTC"

  # check data
  check_ts(x$DateTime_UTC, samp_freq=samp_freq)

  # sort ascending by date-time
  x = arrange(x, DateTime_UTC)

  # snap to date interval if samp_freq is defined
  if(!is.null(samp_freq)){
    x$DateTime_UTC = snap_ts(x$DateTime_UTC, samp_freq, nearest=FALSE)
  }

  # Remove duplicates
  if(any(duplicated(x$DateTime_UTC))){
    if(dup_action=="average"){
      x = x %>% group_by(DateTime_UTC) %>% summarise_each(funs(mean(.,na.rm=T))) %>% ungroup()
    }else if(dup_action=="drop"){
      x = slice(x, match(DateTime_UTC, unique(DateTime_UTC)))
    }else{
      cat("Duplicates found but no action taken.\n")
    }
  }
  x
}

# Fold the data together into one data frame
fold_ts = function(...){
  ll = (...)
  if(length(ll)>1){
     x = Reduce(function(df1,df2) full_join(df1,df2,by="DateTime_UTC"), ll)
  }else{
    x = ll[[1]]
  }
  # cat("Your data are cleaned.\n")
  arrange(x, DateTime_UTC)
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
  dataoutput = data %>% select_(.dots=c("DateTime_UTC",getvars))
  ddate = as.character(sort(as.Date(dnld_date), decreasing=TRUE))[1] # get latest date
  write_csv(dataoutput, paste0(site,"_",ddate,".csv"))
  cat(paste0("The merged data were saved as ",site,"_",ddate,".csv\n"))
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
