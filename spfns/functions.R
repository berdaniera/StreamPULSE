check_ts = function(x, samp.freq=NULL){
  dx = diff(x)
  units(dx) = "secs"
  if(is.null(samp.freq)){
    # samp.freq = mode
    mdx = as.numeric(names(table(dx)[1]))
  }else{
    re = regexec("([0-9]+)([A-Z])",samp.freq)[[1]]
    if(-1%in%re){
      stop("Please enter a correct string")
    }else{
      ml = attr(re,"match.length")
      nn = as.numeric(substr(samp.freq, re[2], ml[2]))
      uu = substr(samp.freq, re[3], ml[1])
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
  missingdays = sum(as.numeric(gaps$t2-gaps$t1,units="secs"))
  missingdays = as.numeric(floor(missingdays)) # number of missing days
  cat("Data:",deparse(substitute(x)),"\n",
    "Sampling frequency:",mdx/60,"minutes\n",
    "Date-times out of order:",wrongorder,"\n",
    "Gaps in data:",numbergaps,"\n",
    "Missing days:",missingdays,"\n")
}

# Data Steps
# for each download:
# 1. Load data
# get_csci()
# get_hobo()
# sp_data()

read_hobo = function(ff){
  f1 = read_csv(ff, skip=1)
  f1 = f1[,-grep("Coupler|File|Stopped",colnames(f1))]
  m = regexpr("\\,.*",colnames(f1),perl=T) # parse column names
  uu = sapply(strsplit(regmatches(colnames(f1),m)," "), function(x) x[2]) # get units
  colnames(f1) = gsub("\\ |\\/","",c("N","DateTimeUTC",paste(unlist(strsplit(colnames(f1)[3],","))[1],uu[2]),"Temp"))
  tzoff = ifelse(grepl("-",uu[1]), sub("-","+",uu[1]), sub("+","-",uu[1])) # switch time zone to match R conventions
  dada = sub("^(.*\\ )12(:.{2}:.{2}\\ )AM(.*)$", "\\10\\2AM\\3", paste(f1$DateTimeUTC,tzoff)) # convert 12AM to 0AM (midnight)
  dada = sub("^(.*\\ )12(:.{2}:.{2}\\ )PM(.*)$", "\\112\\2AM\\3", dada) # convert 12PM to 12AM (noon)
  f1$DateTimeUTC = parse_datetime(dada,"%D %T %p %Z")
  #if("AbsPreskPa" %in% colnames(f1)) f1 = f1 %>% mutate(AbsPresPa = AbsPreskPa*1000) %>% select(-AbsPreskPa)
  f1 %>% filter(apply(f1,1,function(x) all(!is.na(x)))) %>% dplyr::select(-N)
}

read_csci = function(ff){
  tzoff = 4 # EST
  if(grepl("WI_", ff)) tzoff = 5
  if(grepl("AZ_|WY_", ff)) tzoff = 6
  hh = read_csv(ff, skip=1, n_max=2)
  f1 = read_csv(ff, skip=4, col_names=colnames(hh),col_types=cols(TIMESTAMP = "c"))
  f1$DateTimeUTC = parse_datetime(paste(f1$TIMESTAMP,tzoff),"%m/%d/%Y %T %Z")
  ccn = colnames(f1)
  f1 %>% dplyr::select_(.dots=c("DateTimeUTC",ccn[-grep("DateTimeUTC",ccn)])) %>% rename(LocalDateTime = TIMESTAMP)
}

# function to load streampulse files
sp_in = function(ff){
  datalogger = sub("(.*_)(.*)\\..*", "\\2", ff)
  if(datalogger == "CS"){ # cs data logger
    read_csci(ff)
  }else if(grepl("H",datalogger)){ # hobo data logger
    read_hobo(ff)
  }else{
    read_csv(ff)
  }
}



####
snap_ts = function(x, samp.freq, nearest=FALSE){
  # x is a date-time vector to be snapped
  # freq is the frequency of observations as a string
  #   containing the number of units and the unit (S,M,H,D)
  #   e.g., '15M', '1H', '3D', '66S'
  # nearest is logical to snap to floor (default) or to nearest time cut
  re = regexec("([0-9]+)([A-Z])",samp.freq)[[1]]
  if(-1%in%re){
    stop("Please enter a correct string")
  }else{
    ml = attr(re,"match.length")
    nn = as.numeric(substr(samp.freq, re[2], ml[2]))
    uu = substr(samp.freq, re[3], ml[1])
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
# spread_ts() - spread dates out
# - option to fill, interpolate, or NA

# 2. wash the data
wash_ts = function(x, dup.action=c("average","drop"), samp.freq=NULL, dt.name=NULL, ...){
  if(is.null(dt.name)){
    cx = colnames(x)[which(sapply(x, function(x) inherits(x, "POSIXct")))]  # find column with date-time formatting
    if(length(cx) == 0) stop("No date-time columns.")
    if(length(cx) > 1) stop("Too many date-time columns. Remove duplicates or specify `dt.name`.")
  }else{
    cx = dt.name
  }
  if(colnames(x)[1] != cx){  # move date-time column to first column
    x = x[,c(cx,colnames(x)[-which(colnames(x)==cx)])]
  }
  colnames(x)[1] = "DateTime"

  # check data
  check_ts(x$DateTime, samp.freq=samp.freq)

  # sort ascending by date-time
  x = arrange(x, DateTime)

  # snap to date interval if samp.freq is defined
  if(!is.null(samp.freq)){
    x$DateTime = snap_ts(x$DateTime, samp.freq, nearest=FALSE)
  }

  # Remove duplicates
  if(any(duplicated(x$DateTime))){
    if(dup.action=="average"){
      x = x %>% group_by(DateTime) %>% summarise_each(funs(mean(.,na.rm=T))) %>% ungroup()
    }else if(dup.action=="drop"){
      x = slice(x, match(DateTime, unique(DateTime)))
    }else{
      print("Duplicates found but no action taken.")
    }
  }

  print("Your data are cleaned.")
  x
}

# 3. Fold the data together
fold_ts = function(...){
  if(!is.list(...)){ ll = list(...) }else{ ll = (...) }
  if(!all(sapply(ll,function(x) colnames(x)[1]=="DateTime"))){
    print("Please clean all data sets before running merge_ts()")
  }
  Reduce(function(df1,df2) full_join(df1,df2,by="DateTime"), ll)
}


# 4. Transform

# Pressure to level
calc_level = function(water.kpa, air.temp, air.kpa, sensor.height){
  dkpa = water.kpa - air.kpa # g/(m*s^2)
  p = (999.83952 + 16.945176*Tref -
      7.9870401e-03*air.temp^2 - 46.170461e-06*air.temp^3 +
      105.56302e-09*air.temp^4 - 280.54253e-12*air.temp^5)/
      (1+16.879850e-03*air.temp) # kg/m^3
  g = 9.80655 # m/s^2
  (sensor.height + dkpa*1000/(p*g)) # m
}

# CS mV to turbidity
# Turbidity = ((Scale factor of sensors)*(Voltage from sensor)) + offset
# Turb = ((range of sensor with set gain/5V)*(Voltage)) + offset
# Turb = ((3000/5)*(Voltage)) + offset
# Turb = 600*Voltage + offset
# Calculated offset of turbidity sensors are ~+48.62
calc_turb = function(voltage, turb.off=48.62){
  600*voltage + offset
}

# CS mV to fDOM
# fDOM conversion is done the same way...
# fDOM = ((2500/5)*Voltage) + offset
# fDOM = 500*Voltage + offset
# Calculated offset of fDOM sensors are ~+35.42
calc_fdom = function(voltage, fdom.off=35.42){
  500*voltage + offset
}


# 5. select columns and export





# spdata.r
#depends
library(tidyverse)

# set wd

# enter standards and date to fit
# date formatting
fdate = "20160804"
location = "NC"
sites = "Eno"
fl = c("CS","HD","HL")
ff = paste0(location,"_",sites,"_",fdate,"_",fl,".csv")

x = sp_in(ff[3])
x = wash_ts(x, dup.action="drop", samp.freq="15M")

# run cleaning
# 1. load and wash data
x = sapply(ff,function(f){
  xx = sp_in(f)
  wash_ts(xx, dup.action="average", samp.freq="15M")
})

# 2. fold the data up
data = fold_ts(x)

data %>% dplyr::select(DateTime,pH,DOconcmgL,AbsPreskPa)


# 3. transforms
# WHAT OTHER TRANFORMS DO WE NEED?
# LIGHT?
# UNITS


# 4. select columns and save

write_csv(data,"siteid_location_fdate.csv")
