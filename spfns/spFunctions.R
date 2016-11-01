checkpkg = function(pkg){
  if(!pkg %in% rownames(installed.packages())) install.packages(pkg)
  library(pkg, character.only=TRUE)
}
checkpkg("zoo")
checkpkg("tibble")
checkpkg("readr")
checkpkg("dplyr")

# Calculate depth with water pressure (kPa), air pressure (kPa), air temp (C), depth_offset (m)
kPa2depth = function(df, depth_offset=NULL, WaterPres_kPa=NULL, AirPres_kPa=NULL, AirTemp_C=NULL){
  # If parameters not individually defined, get the parameters from the dataframe (df)
  if(is.null(depth_offset)) depth_offset = df$depth_offset
  if(is.null(WaterPres_kPa)) WaterPres_kPa = df$WaterPres_kPa
  if(is.null(AirPres_kPa)) AirPres_kPa = df$AirPres_kPa
  if(is.null(AirTemp_C)) AirTemp_C = df$AirTemp_C
  dkpa = WaterPres_kPa - AirPres_kPa # g/(m*s^2)
  p = (999.83952 + 16.945176*AirTemp_C -
      7.9870401e-03*AirTemp_C^2 - 46.170461e-06*AirTemp_C^3 +
      105.56302e-09*AirTemp_C^4 - 280.54253e-12*AirTemp_C^5)/
      (1+16.879850e-03*AirTemp_C) # kg/m^3
  g = 9.80655 # m/s^2
  depth_offset + dkpa*1000/(p*g) # m
}

# FROM STREAM METABOLIZER
calc_light <- function(solar.time, latitude, longitude, max.PAR=2326,
                       coef.SW.to.PAR=formals(convert_SW_to_PAR)$coef) {
  app.solar.time <- solar.time %>%
    convert_solartime_to_UTC(longitude=longitude, time.type='mean solar') %>%
    convert_UTC_to_solartime(longitude=longitude, time.type='apparent solar')
  sw <- calc_solar_insolation(
    app.solar.time, latitude=latitude,
    max.insolation=convert_PAR_to_SW(max.PAR, coef=1/coef.SW.to.PAR),
    format=c("degrees", "radians"))
  par <- convert_SW_to_PAR(sw, coef=coef.SW.to.PAR)
  par
}



#### GET CLIMATE DATA FROM CHAPEL HILL
NCAirPressure = function(dates, start_datetime=NULL, end_datetime=NULL){
  tf = tempfile()
  download.file("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/2016/746939-93785-2016.gz",tf,mode="wb")
  x = read.table(tf)
  x[x==-9999] = NA
  colnames(x) = c("y","m","d","h","air_temp","dewtemp","air_kPa","winddir","sindspeed","skycover","precip1h","precip6h")
  x$air_kPa = x$air_kPa/100
  x$air_temp = x$air_temp/10
  x$DateTime_UTC = parse_datetime(paste0(x$y,"-",sprintf("%02d",x$m),"-",sprintf("%02d",x$d)," ",sprintf("%02d",x$h),":00:00 0"), "%F %T %Z")
  x = as_tibble(x) %>% select(DateTime_UTC,air_temp,air_kPa)
  ss = tibble(DateTime_UTC=seq(x$DateTime_UTC[1], x$DateTime_UTC[nrow(x)], by=900))
  xx = left_join(ss, x)
  xx = mutate(xx, air_temp=na.approx(air_temp), air_kPa=na.approx(air_kPa))
  if(is.null(start_datetime)){
    daterng = range(dates)
  }else{
    daterng = parse_datetime(c(start_datetime,end_datetime),"%Y-%m-%d %T %Z")
  }
  xtmp = xx %>% filter(DateTime_UTC>daterng[1] & DateTime_UTC<daterng[2])
  select(xtmp, DateTime_UTC, air_kPa, air_temp)
}
