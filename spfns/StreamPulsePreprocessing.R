### StreamPULSE data preprocessing
# Aaron Berdanier
# aaron.berdanier@gmail.com
# Last updated: 2016-10-06

# set working directory to find your data files
# This will be a file path. It could be something like: wd <- "C:\Users\username\Desktop\StreampulseUpload\"
wd <- "."
setwd(wd)
# This directory should contain the files that you wish to upload and the spfns.R file
# File formatting note (see SOP for more details):
#   Files should be as `.dat` from CR1000 loggers, `.csv` from HOBO loggers, or `.csv` from other systems
#   File names should match `XX_SiteName_YYYYMMDD_ZZ` where XX is the site code and ZZ is the data logger code
#   Data from HOBO loggers will include DO, Light, Water Pressure/temp and Air Pressure/temp

# load required packages
library(dplyr)
library(readr)
source("spfns.R")


################
### 1. LOAD DATA
# `sp_in()` loads and munges all data files from the defined site and download date
# It needs `sitedate` (as `REGIONID_SITEID_YYYYMMDD`) and `gmt.off`

sitedate <- "NC_Eno_20160922"

# We need timezone information to correctly convert the Campbell Scientific TIMESTAMPs
# IMPORTANT: CR1000 dataloggers sync to the download computer
#   If the clock on your download computer adjusts for daylight savings time (it may or may not),
#   then the `dst` option *must* be set to TRUE (this is the default setting)
# For lat and lng, accuracy within a degree should be fine
#   e.g., in NC:  lat <- 36; lng <- (-78)
lat <- 36
lng <- (-78)
gmtoff <- get_gmtoff(lat, lng, sitedate, dst=TRUE)

data <- sp_in(sitedate, gmtoff)
data

################
### 2. CONVERSIONS where necessary
depth_offset <- 0 # The distance (in m) from the bed to the water pressure sensor
fdom_offset <- 0 # The measured calibration offset for the fDOM sensor
turb_offset <- 0 # The measured calibration offset forthe turbidity sensor

# Calculate water depth (m) from pressure (kPa)
# This function requires a preprocessed table and a `depth_offset`
# It can also be calculated by explicitly defining matched data set variables:
#   `water_kPa`, `air_kPa`, `air_temp`
data$depth <- kPa2depth(data, depth_offset)

# Calculate turbidity (NTU) from Cyclops 7 mV reading
# This function requires the measured mV column from the data table
data$TurbidityNTU <- mV2turb(data$Turb, turb_offset)

# Calculate fDOM from Cyclops 7 mV reading
data$fDOM <- mV2fdom(data$fDOM, fdom_offset)

# mV from Viasala to CO2 (ppm)
# coming soon...


################
# 3. SELECT EXPORT COLUMNS AND SAVE
# In the select() function, define the data columns that you'd like to export
# These are the data that you will upload onto StreamPULSE
dataoutput <- data %>% select(DateTime,pH,DOconcmgL,AbsPreskPa)
write_csv(dataoutput, paste0(sitedate,".csv"))
# This will save a file in your working directory with the name: REGIONID_SITEID_DOWNLOADDATE.csv
