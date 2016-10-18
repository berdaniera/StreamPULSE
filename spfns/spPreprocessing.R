### StreamPULSE data preprocessing
# Aaron Berdanier
# aaron.berdanier@gmail.com
# Last updated: 2016-10-18

# FYI: you need an internet connection to run this code
# Some functions to load
checkpkg <- function(pkg) if(!pkg %in% rownames(installed.packages())) install.packages(pkg)
source_github <- function(url){
  checkpkg("RCurl")
  code <- RCurl::getURL(url, followlocation=TRUE, cainfo=system.file("CurlSSL", "cacert.pem", package="RCurl"))
  eval(parse(text=code), envir=.GlobalEnv)
}

# Set working directory to find your data files
# This will be a file path. It could be something like: wd <- "C:/Users/username/Desktop/StreampulseUpload/"
wd <- "/home/aaron/Desktop/SP/"
setwd(wd)
# This directory should contain the datalogger files that you wish to upload
# FOR EXAMPLE, you might have four files in this folder:
# - NC_Eno_2016-10-06_CS.dat - the campbell scientific CR1000 file
# - NC_Eno_2016-10-06_HA.csv - the hobo air pressure file
# - NC_Eno_2016-10-06_HD.csv - the hobo oxygen sensor file
# - NC_Eno_2016-10-06_HW.csv - the hobo water pressure file

# Load the latest spFns file
source_github("https://raw.githubusercontent.com/berdaniera/StreamPULSE/master/spfns/spFns.R")
# IF THIS DOESN'T WORK... let me know!
# Alternatvely, you can go to the GitHub link above, save the spFns.R file into your working directory, and then run:
#source("spFns.R")

################
### 1. LOAD DATA
# `sp_in()` loads and munges all data files from the defined site and download date
# It needs `sitedate` (as `REGIONID_SITEID_YYYY-MM-DD`) and `gmtoff`

site <- "NC_Eno"
dnld_date <- "2016-10-06"
# The functions can accommodate multiple download dates
#dnld_date <- c("2016-10-06","2016-10-11","2016-10-13")

# We need timezone information to correctly convert the Campbell Scientific TIMESTAMPs
# IMPORTANT: CR1000 dataloggers sync the clock to the download computer
#   If the clock on your download computer adjusts for daylight savings time
#    then the `dst` option **must** be set to TRUE (this is the default setting).
# For lat and lng, accuracy within a degree should be fine
#   e.g., in NC:  lat <- 36; lng <- (-78)
lat <- 36
lng <- (-78)
gmtoff <- get_gmtoff(lat, lng, dnld_date, dst=TRUE)

data <- sp_in(site, dnld_date, gmtoff)
data


################
### 2. CONVERSIONS where necessary
# You only need to run the ones of these that you need.
# For example, for sites with only Hobo loggers, you will not need to run the Turbidity and fDOM calculations

# Conversion factors for sensors
depth_offset <- 0 # The distance (in m) from the bed to the water pressure sensor
fdom_offset <- 0 # The measured calibration offset for the fDOM sensor
turb_offset <- 0 # The measured calibration offset forthe turbidity sensor

# Calculate water depth (m) from pressure (kPa)
# This function requires a preprocessed table and a `depth_offset`
# It can also be calculated by explicitly defining matched data set variables:
#   `water_kPa`, `air_kPa`, `air_temp`
data$depthm <- kPa2depth(data, depth_offset)

# Calculate turbidity (NTU) from Cyclops 7 mV reading
# This function requires the measured mV column from the data table
data$TurbidityNTU <- mV2turb(data$Turb, turb_offset)

# Calculate fDOM from Cyclops 7 mV reading
data$fDOM <- mV2fdom(data$fDOM, fdom_offset)

# coming soon...
# Viasala to CO2 (ppm)
# Lux to par?


################
# 3. SELECT EXPORT COLUMNS AND SAVE
# View the columns in the data -- these are your options!
colnames(data)

# Make a list of the data variables that you'd like to save
# These are the data that you will upload to the StreamPULSE database
# (the function automatically grabs "DateTime", so you don't need to add it here)
getvars <- c("water_temp", "depthm", "DOconcmgL")

# This will save a file in your working directory with the name: REGIONID_SITEID_DOWNLOADDATE.csv
save_SPcsv(getvars)
