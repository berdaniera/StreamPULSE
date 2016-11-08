_StreamPULSE core sites_ can upload raw datalogger files as well as pre-formatted data.

### File naming

Name your upload file -- `REGIONID_SITEID_YYYY-MM-DD_LOGGERID.xxx` -- where
- `REGIONID` is the name of your region (PR, FL, NC, WI, or AZ),
- `SITEID` is your unique site name,
- `YYYY-MM-DD` is the download date, and
- `LOGGERID.xxx` is the logger routing code:
  * `CS.dat`: CR1000 data file
  * `HD.csv`: Hobo DO logger
  * `HW.csv`: Hobo water pressure logger
  * `HA.csv`: Hobo air pressure logger
  * `HP.csv`: Hobo light pendant logger
  * `XX.csv`: Calibrated and formatted data

### Calibrated and formatted data

You can upload raw data (from the datalogger) *and/or* calibrated data (e.g., turbidity in NTU, water level or discharge, etc.) at the same time.

If you modify a datalogger file to generate calibrated and derived variables, you must save it as a `.csv` with:
- the `_XX.csv` extension,
- one header row followed directly by data rows, one row per timestamp,
- the first column as a Date-Time stamp **in your local timezone (no DST)** formatted as: `YYYY-MM-DD HH:MM:SS`, and
- additional columns for each data variable.

### Variables

Potential variables include:
- Date-Time (UTC)
- DO (mg/L)
- Saturation DO (mg/L)
- Water Temperature (°C)
- Water Pressure (kPa)
- Air Temperature (°C)
- Air Pressure (kPa)
- Depth (m)
- Discharge (m3/s)
- Velocity (m/s)
- Light, PAR (μmol/m2/s)
- Light, lux
- Specific Conductivity (mS/cm or μS/cm)
- pH
- fDOM (frac)
- fDOM (mV from sensor)
- Turbidity (NTU)
- Turbidity (mV from sensor)
- Nitrate (mg/L)
- CO2 (ppm)

### Date formatting help for calibrated files

Date-time stamps can be challenging to format.

If you are using `R` you can create a 'POSIXct' object. Below is an example converting a date-time string to the correct format:
```R
datetimeorig <- "8/31/2016 13:24:16" # can also be a vector
# In POSIX, we 1. designate the format to match the original date time
#     and 2. specify the timezone...
dtval <- as.POSIXct(datetimeorig, format="%m/%d/%Y %H:%M:%S", tz="EST")
```
The `as.POSIXct()` function can convert any date-time format and any time zone. For details on all of the format structure codes, [see the R documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html).

### Saving files

Exporting a `.csv` from R is easy with the `readr` package, which saves files without row names and preserves the ISO date-time format (GOOD!):
```R
library(readr)
write_csv(datatable, path="NC_Eno_2016-10-13_XX.csv")
```
