Leveraged sites (those with a different sensor-logger array than the core sites) need to format the data prior to upload. The main criteria are:
- One header row followed directly by data rows, one row per timestamp
- The first column as a Date-Time stamp converted to UTC standard time and formatted as: `YYYY-MM-DD HH:MM:SS`
- Additional columns for each data variable

### Variables

The minimum set of variables for upload are:
- Date-Time (UTC)
- Water Temperature (°C)
- DO (mg/L)
- Saturation DO (mg/L) (OR air pressure)
- Depth (m)

Bonus variables include:
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
- Water Pressure (kPa)
- Air Temperature (°C)
- Air Pressure (kPa)

**Convert your variables to one of these formats prior to uploading (check your units!).**

### File naming

Name your upload file with this naming convention: `REGIONID_SITEID_YYYY-MM-DD_XX.csv`
where
- `REGIONID` is the name of your region (US: FIPS state code; Internaitonal: ISO 3166-1 alpha-2 code)
- `SITEID` is your chosen unique site name (only alphanumeric characters, any length you want)
- `YYYY-MM-DD` is the data download date (or the latest date in the timeseries), and
- `XX.csv` tells our data script to treat this as a leveraged site

### Date formatting help

Date time stamps can be challenging to format. If you are using `R` you will want to create a 'POSIXct' object. Below is an example converting a date time string to the correct format:
```R
dtorig <- "08/31/2016 13:24:16" # can also be a vector
# In POSIX, we designate the format to match the original date time and specify the timezone of the original date time...
dtval <- as.POSIXct(dtorig, format="%m/%d/%Y %H:%M:%S", tz="EST")
# Then, just switch the display to UTC
attr(dtval,"tzone") <- "UTC"
```
The `as.POSIXct()` function can convert any date-time format and any time zone. For details on all of the format structure codes, [see the R documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html).

### Saving files

Exporting a .csv from R is easy with the `readr` package:
```R
library(readr) # saves files without row names and preserves the ISO date-time format
write_csv(datatable, file="NC_Eno_2016-10-13_XX.csv")
```
