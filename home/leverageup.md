Sites contributing data with a different sensor array than the core sites need to format the data prior to upload. The main criteria are:
- One header row followed directly by data rows, one row per timestamp
- The first column as a Date-Time stamp converted to UTC standard time and formatted as: `YYYY-MM-DD HH:MM:SS`
- Additional columns for each data variable

### Variables

Possible variables include (*email Aaron if you see an issue here*):
- DateTime (UTC)
- DO (mg/L)
- satDO (mg/L)
- WaterTemp (°C)
- AirTemp (°C)
- WaterPres (kPa)
- AirPres (kPa)
- pH
- fDOM (mV from sensor)
- fDOM (frac)
- Turbidity (mV from sensor)
- Turbidity (NTU)
- Nitrate (mg/L)
- SpecCond (mS/cm)
- SpecCond (μS/cm)
- Depth (m)
- Light (lux)
- Light (PAR)
- CO2 (ppm)

**Convert your variables to one of these formats prior to uploading (check your units!).**

### File naming

Name your upload file with this naming convention: `REGIONID_SITEID_YYYY-MM-DD_XX.csv`
where
- `REGIONID` is the name of your region (US: FIPS state code; Internaitonal: ISO 3166-1 alpha-2 code)
- `SITEID` is your chosen unique site name (only alphanumeric characters, any length you want)
- `YYYY-MM-DD` is the data download date (or the latest date in the timeseries), and
- `XX.csv` tells our data script to treat this as an ancillary site

### Date formatting help

Date time stamps can be challenging to format. If you are using `R` you will want to create a 'POSIXct' object. Below is an example converting a date time string to the correct format:
```R
dtorig <- "08/31/2016 13:24:16" # can also be a vector
dtval <- as.POSIXct(dtorig, format="%m/%d/%Y %H:%M:%S", tz="EST") # this will create a POSIXct object
attr(dtval,"tzone") <- "UTC" # sets the display timezone as UTC
```
The `as.POSIXct()` function can convert any date-time format. For details on all of the format structure codes, [see the R documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html).
