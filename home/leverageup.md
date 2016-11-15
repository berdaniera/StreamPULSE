_Leveraged sites_ need to format the data prior to upload as a `.csv` file with:
- the specified file name (see below),
- one header row followed directly by data rows, one row per timestamp,
- the first column as a Date-Time stamp converted to UTC standard time and formatted as: `YYYY-MM-DD HH:MM:SS`, and
- additional columns for each data variable.

If you are uploading updated data from an existing site, please only include the most recent data (since your last upload) in your file -- our system saves the full uploaded file each time and redundant data just use extra bandwidth.

### Variables

The minimum set of variables for upload are:
- Date-Time (UTC)
- Water Temperature (°C)
- DO (mg/L)
- Saturation DO (mg/L) *and/or* Air Pressure (kPa)
- Depth (m) *and/or* Discharge (m3/s) *and/or* Velocity (m/s)

Bonus variables include:
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

*Convert your variables to one of these formats prior to uploading (check your units!).*

### File naming

Name your upload file -- `REGIONID_SITEID_YYYY-MM-DD.csv` -- where
- `REGIONID` is the name of your region (US: [FIPS state code](https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code); International: [ISO 3166-1 alpha-2 code](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2)),
- `SITEID` is your chosen unique site name (only alphanumeric characters, any length you want), and
- `YYYY-MM-DD` is the download date or the last date in the timeseries.

### Date formatting help

Date-time stamps can be challenging to format.

In `R` you can create a 'POSIXct' object. Below is an example converting a date-time string to the correct format:
```R
datetimeorig <- "8/31/16 13:24:16" # can also be a vector
# In POSIX, we 1. designate the format to match the original date time
#     and 2. specify the timezone... a full list can be viewed by running OlsonNames()
dtval <- as.POSIXct(datetimeorig, format="%m/%d/%y %H:%M:%S", tz="EST")
# Then, just switch the display to UTC
attr(dtval,"tzone") <- "UTC"
# The output will be 2016-08-31T18:24:16Z
```
The `as.POSIXct()` function can convert any date-time format and any time zone. For details on all of the format structure codes, [see the R documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html).

In `matlab` you can create a date time string with the numeric values for your timestamp, accounting for the UTC offset:
```
time.UTC = -5; % UTC offset for EST
timeVec = [time.year time.month time.day time.hour-time.UTC time.min time.sec];
timeStr = datestr(timeVec,'yyyy-mm-dd HH:MM:SS'); % what you will save
```

In `Excel` you can modify the timestamp based on the timezone offset:
```
=TimeCell+(tzOffsetHours/24)
```
and then modify the cell format of the new column manually to match `YYYY-MM-DD HH:MM:SS` and save as a `.csv` from there.

### Saving files

Exporting a `.csv` from R is easy with the `readr` package, which saves files without row names and preserves the ISO date-time format:
```R
library(readr)
write_csv(datatable, path="NC_Eno_2016-10-13.csv")
```
