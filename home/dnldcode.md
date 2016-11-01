For now, this download gives you the entire data record for the selected sites at once. The output is a `.csv` file with four columns (DateTime_UTC, Site, variable, and value).

To go from the `tidy` format to the 'wide' format, you can follow this script:
```R
library(readr) # to read the csv in
library(tidyr) # to spread the data out
dat <- read_csv("path/to/your/StreamPULSE_YYYY-MM-DD.csv")
dat <- spread(dat, variable, value)
```

Then, the data are easy to graph, filter, manipulate, etc.
```R
library(dplyr) # for munging the data
datEno <- dat %>% filter(Site="NC_Eno") %>% select(-Site)
```

More data examples to come soon...
