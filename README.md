# **aqerimportr**

[![Build Status](https://travis-ci.org/skgrange/aqerimportr.svg?branch=master)](https://travis-ci.org/skgrange/aqerimportr)

**aqerimportr** is an R package to get and import European Air Quality e-Reporting (AQER) data. **aqerimportr** allows for efficient programmatic interaction with the portal located [here](http://discomap.eea.europa.eu/map/fme/AirQualityExport.htm). 

## Installation

To install the development version: 

```
# Install package
remotes::install_github("skgrange/aqerimportr")
```

## Usage

Most of **aqerimportr**'s functions use a `aqer_*` prefix. The more useful functions are: `aqer_metadata`, `aqer_read_csv`, `aqer_data_clean`, and `aqer_file_list`. 

### Importing metadata

A metadata table is updated daily and contains useful information about sites and measurements and can be accessed easily: 

```
# Load package
library(aqerimportr)

# Get dynamic metadata
data_metadata <- aqer_metadata()

# Get dynamic metadata with some cleaning
data_metadata_smonitor <- aqer_metadata(as_smonitor = TRUE)
```

### Importing observations

Importing observations is easy with `aqer_read_csv`. `aqer_read_csv` takes a vector of file names (or remote URLs) and returns a tibble: 

```
# Load data from a single file
data_single_file <- aqer_read_csv(
  "https://ereporting.blob.core.windows.net/downloadservice/AT_10_49487_2019_timeseries.csv" 
)
```

`aqer_read_csv` will faithfully return the format of the data file and therefore dates and some other variables will not be formatted. Use `aqer_data_clean` to make the table more suitable for analysis. There are a few ways to do this, but a good way is to immediately pipe the cleaning function:

```
# Load data from a single file and then clean a few things
data_single_file_clean <- aqer_read_csv(
  "https://ereporting.blob.core.windows.net/downloadservice/AT_10_49487_2019_timeseries.csv" 
) %>% 
  aqer_data_clean()
```

Generally, `aqer_read_csv` will be used in conjunction with `aqer_file_list` which queries the data portal for what data files are available. For example, let's get all validated (`e1a`) Danish data for 2017: 

```
# Build a file list
file_list_remote <- aqer_file_list(
  country = "dk",
  start = 2017, 
  end = 2017, 
  source = "e1a"
)

# Should be 145 files
length(file_list_remote)

# Get load all these files, will take a bit of time...
data_dk_all <- aqer_read_csv(file_list_remote, verbose = TRUE)

# Clean the observations
data_dk_all_clean <- aqer_data_clean(data_dk_all)

# Ready for some analysis...
```

So-called "near-real-time" observations are also available by using `e2a` as the data source too: 

```
# Build a file list for e2a data
file_list_remote_e2a <- aqer_file_list(
  country = "dk",
  start = 2018, 
  end = 2019, 
  source = "e2a"
)

# Get load all these files, beware that these data are not validated
data_dk_near_real_time <- aqer_read_csv(file_list_remote_e2a, verbose = TRUE)

# How up to date are these data? 
data_dk_near_real_time %>% 
  aqer_data_clean() %>% 
  dplyr::summarise(date_max = max(date))
  
# Should be within a few days...
```
