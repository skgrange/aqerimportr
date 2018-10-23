#' Function to import Air Quality e-Reporting (AQER) observational data.  
#' 
#' @param url A vector of URLs from \code{\link{get_e2a_file_list}}.
#' 
#' @param as_smonitor Should the return be formatted for use within the 
#' \code{smonitor} framework? 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{get_e2a_file_list}}
#' 
#' @examples
#' 
#' # Example url
#' url <- "https://ereporting.blob.core.windows.net/downloadservice/GI_9_22755_2017_timeseries.csv"
#' data_aqer <- get_e2a_data(url)
#' 
#' @export
get_e2a_data <- function(url, as_smonitor = FALSE, verbose = FALSE)
  purrr::map_dfr(url, read_csv_e1a, as_smonitor = as_smonitor, verbose = verbose)


read_csv_e1a <- function(file, encoding = "UCS-2LE", as_smonitor = FALSE,
                         verbose = FALSE) {
  
  # The message
  if (verbose) {
    
    cat(
      "\r", 
      crayon::green(str_date_formatted()), 
      ": ", 
      crayon::green(basename(file)), 
      sep = ""
    )
    
  }
  
  # Static data types
  col_types <- c(
    "character", "character", "character", "character", "character", 
    "character", "character","character","character","character","character",
    "numeric", "character", "character", "character", "integer", "integer"
  )
  
  # Use base reader because of encoding issues when using readr
  df <- read.csv(
    file, 
    fileEncoding = encoding, 
    stringsAsFactors = FALSE,
    colClasses = col_types
  ) %>% 
    as_tibble()
  
  # Clean names
  names(df) <- str_to_underscore(names(df))
  
  # Clean table
  if (as_smonitor) df <- clean_e2a_data(df)
  
  return(df)
  
}


#' Function to format Air Quality e-Reporting (AQER) observational data for easy
#' use in the \strong{smonitor} framework. 
#' 
#' @param df Data frame from \code{\link{get_e2a_data}}
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{get_e2a_data}}
#' 
#' @examples
#' 
#' # Example url
#' url <- "https://ereporting.blob.core.windows.net/downloadservice/GI_9_22755_2017_timeseries.csv"
#' data_aqer <- get_e2a_data(url)
#' 
#' # Clean data
#' data_aqer_clean <- clean_e2a_data(data_aqer)
#' 
#' @export
clean_e2a_data <- function(df) {
  
  df %>% 
    mutate(site = stringr::str_to_lower(air_quality_station_eo_i_code),
           variable = stringr::str_to_lower(air_pollutant),
           observed_property = basename(air_pollutant_code),
           observed_property = as.integer(observed_property),
           datetime_begin = lubridate::ymd_hms(datetime_begin, tz = "UTC"),
           datetime_end = lubridate::ymd_hms(datetime_end, tz = "UTC"),
           concentration = ifelse(concentration == -9999, NA, concentration)) %>% 
    select(-countrycode,
           -namespace,
           -air_quality_network,
           -air_quality_station,
           -air_pollutant,
           -air_pollutant_code,
           -air_quality_station_eo_i_code) %>% 
    rename(sampling_point_process = sampling_process,
           period = averaging_time,
           date = datetime_begin,
           date_end = datetime_end,
           unit = unit_of_measurement,
           value = concentration) %>% 
    select(site, 
           variable, 
           observed_property,
           period,
           site,
           sample,
           sampling_point, 
           sampling_point_process,
           unit, 
           date,
           date_end, 
           validity,
           verification, 
           value)
  
}


str_to_underscore <- function(x) {
  
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub(":", "_", x, fixed = TRUE)
  x <- gsub("\\$", "_", x)
  x <- gsub(" ", "_", x)
  x <- gsub("__", "_", x)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  x <- tolower(x)
  x <- stringr::str_trim(x)
  return(x)
  
}
