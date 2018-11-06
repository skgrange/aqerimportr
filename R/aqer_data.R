#' Function to import Air Quality e-Reporting (AQER) observational data.  
#' 
#' @param url A vector of URLs from \code{\link{aqer_file_list}}.
#' 
#' @param encoding Encoding of \code{url}. 
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
#' @seealso \code{\link{aqer_file_list}}
#' 
#' @examples
#' 
#' # Example url
#' url <- "https://ereporting.blob.core.windows.net/downloadservice/GI_9_22755_2017_timeseries.csv"
#' data_aqer <- aqer_data(url)
#' 
#' @export
aqer_data <- function(url, encoding = "UCS-2LE", as_smonitor = FALSE, 
                      verbose = FALSE) {
 
  # Use non-vectorised function
  purrr::map_dfr(
    url, 
    aqer_read_csv_worker, 
    encoding = encoding,
    as_smonitor = as_smonitor, 
    verbose = verbose
  ) 
  
}


#' Function to format Air Quality e-Reporting (AQER) observational data for easy
#' use in the \strong{smonitor} framework. 
#' 
#' @param df Data frame from \code{\link{aqer_data}}
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{aqer_data}}
#' 
#' @examples
#' 
#' # Example url
#' url <- "https://ereporting.blob.core.windows.net/downloadservice/GI_9_22755_2017_timeseries.csv"
#' data_aqer <- aqer_data(url)
#' 
#' # Clean data
#' data_aqer_clean <- aqer_data_clean(data_aqer)
#' 
#' @export
aqer_data_clean <- function(df) {
  
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
