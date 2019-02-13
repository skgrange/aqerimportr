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
#' @param df Data frame from \code{\link{aqer_data}}.
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
  
  # Correct names if needed
  if (names(df)[1] == "Countrycode") names(df) <- str_to_underscore(names(df))
  
  # Check for malformed observations
  vector_to_test <- c(
    df$countrycode, 
    df$air_pollutant_code, 
    df$unit_of_measurement,
    df$datetime_begin
  )
  
  if (any(vector_to_test == "")) {
    
    # Drop observations
    df <- df %>% 
      filter(countrycode != "", 
             air_quality_station_eo_i_code != "",
             air_pollutant_code != "",
             unit_of_measurement != "",
             datetime_begin != "")
    
    # Raise a warning
    warning(
      "Malformed observations detected, observations removed...",
      call. = FALSE
    )
    
  }
  
  df <- df %>% 
    mutate(site = stringr::str_to_lower(air_quality_station_eo_i_code),
           site = stringr::str_remove(site, "^sta-"),
           variable = stringr::str_to_lower(air_pollutant),
           variable = stringr::str_replace_all(variable, " ", "_"), 
           variable = if_else(variable == "nox_as_no2", "nox", variable),
           observed_property = basename(air_pollutant_code),
           observed_property = as.integer(observed_property),
           datetime_begin = lubridate::ymd_hms(datetime_begin, tz = "UTC"),
           datetime_end = lubridate::ymd_hms(datetime_end, tz = "UTC"),
           concentration = if_else(concentration == -9999, NA_real_, concentration)) %>% 
    select(-countrycode,
           -namespace,
           -air_quality_network,
           -air_quality_station,
           -air_pollutant,
           -air_pollutant_code,
           -air_quality_station_eo_i_code) %>% 
    rename(feature_of_interest = sample,
           procedure = sampling_process,
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
           feature_of_interest,
           sampling_point, 
           procedure,
           unit, 
           date,
           date_end, 
           validity,
           verification, 
           value) %>% 
    dplyr::mutate_if(is.character, ~if_else(. == "", NA_character_, .))
  
  return(df)
  
}
