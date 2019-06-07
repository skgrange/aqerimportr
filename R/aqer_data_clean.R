#' Function to format Air Quality e-Reporting (AQER) observational data for easy
#' use in the \strong{smonitor} framework. 
#' 
#' @param df Tibble from \code{\link{aqer_read_csv}}.
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{aqer_read_csv}}
#' 
#' @examples
#' 
#' # Example url
#' url <- "https://ereporting.blob.core.windows.net/downloadservice/GI_9_22755_2017_timeseries.csv"
#' data_aqer <- aqer_read_csv(url)
#' 
#' # Clean data
#' data_aqer_clean <- aqer_data_clean(data_aqer)
#' 
#' # Another example, this files has missing units and will raise a warning
#' url <- "https://ereporting.blob.core.windows.net/downloadservice/AT_618_1437_2013_timeseries.csv"
#' data_aqer_clean_test <- aqer_read_csv(url) %>% 
#'   aqer_data_clean()
#' 
#' @export
aqer_data_clean <- function(df) {
  
  # Correct names if needed
  if (names(df)[1] == "Countrycode") names(df) <- str_to_underscore(names(df))
  
  # Check for malformed observations
  # Variables to test
  variable_names <- c(
    "countrycode", "air_pollutant_code", "unit_of_measurement", "datetime_begin"
  )
  
  # Catch nas and push to characters
  df <- dplyr::mutate_at(df, variable_names, stringr::str_replace_na, "")
  
  # Test variables
  missing_values <- df %>% 
    select(!!variable_names) %>% 
    dplyr::mutate_all(~. == "") %>% 
    as.matrix() %>% 
    any()
  
  if (missing_values) {
    
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
    rename(period = averaging_time,
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
           sampling_process,
           unit, 
           date,
           date_end, 
           validity,
           verification, 
           value) %>% 
    dplyr::mutate_if(is.character, ~if_else(. == "", NA_character_, .))
  
  return(df)
  
}
