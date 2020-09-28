#' Function to format Air Quality e-Reporting (AQER) observational data for easy
#' use in the \strong{smonitor} framework. 
#' 
#' @param df Tibble from \code{\link{aqer_read_csv}}.
#' 
#' @param drop_sites Should bad sites be removed from the table? 
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
#' # Another example, this files has missing units
#' url <- "https://ereporting.blob.core.windows.net/downloadservice/AT_618_1437_2013_timeseries.csv"
#' data_aqer_clean_test <- aqer_read_csv(url) %>% 
#'   aqer_data_clean()
#' 
#' @export
aqer_data_clean <- function(df, drop_sites = FALSE) {
  
  # Correct names if needed
  if (names(df)[1] == "Countrycode") names(df) <- str_to_underscore(names(df))
  
  # Check for malformed observations
  # Variables to test
  variable_names <- c("countrycode", "air_pollutant_code", "datetime_begin")
  
  # Catch nas and push to characters
  df <- dplyr::mutate_at(df, variable_names, stringr::str_replace_na, "")
  
  # Test variables
  missing_values <- df %>% 
    select(!!variable_names) %>% 
    dplyr::mutate_all(~. == "") %>% 
    as.matrix() %>% 
    any()
  
  # Drop observations
  if (missing_values) {
    df <- df %>% 
      filter(countrycode != "", 
             air_quality_station_eo_i_code != "",
             air_pollutant_code != "",
             datetime_begin != "")
  }
  
  # Also check for incorrect air_pollutant_code/observed properties, causes
  # a lot of issues if incorrect
  df <- df %>% 
    mutate(observed_property = basename(air_pollutant_code),
           observed_property = suppressWarnings(as.integer(observed_property)))
  
  # Check for missing-ness
  missing_observed_properties <- anyNA(df$observed_property)
  
  # Drop observations
  if (missing_observed_properties) {
    df <- filter(df, !is.na(observed_property))
  }
  
  # Raise a warning for missing observations
  if (any(missing_values, anyNA(df$observed_property))) {
    warning(
      "Malformed observations detected, observations have been removed...",
      call. = FALSE,
      immediate. = TRUE
    )
  }
  
  # Clean the observations
  df <- aqer_data_table_formatter(df)
  
  # Drop bad sites
  if (drop_sites) {
    df <- filter(df, !stringr::str_detect(site, "^0|^1"))
  }
  
  return(df)
  
}


aqer_data_table_formatter <- function(df) {
  
  df %>% 
    mutate(
      # 2020 files only contain NAs in air_quality_station_eo_i_code
      air_quality_station_eo_i_code = if_else(
        is.na(air_quality_station_eo_i_code) & !is.na(air_quality_station), 
        air_quality_station, air_quality_station_eo_i_code
      ),
      air_quality_station_eo_i_code = stringr::str_remove(
        air_quality_station_eo_i_code, "^Station_"
      ),
      site = stringr::str_to_lower(air_quality_station_eo_i_code),
      site = stringr::str_remove_all(site, "^sta-|^sta\\.|^sta_"),
      site = stringr::str_remove_all(site, "^de_|^ie\\.|^gib_station_|^hr_doc_type_d_sta_"),
      # Swedish sites wrong
      site = if_else(
        stringr::str_detect(site, "^e") & countrycode == "SE", 
        stringr::str_replace(site, "^e", "se"), site
      ),
      # For wrong Turkey's sites
      site = if_else(
        stringr::str_detect(site, "^4") & countrycode == "TR",
        stringr::str_c("tr", site), site
      ),
      variable = stringr::str_to_lower(air_pollutant),
      variable = stringr::str_replace_all(variable, " ", "_"), 
      variable = if_else(variable == "nox_as_no2", "nox", variable),
      datetime_begin = lubridate::ymd_hms(datetime_begin, tz = "UTC"),
      datetime_end = lubridate::ymd_hms(datetime_end, tz = "UTC"),
      concentration = if_else(concentration == -9999, NA_real_, concentration),
      validity = if_else(validity == -99, NA_integer_, validity)
    ) %>% 
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
  
}
