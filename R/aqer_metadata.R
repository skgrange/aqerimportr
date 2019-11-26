#' Function to import Air Quality e-Reporting (AQER) metadata. 
#' 
#' @param file File name or URL of metadata file. Do not use for "live" version. 
#' 
#' @param as_smonitor Should the return be formatted for use within the 
#' \code{smonitor} framework? 
#'
#' @author Stuart K. Grange
#'
#' @return Tibble.
#' 
#' @seealso \href{http://discomap.eea.europa.eu/map/fme/AirQualityExport.htm}{AQER download portal}
#' 
#' @examples
#' 
#' # Load aqer metadata
#' data_metadata <- aqer_metadata()
#' 
#' # Load aqer metadata and clean a bit
#' data_metadata_clean <- aqer_metadata(as_smonitor = TRUE)
#'
#' @export
aqer_metadata <- function(file = NA, as_smonitor = FALSE) {
  
  # The default
  if (is.na(file[1])) {
    file <- "http://discomap.eea.europa.eu/map/fme/metadata/PanEuropean_metadata.csv"
  }
  
  # Read data
  df <- readr::read_delim(
    file, 
    na = c("-999", ""), 
    delim = "\t",
    col_types = readr::cols(
      ObservationDateBegin = readr::col_character(),
      ObservationDateEnd = readr::col_character()
    ),
    progress = FALSE
  )
  
  if (as_smonitor) {
    
    # Load look-up table for decoding pollutant code
    df_variables <- read_observed_properties() %>% 
      select(observed_property,
             variable)
    
    # Use smonitor nomenclature
    df <- aqer_metadata_clean_for_smonitor(df, df_variables)
    
  }
  
  return(df)
  
}


aqer_metadata_clean_for_smonitor <- function(df, df_variables) {
  
  df <- df %>% 
    purrr::set_names(str_to_underscore(names(.))) %>% 
    mutate(observed_property = basename(air_pollutant_code),
           observed_property = as.integer(observed_property)) %>% 
    left_join(df_variables, by = "observed_property") %>% 
    mutate(site = stringr::str_to_lower(air_quality_station_eo_i_code),
           site = stringr::str_remove(site, "^sta-"),
           site_area = stringr::str_replace_all(air_quality_station_area, "-", "_"),
           timezone = basename(timezone),
           observation_date_begin = lubridate::ymd_hms(observation_date_begin, tz = "UTC"),
           observation_date_end = lubridate::ymd_hms(observation_date_end, tz = "UTC")) %>% 
    rename(country_iso_code = countrycode,
           elevation = altitude,
           site_type = air_quality_station_type,
           national_code = air_quality_station_nat_code,
           eu_code = air_quality_station,
           network = air_quality_network,
           eoi_code = air_quality_station_eo_i_code,
           distance_building = building_distance,
           distance_kerb = kerb_distance) %>% 
    select(-air_pollutant_code,
           -projection,
           -air_quality_station_area) %>% 
    dplyr::mutate_if(
      ~is.double(.) & !lubridate::is.POSIXct(.), 
      ~if_else(. %in% aqer_metadata_na_strings(), NA_real_, .)
    ) %>% 
    dplyr::mutate_if(
      is.integer, ~if_else(. %in% aqer_metadata_na_strings(), NA_integer_, .)
    ) %>% 
    select(country_iso_code,
           site,
           variable,
           everything())
  
  # Check join
  if (anyNA(df$variable)) {
    warning("There are missing variables...", call. = FALSE)
  }
  
  # Check sites
  if (anyNA(df$site)) {
    warning("There are missing sites...", call. = FALSE)
  }
  
  return(df)
  
}


aqer_metadata_na_strings <- function() c(-9999, 9999, -9900, -99)
