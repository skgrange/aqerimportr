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
#' @examples
#' 
#' # Load aqer metadata
#' data_metadata <- aqer_metadata()
#'
#' @export
aqer_metadata <- function(file = NA, as_smonitor = FALSE) {
  
  # The default
  if (is.na(file[1]))
    file <- "https://ereporting.blob.core.windows.net/downloadservice/metadata.csv"
  
  # Get data as text
  text <- readLines(file, warn = FALSE)
  
  # Parse 
  suppressWarnings(
    df <- text %>% 
      stringr::str_c(collapse = "\n") %>% 
      readr::read_csv(na = c("-999", ""), progress = FALSE)
  )
  
  if (as_smonitor) {
    
    # Underscore the name
    names(df) <- str_to_underscore(names(df))
    
    # Use smonitor nomenclature
    df <- aqer_metadata_clean_for_smonitor(df)
    
  }
  
  return(df)
  
}


aqer_metadata_clean_for_smonitor <- function(df) {
  
  df %>% 
    mutate(site = stringr::str_to_lower(air_quality_station_eo_i_code),
           variable = stringr::str_to_lower(air_pollutant),
           variable = stringr::str_replace_all(variable, " ", "_"), 
           variable = if_else(variable == "nox_as_no2", "nox", variable),
           observed_property = basename(air_pollutant_code),
           observed_property = as.integer(observed_property),
           countrycode = stringr::str_to_lower(countrycode)) %>% 
    rename(country_iso = countrycode,
           elevation = altitude,
           site_type = air_quality_station_type,
           site_area = air_quality_station_area,
           national_code = air_quality_station_nat_code,
           eu_code = air_quality_station,
           network = air_quality_network) %>% 
    select(-air_pollutant,
           -air_pollutant_code,
           -projection)
    
}
