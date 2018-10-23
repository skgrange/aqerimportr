#' Function to get Air Quality e-Reporting (AQER) sites/stations. 
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
#' # Get countries
#' data_sites <- get_e2a_sites()
#'
#' @export
get_e2a_sites <- function(as_smonitor = FALSE) {
  
  df <- get_e2a_metadata() %>% 
    distinct(Countrycode,
             AirQualityStation,
             AirQualityStationNatCode,
             AirQualityStationEoICode,
             AirQualityStationArea,
             AirQualityStationType,
             Longitude,
             Latitude,
             Altitude)
  
  if (as_smonitor) {
    
    names(df) <- str_to_underscore(names(df))
    names(df) <- ifelse(names(df) == "altitude", "elevation", names(df))
    
  }
  
  return(df)
  
}
