#' Function to get Air Quality e-Reporting (AQER) pollutants.  
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @examples 
#' 
#' # Get pollutants
#' data_pollutants <- aqer_pollutants()
#'
#' @export
aqer_pollutants <- function() {
  
  aqer_metadata() %>% 
    mutate(observed_property = basename(AirPollutantCode)) %>% 
    select(observed_property,
           variable = AirPollutant) %>% 
    distinct()
  
}
