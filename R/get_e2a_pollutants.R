#' Function to get Air Quality e-Reporting (AQER) pollutants.  
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @examples 
#' 
#' # Get countries
#' data_pollutants <- get_e2a_pollutants()
#'
#' @export
get_e2a_pollutants <- function() {
  
  get_e2a_metadata() %>% 
    mutate(observed_property = basename(AirPollutantCode)) %>% 
    select(observed_property,
           variable = AirPollutant) %>% 
    distinct()
  
}
