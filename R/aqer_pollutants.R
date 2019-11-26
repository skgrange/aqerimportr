#' Function to get Air Quality e-Reporting (AQER) pollutants.  
#' 
#' @param file File name or URL of metadata file. Do not use for "live" version. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{aqer_metadata}}
#' 
#' @examples 
#' 
#' # Get pollutants
#' data_pollutants <- aqer_pollutants()
#'
#' @export
aqer_pollutants <- function(file = NA) {
  
  # Use the metadata reader
  aqer_metadata(file = file, as_smonitor = TRUE) %>% 
    distinct(observed_property,
             variable) %>% 
    arrange(observed_property)
  
}
