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
#' # Load metadata
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
  
  # if (as_smonitor)
  
  return(df)
  
}
