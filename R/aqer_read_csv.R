#' Function to read an Air Quality e-Reporting (AQER) observational data file.
#' 
#' @param file Vector of file names to read. 
#' 
#' @param encoding Encoding of \code{file}. 
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
#' @note To-do: Enhance to use \strong{readr}. 
#' 
#' @export
aqer_read_csv <- function(file, encoding = "UCS-2LE", as_smonitor = FALSE,
                          verbose = FALSE) {
  
  purrr::map_dfr(
    file,
    aqer_read_csv_worker,
    encoding = encoding,
    as_smonitor = as_smonitor,
    verbose = verbose
  )
  
}


aqer_read_csv_worker <- function(file, encoding, as_smonitor, verbose) {
  
  # The message
  if (verbose) {
    
    cat(
      "\r", 
      crayon::green(date_message()), 
      crayon::green(basename(file)), 
      sep = ""
    )
    
  }
  
  # Static data types
  col_types <- c(
    "character", "character", "character", "character", "character", 
    "character", "character","character","character","character","character",
    "numeric", "character", "character", "character", "integer", "integer"
  )
  
  # Use base reader because of encoding issues when using readr
  df <- read.csv(
    file, 
    fileEncoding = encoding, 
    colClasses = col_types
  ) %>% 
    as_tibble()
  
  # Clean names
  names(df) <- str_to_underscore(names(df))
  
  # Clean table
  if (as_smonitor) df <- aqer_data_clean(df)
  
  return(df)
  
}


str_to_underscore <- function(x) {
  
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub(":", "_", x, fixed = TRUE)
  x <- gsub("\\$", "_", x)
  x <- gsub(" ", "_", x)
  x <- gsub("__", "_", x)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  x <- tolower(x)
  x <- stringr::str_trim(x)
  return(x)
  
}
