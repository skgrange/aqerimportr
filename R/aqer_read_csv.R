#' Functions to read Air Quality e-Reporting (AQER) observational data files.
#' 
#' \code{aqer_read_csv_worker} is the unvectorised worker function which can be
#' used other situations other than just reading many files, such as exporting 
#' data in parallel. 
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
#' @note To-do: Enhance to use \strong{readr}, but need to address encoding 
#' issues first. 
#' 
#' @seealso \code{\link{aqer_file_list}}
#' 
#' @examples
#' 
#' # Example url
#' url <- "https://ereporting.blob.core.windows.net/downloadservice/GI_9_22755_2017_timeseries.csv"
#' data_aqer <- aqer_read_csv(url)
#' 
#' # Correct some data types
#' data_aqer_clean <- aqer_read_csv(url, as_smonitor = TRUE)
#' 
#' @export
aqer_read_csv <- function(file, encoding = "UCS-2LE", as_smonitor = FALSE,
                          verbose = FALSE) {
  
  # Vectorise function
  purrr::map_dfr(
    file,
    aqer_read_csv_worker,
    encoding = encoding,
    as_smonitor = as_smonitor,
    verbose = verbose
  )
  
}


#' @rdname aqer_read_csv
#' @export
aqer_read_csv_worker <- function(file, encoding, as_smonitor, verbose) {
  
  # The message
  if (verbose) message(date_message(), basename(file))
  
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
  
  # Clean table for smonitor
  if (as_smonitor) {
    
    # Clean names
    names(df) <- str_to_underscore(names(df))
    
    # Clean table
    df <- aqer_data_clean(df)
    
  }
  
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
  x <- stringr::str_to_lower(x)
  x <- stringr::str_trim(x)
  return(x)
  
}
