#' Functions to read Air Quality e-Reporting (AQER) observational data files.
#' 
#' \code{aqer_read_csv_worker} is the unvectorised worker function which can be
#' used other situations other than just reading many files, such as exporting 
#' data in parallel. 
#' 
#' @param file Vector of file names to read or a character vector to be parsed. 
#' 
#' @param encoding Encoding of \code{file}. 
#' 
#' @param as_smonitor Should the return be formatted for use within the 
#' \code{smonitor} framework? 
#' 
#' @param timeout When file is a URL, how many seconds should be used as a 
#' timeout?
#' 
#' @param warn Should the function raise warnings? 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
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
                          timeout = 30, warn = TRUE, verbose = FALSE) {
  
  # Vectorise function
  purrr::map_dfr(
    file,
    aqer_read_csv_worker,
    encoding = encoding,
    as_smonitor = as_smonitor,
    timeout = timeout,
    warn = warn,
    verbose = verbose
  )
  
}


#' @rdname aqer_read_csv
#' @export
aqer_read_csv_worker <- function(file, encoding, as_smonitor, timeout, warn, 
                                 verbose) {
  
  # The message
  if (verbose) message(date_message(), file)
  
  # If a url, use httr to get contents
  if (stringr::str_detect(file[1], "^http:|^https:")) {
    file <- get_url_contents(file, timeout = timeout)
  }

  # Try to load data with readr first
  df <- aqer_read_csv_worker_readr_safe(file)
  
  # Try with base read.table and different encoding
  if (nrow(df) == 0) {
    
    if (warn) {
      warning(
        date_message(), 
        basename(file), 
        " has an odd encoding...",
        call. = FALSE,
        immediate. = TRUE
      )
    }
    
    df <- aqer_read_csv_worker_base(file, encoding)
    
  }
  
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


aqer_read_csv_col_types <- function() {
  
  readr::cols(
    Countrycode = readr::col_character(),
    Namespace = readr::col_character(),
    AirQualityNetwork = readr::col_character(),
    AirQualityStation = readr::col_character(),
    AirQualityStationEoICode = readr::col_character(),
    SamplingPoint = readr::col_character(),
    SamplingProcess = readr::col_character(),
    Sample = readr::col_character(),
    AirPollutant = readr::col_character(),
    AirPollutantCode = readr::col_character(),
    AveragingTime = readr::col_character(),
    Concentration = readr::col_double(),
    UnitOfMeasurement = readr::col_character(),
    DatetimeBegin = readr::col_character(),
    DatetimeEnd = readr::col_character(),
    Validity = readr::col_integer(),
    Verification = readr::col_integer()
  )
  
}


aqer_read_csv_worker_readr <- function(file) {
  
  suppressWarnings(
    readr::read_csv(file, col_types = aqer_read_csv_col_types())
  )
  
}


aqer_read_csv_worker_readr_safe <- purrr::possibly(
  aqer_read_csv_worker_readr,
  otherwise = tibble(),
  quiet = TRUE
)


aqer_read_csv_worker_base <- function(file, encoding) {
  
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
  
  return(df)
  
}


get_url_contents <- function(url, timeout = 60) {
  
  # Get page
  response <- httr::GET(url, httr::timeout(timeout))
  
  # Check response
  httr::stop_for_status(response)
  
  # stringi::stri_enc_detect(httr::content(response, "raw"))
  
  # Decode content
  x <- httr::content(response, "text", encoding = "UTF-8")
  
  # httr is not handling some special characters
  if (is.na(x)) x <- stringi::stri_encode(response$content, "UTF-16")
  
  return(x)
  
}
