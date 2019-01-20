#' Function to get remote file lists containing Air Quality e-Reporting (AQER)
#' observational data.  
#' 
#' @param country Country code to filter file list to. 
#' 
#' @param pollutant Pollutant code to filter file list to. 
#' 
#' @param start Start year to filter file list to. 
#' 
#' @param end End year to filter file list to. 
#' 
#' @param station Station code to filter file list to. 
#' 
#' @param source What data source should be used? Can be either \code{"all"},
#' \code{"e1a"} for validated and reported data, or \code{"e2a"} for near real 
#' time data which are not validated data. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector. 
#' 
#' @examples 
#' 
#' # Get all swiss files
#' aqer_file_list(country = "ch")
#' 
#' # Get all validated/reported for Switzerland
#' aqer_file_list(country = "ch", source = "e1a")
#' 
#' # Get all uk files for 2018
#' aqer_file_list(country = "gb", start = 2018, end = 2018)
#'
#'
#' @export
aqer_file_list <- function(country = "", pollutant = "", start = 2013, end = NA, 
                           station = "", source = "all", verbose = FALSE) {
  
  # Catch NA inputs
  date_system <- lubridate::now()
  start <- if_else(is.na(start), lubridate::year(date_system), as.numeric(start))
  end <- if_else(is.na(end), lubridate::year(date_system), as.numeric(end))
  
  # Check source input
  source <- stringr::str_to_lower(source)
  
  if (!source %in% c("all", "e1a", "e2a"))
    stop("`source` must be one of `all`, `e1a`, or `e2a`...", call. = FALSE)
  
  # Create string
  query_string <- aqer_file_query(
    country = stringr::str_to_upper(country), 
    pollutant = pollutant, 
    start = start, 
    end = end, 
    station = station,
    source = source
  )
  
  # Message
  if (verbose) message(date_message(), query_string, "...")
  
  # Get file list
  file_list <- purrr::map(query_string, readLines, warn = FALSE) %>% 
    purrr::flatten_chr()
  
  return(file_list)
  
}


aqer_file_query <- function(country, pollutant, start, end, station, source) {

  stringr::str_c(
    "http://fme.discomap.eea.europa.eu/fmedatastreaming/AirQualityDownload/AQData_Extract.fmw?CountryCode=", 
    country,
    "&CityName=&Pollutant=",
    pollutant,
    "&Year_from=",
    start,
    "&Year_to=",
    end, 
    "&Station=",
    station,
    "&Samplingpoint=&Source=",
    source, 
    "&Output=TEXT&UpdateDate="
  )
  
}


str_date_formatted <- function(date = NA, time_zone = TRUE, 
                               fractional_seconds = TRUE) {
  
  # Get date if not supplied
  if (is.na(date)[1]) date <- lubridate::now(tz = Sys.timezone())
  
  # Format string
  format_date <- ifelse(
    fractional_seconds, 
    "%Y-%m-%d %H:%M:%OS3", 
    "%Y-%m-%d %H:%M:%S"
  )
  
  # Format
  x <- format(date, format = format_date, usetz = time_zone)
  
  return(x)
  
}


date_message <- function() stringr::str_c(str_date_formatted(), ": ")
