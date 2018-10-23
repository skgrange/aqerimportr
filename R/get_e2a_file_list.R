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
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector. 
#' 
#' @examples 
#' 
#' # Get all swiss files
#' get_e2a_file_list(country = "ch")
#' 
#' # Get all uk files for 2018
#' get_e2a_file_list(country = "gb", start = 2018, end = 2018)
#'
#'
#' @export
get_e2a_file_list <- function(country = "", pollutant = "", start = NA, end = NA, 
                              station = "", verbose = FALSE) {
  
  date_system <- lubridate::now()
  start <- ifelse(is.na(start), lubridate::year(date_system), start)
  end <- ifelse(is.na(end), lubridate::year(date_system), end)
  
  # Create string
  query_string <- build_e2a_file_query(
    country = stringr::str_to_upper(country), 
    pollutant = pollutant, 
    start = start, 
    end = end, 
    station = station
  )
  
  # Message
  if (verbose) message(str_date_formatted(), ": ", query_string, "...")
  
  # Get file list
  file_list <- purrr::map(query_string, readLines, warn = FALSE) %>% 
    purrr::flatten_chr()
  
  return(file_list)
  
}


build_e2a_file_query <- function(country, pollutant, start, end, station) {
  
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
    "&Samplingpoint=&Source=All&Output=TEXT&UpdateDate="
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
