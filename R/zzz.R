#' Function to squash R check's global variable notes. 
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(
    ".", "air_quality_station_eo_i_code", "air_pollutant", "air_pollutant_code", 
    "observed_property", "datetime_begin", "datetime_end", "concentration",
    "countrycode", "namespace", "air_quality_network", "air_quality_station",
    "sampling_process", "averaging_time", "unit_of_measurement", "site", 
    "variable", "validity", "verification", "value", "Countrycode", "country",
    "period", "sampling_point", "sampling_point_process", "unit", "date_end",
    "AirPollutantCode", "AirPollutant", "AirQualityStation", 
    "AirQualityStationNatCode", "AirQualityStationEoICode", "AirQualityStationArea",
    "AirQualityStationType", "Longitude", "Latitude", "Altitude", "altitude",
    "air_quality_station_type", "air_quality_station_area", 
    "air_quality_station_nat_code", "projection", "feature_of_interest", 
    "procedure", "building_distance", "kerb_distance", "country_iso_code",
    "observation_date_begin", "observation_date_end", "timezone"
  )
  
  # Squash the note
  utils::globalVariables(variables)
  
}
