library("dplyr")


get_flight_data <- function(year, icao = NULL) {
  if (!is.numeric(year) || length(year) != 1 || year < 2000 || year > 2023) {
    stop(paste("Invalid year argument:", year))
  }
  
  if (!is.null(icao) && !is.character(icao)) {
    stop(paste("Invalid type for icao argument:", icao))
  }
  
  load(paste0("data/flights/bfd_", year, ".rdata"))
  flights <- bfd %>%
    select(-c("route", "real_depart", "real_arrival", "delay_arrival", "real_flight_length"), -starts_with("arrival_"), -starts_with("outlier_")) %>%
    select(where(~ !all(is.na(.))))
  
  if (!is.null(icao)) {

    unique_icao_codes <- unique(flights$depart)
    if (length(icao) == 1 && icao %in% unique_icao_codes) {
      flights <- flights %>%
        filter(depart == icao) %>%
        select(-depart, -depart_lon, -depart_lat, -depart_elevation)
    }
    else {
      stop(paste("Invalid icao value:", icao))
    }

  }
  
  flights <- flights %>%
    filter(status != "CANCELADO") %>%
    mutate(target = delay_depart > 15) %>%
    select(-status, -delay_depart)
  
  # TODO: is any of these factors ordered?
  flights$arrival <- as.factor(flights$arrival)
  flights$company <- as.factor(flights$company)
  flights$flight <- as.factor(flights$flight)
  flights$di <- as.factor(flights$di)
  flights$type <- as.factor(flights$type)
  flights$depart_sky_coverage <- as.factor(flights$depart_sky_coverage)
  flights$target <- as.factor(flights$target)
  
  return(flights)
}
