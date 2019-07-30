#' Get a single time series 
#' 
#' A wrapper for getCorrectedTimeSeries, but with the option to 
#' rename the data column in the resulting data
#' 
#' @param tsID the time series unique identifier
#' @param start the starting date of interest as a string in the form YYYY-MM-DD
#' @param end the ending date of itnerest as a string in the form YYYY-MM-DD
#' @param name the name to use in the data column
#' 
#' @importFrom magrittr %>%
#' 
#' @export
#' 

getSingleTS <- function(tsID, start, end, name) {
  
  ts_data <- getCorrectedData(tsID, start, end) %>%
    dplyr::rename(!!name := corrected)
  
}

#' Get a single time series, separating the query into 3 year chunks
#' 
#' @param tsID the time series unique identifier
#' @param start the starting date of interest as a string in the form YYYY-MM-DD
#' @param end the ending date of itnerest as a string in the form YYYY-MM-DD
#' @param name the name to use in the data column
#' 
#' @importFrom magrittr %>%
#' 
#' @export
#' 

getSingleTSSplit <- function(tsID, start, end, name) {
  
  dates <- as.Date(c(start, end))
  if(any(is.na(dates))) {
    stop("Invalid date range")
  }
  t <- difftime(dates[2], dates[1], units = "weeks")
  if(t > 156) {
    by_t <- as.difftime(156, units = "weeks")
    start <- seq.Date(dates[1], dates[2], by = by_t)
    end <- c(start[-1], dates[2])
    if(start[length(start)] == end[length(end)]) {
      start <- start[-length(start)]
      end <- end[-length(end)]
    }
    start <- as.character(start)
    end <- as.character(end)
    chunks <- list()
    for(i in 1:length(start)) {
      chunks[[i]] <- geSingleTS(tsID, c(start[i], end[i]), name)
    }
    output_data <- chunks %>%
      purrr::reduce(rbind)
  } else {
    output_data <- getSingleTS(tsID, as.character(dates[1]), 
                                 as.character(dates[2]), name)
  }
  return(output_data)
}

#' Get multiple time series joined into one data frame
#' 
#' @param tsID the time series unique identifiers for each series
#' @param start the starting date of interest as a string in the form YYYY-MM-DD
#' @param end the ending date of itnerest as a string in the form YYYY-MM-DD
#' @param names the names to use for each series, corresponding to tsID
#' @param time_zone the time zone to use for output
#' 
#' @importFrom magrittr %>%
#' 
#' @export
#' 

getTimeSeries <- function(tsID, start, end, names, time_zone, 
                            interval = c("All", "Hourly", "30-minute", "15-minute", "5-minute")) {
  
  ts_list <- list()
  for(i in 1:length(tsID)) {
    ts_list[[i]] <- getSingleTSSplit(tsID[i], start, end, names[i])
  }
  out <- ts_list %>%
    purrr::reduce(dplyr::full_join, by = "datetime") %>%
    dplyr::arrange(datetime)
  
  if(interval != "All") {
    if(interval == "Hourly")
      date_seq <- seq.POSIXt(as.POSIXct(start), as.POSIXct(end), by = "1 hour")
    else if(interval == "30-minute")
      date_seq <- seq.POSIXt(as.POSIXct(start), as.POSIXct(end), by = "30 min")
    else if(interval == "15-minute")
      date_seq <- seq.POSIXt(as.POSIXct(start), as.POSIXct(end), by = "15 min")
    else if(interval == "5-minute")
      date_seq <- seq.POSIXt(as.POSIXct(start), as.POSIXct(end), by = "5 min")
    
    dt <- data.frame(datetime = date_seq)
    out <- dplyr::left_join(dt, out)
  }
  
  out <- out %>%
    dplyr::mutate(datetime = as.character(datetime, format = "%Y-%m-%d %H:%M %Z", tz = time_zone))
  return(out)
  
}

#' Authenticate API connection and get multiple time series joined into one data frame
#' 
#' For use in asynchronous programming for the shiny application
#' 
#' @param tsID the time series unique identifiers for each series
#' @param start the starting date of interest as a string in the form YYYY-MM-DD
#' @param end the ending date of itnerest as a string in the form YYYY-MM-DD
#' @param names the names to use for each series, corresponding to tsID
#' @param time_zone the time zone to use for output
#' @param id the AQUARIUS API username to authenticate with
#' @param pw the AQUARIUS API encrypted password to authenticate with
#' 
#' @export
#' 

getTimeSeriesConnect <- function(tsID, start, end, names, time_zone, 
                                 interval = c("All", "Hourly", "30-minute", "15-minute", "5-minute"),
                                 id, pw) {
  
  tkn <- retryToken(id, pw)
  out <- getTimeSeries(tsID, date_range, names, time_zone, interval)
  
}