#' Base function to get data or metadata from the AQ Publish API
#' 
#' Given a service request and a set of query parameters and values, return the 
#' output from the API
#' 
#' @param resource the resource to retrieve - such as raw data (GetTimeSeriesRawData),
#' or corrections (GetCorrectionList). See AQ API documentation for details on available
#' resources
#' @param parameters a character vector the names of the parameters to query by 
#' @param values the values corresponding to each parameter
#' @param host the host to use - could be a test server for development, or the production
#' server for running real queries
#' 
#' @return the response from the query
#' 
#' @export
#' 

genericAQ <- function(resource, parameters, values, host="ts-api.nwis.usgs.gov") {
  
  if(length(parameters) != length(values))
    stop("parameters not the same length as values")
  
  url <- paste0("http://", host, "/AQUARIUS/Publish/V2/" , resource, "?")
  q <- as.list(values)
  names(q) <- parameters
  resp <- httr::GET(url, query = q)
  cont <- httr::content(resp, "text")
  output <- jsonlite::fromJSON(cont)
  return(output)
}

#' Authenticate to the AQ API
#' 
#' Use the username and encrypted password to authenticate to the AQUARIUS API
#' 
#' @param id the username to use for authentication
#' @param pw the encrypted password to use for authentication
#' 
#' @return the response from the authentication
#' 
#' @export
#' 

getToken <- function(id, pw) {
  url <- paste0("http://ts-api.nwis.usgs.gov/AQUARIUS/Publish/V2/session?username=",
                id, "&encryptedPassword=", pw)
  a <- httr::POST(url)
  return(a)
}

#' Test to see if the API connection is working
#' 
#' @return a logical indicating whether a call to GetUnitList worked correctly
#' 
#' @export
#' 

testToken <- function() {
  test <- try({genericAQ("GetUnitList", "GroupIdentifier", "Capacitance")}, silent=TRUE)
  if(class(test) != "try-error") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Retry authentication
#' 
#' Retry authentication using the username and password until it works, or a maximum
#' of 5 times
#' 
#' @param id the username to use for authentication
#' @param pw the encrypted password to use for authentication
#' 
#' @return the response from \code{getToken} from the successful authentication or the
#' failed response from the final try
#' 
#' @export
#' 

retryToken <- function(id, pw) {
  tkn <- getToken(id, pw)
  i <- 0
  while(!(testToken()) & i < 5) {
    Sys.sleep(3)
    tkn <- getToken(id, pw)
    i <- i + 1
  }
  return(tkn)
}

#' Get time series descriptions, including unique identifiers
#' 
#' Get a list of time series descriptions based on the location identifier and parameter name
#' 
#' @param location the location identifier
#' @param parameter the parameter name
#' 
#' @return a data frame of time series descriptions matching the given location and parameter,
#' including a column giving the unique identifier for each time series
#' 
#' @export
#' 

getTimeSeriesIDs <- function(location, parameter) {
  serviceRequest <- "GetTimeSeriesDescriptionList"
  parameters <- c("LocationIdentifier", "Parameter", "Publish", "ComputationIdentifier", "ComputationPeriodIdentifier")
  values <- c(location, parameter, "true", "Instantaneous", "Points")
  raw <- genericAQ(serviceRequest, parameters, values)
  out <- raw$TimeSeriesDescriptions
  return(out)
}

#' Get a list of approval transactions
#' 
#' Get a data frame with details about when approval levels were changed for a given time series
#' in a particular date range
#' 
#' @param tsID the time series unique identifier
#' @param start the starting date of interest, as a string in the form YYYY-MM-DD
#' @param end the ending date of interest, as a string in the format YYYY-MM-DD
#' 
#' @return a data frame of approval transactions
#' 
#' @export
#' 

getApprovalList <- function(tsID, start, end) {
  serviceRequest <- "GetApprovalsTransactionList"
  parameters <- c("TimeSeriesUniqueId", "QueryFrom", "QueryTo")
  values <- c(tsID, start, end)
  raw <- genericAQ(serviceRequest, parameters, values)
  out <- raw$ApprovalsTransactions
  out$DateAppliedUtc <- as.POSIXct(out$DateAppliedUtc, format="%Y-%m-%dT%H:%M", tz = "GMT")
  out$StartTime <- as.POSIXct(out$StartTime, format="%Y-%m-%dT%H:%M")
  out$EndTime <- as.POSIXct(out$EndTime, format="%Y-%m-%dT%H:%M")
  return(out)
}

#' Get raw (uncorrected) time series data
#' 
#' Return a time series of data from AQUARIUS without any corrections applied
#' 
#' @param tsID the time series unique identifier
#' @param start the starting date of interest as a string in the form YYYY-MM-DD
#' @param end the ending date of interest as a string in the form YYYY-MM-DD
#' 
#' @return a data frame of the raw time series data. If no data is
#' found for the time series and date range, return a data frame with no columns
#' or rows.
#' 
#' @export
#' 

getRawData <- function(tsID, start, end) {
  serviceRequest <- "GetTimeSeriesRawData"
  parameters <- c("TimeSeriesUniqueId", "QueryFrom", "QueryTo", "GetParts", "UtcOffset")
  values <- c(tsID, start, end, "PointsOnly", "0")
  raw <- genericAQ(serviceRequest, parameters, values)
  out <- raw$Points
  if(length(out) == 0) {
    message("No data found")
    out <- data.frame()
  } else {
    out[,2] <- out[,2][,1]
    out$Timestamp <- as.POSIXct(out$Timestamp, format="%Y-%m-%dT%H:%M:%S", tz="GMT")
    names(out) <- c("datetime", "raw")
    out <- stats::na.omit(out)
  }
  return(out)
}

#' Get corrected data for the time series
#' 
#' Get time series data from AQUARIUS with all corrections applied
#' 
#' @param tsID the time series unique identifier
#' @param start the starting date of interest as a string in the form YYYY-MM-DD
#' @param end the ending date of interest as a string in the form YYYY-MM-DD
#' 
#' @return a data frame of corrected data for the time series. If no data is
#' found for the time series and date range, return a data frame with no columns
#' or rows.
#' 
#' @export
#' 

getCorrectedData <- function(tsID, start, end) {
  serviceRequest <- "GetTimeSeriesCorrectedData"
  parameters <- c("TimeSeriesUniqueId", "QueryFrom", "QueryTo", "GetParts", "UtcOffset")
  values <- c(tsID, start, end, "PointsOnly", "0")
  corrected <- genericAQ(serviceRequest, parameters, values)
  out <- corrected$Points
  if(length(out) == 0) {
    message("No data found")
    out <- data.frame()
  } else {
    out[,2] <- out[,2][,1]
    out$Timestamp <- as.POSIXct(out$Timestamp, format="%Y-%m-%dT%H:%M:%S", tz="GMT")
    names(out) <- c("datetime", "corrected")
    out <- stats::na.omit(out)
  }
  return(out)
}

#' Get a list of USGS Multi-point corrections 
#' 
#' @param tsID the time series unique identifier
#' @param start the start date of interest as a string in the form YYYY-MM-DD
#' @param end the end date of interest as a string in the form YYYY-MM-DD
#' 
#' @return a list of multiPointCorrection objects.
#' 
#' @seealso [multiPointCorrection()] for details on the multiPointCorrection class
#' 
#' @importFrom magrittr %>%
#' 
#' @export
#' 

getCorrections <- function(tsID, start, end) {
  
  Type <- StartTime <- EndTime <- endNull <- startNull <- . <- ".dplyr.var"
  
  parameters <- c("TimeSeriesUniqueId", "QueryFrom", "QueryTo")
  values <- c(tsID, start, end)
  temp <- genericAQ("GetCorrectionList", parameters, values)
  aq_multipoint <- temp$Corrections %>%
    dplyr::filter(Type == "USGSMultiPoint") %>%
    dplyr::mutate(
      StartTime = lubridate::ymd_hms(StartTime),
      EndTime = lubridate::ymd_hms(EndTime)
    )
  
  aq_multipoint$endNull <- lapply(aq_multipoint$Parameters$EndShiftPoints, is.null)
  aq_multipoint$startNull <- lapply(aq_multipoint$Parameters$StartShiftPoints, is.null)
  aq_multipoint <- aq_multipoint %>%
    dplyr::filter(endNull == FALSE, startNull == FALSE) %>%
    dplyr::select(-startNull, -endNull)
  
  startShiftPoints <- aq_multipoint$Parameters$StartShiftPoints
  startValues <- purrr::map(startShiftPoints, "Value")
  startOffsets <- purrr::map(startShiftPoints, "Offset")
  endShiftPoints <- aq_multipoint$Parameters$EndShiftPoints
  endValues <- purrr::map(endShiftPoints, "Value")
  endOffsets <- purrr::map(endShiftPoints, "Offset")
  
  set <- aq_multipoint$Parameters$UsgsType %>%
    unlist() %>%
    gsub("Set ", "", .) %>%
    as.integer()
  
  corrections <- list()
  for(i in seq_along(startValues)) {
    corrections[[i]] <- multiPointCorrection(
      startTime = aq_multipoint$StartTime[i],
      endTime = aq_multipoint$EndTime[i],
      set = set[i],
      startValues = startValues[[i]],
      startOffsets = startOffsets[[i]],
      endValues = endValues[[i]],
      endOffsets = endOffsets[[i]]
    )
  }
  return(corrections)
}

#' Get gap tolerances from AQUARIUS
#' 
#' @param tsID the time series unique identifier
#' @param start the start date of interest as a string in the form YYYY-MM-DD
#' @param end the end date of interest as a string in the form YYYY-MM-DD
#' 
#' @return a data frame of gap tolerances. The gap tolerance for a time series
#' can be changed over time, so the data frame may have 1 or many rows.
#' 
#' @export
#' 

getGapTolerance <- function(tsID, start, end) {
  serviceRequest <- "GetTimeSeriesCorrectedData"
  parameters <- c("TimeSeriesUniqueId", "QueryFrom", "QueryTo", "GetParts", "UtcOffset", "IncludeGapMarkers")
  values <- c(tsID, start, end, "MetadataOnly", "0", "true")
  metadata <- genericAQ(serviceRequest, parameters, values)
  out <- metadata$GapTolerances
  out$StartTime <- as.POSIXct(out$StartTime, format="%Y-%m-%dT%H:%M", tz="GMT")
  out$EndTime <- as.POSIXct(out$EndTime, format="%Y-%m-%dT%H:%M", tz="GMT")
  return(out)
}

#' Get all available time series for a location and date range
#' 
#' @param location the location identifier
#' @param start the start date (or datetime) as a string in a standard, unambiguous 
#' format (recognizable by \code{as.POSIXct}), or a POSIXct object
#' @param end the end date (or datetime) as a string in a standard, unambiguous 
#' format (recognizable by \code{as.POSIXct}), or a POSIXct object
#' @param publish a logical indicating whether to limit the search to published
#' time series (time series marked "Publish" in AQUARIUS)
#' 
#' @return a data frame of time series descriptions for all available time series. Return
#' NULL if no time series match the given parameters
#' 
#' @importFrom magrittr %>%
#' 
#' @export
#' 

getAvailableTimeSeries <- function(location, start, end, publish) {
  
  RawStartTime <- RawEndTime <- ".dplyr.var"
  
  date_range <- as.POSIXct(c(start, end))
  if(any(is.na(date_range))) {
    stop("Invalid date range")
  }
  
  serviceRequest <- "GetTimeSeriesDescriptionList"
  if(publish) {
    parameters <- c("LocationIdentifier", "Publish", "ComputationIdentifier", "ComputationPeriodIdentifier")
    values <- c(location, "true", "Instantaneous", "Points")
  } else {
    parameters <- c("LocationIdentifier", "ComputationIdentifier", "ComputationPeriodIdentifier")
    values <- c(location, "Instantaneous", "Points")
  }
  
  raw <- genericAQ(serviceRequest, parameters, values)
  out <- raw$TimeSeriesDescriptions
  if(is.null(out)) {
    message("No time series found for this location")
    return(NULL)
  }
  out$RawStartTime <- as.POSIXct(out$RawStartTime)
  out$RawEndTime <- as.POSIXct(out$RawEndTime)
  out <- out %>% 
    dplyr::filter(!(RawStartTime >= date_range[2] | RawEndTime <= date_range[1]))
  
  return(out)
}