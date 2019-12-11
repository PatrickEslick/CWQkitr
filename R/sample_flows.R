#' Find QW sample times, with streamflow and gage height, if available from NWIS web services
#' 
#' @param site USGS site number
#' @param start the start date of interest, as a string in the form YYYY-MM-DD
#' @param end the start date of interest, as a string in the form YYYY-MM-DD
#' @param tz the time zone for output, e.g. "America/Chicago", "America/New_York", 
#' see \code{OlsonNames()} for more details
#' 
#' @return a data frame of QW sample times. For each QW sample, the sample start time,
#' and associated streamflow (00061) and gage height (00065) are given.
#' 
#' @export
#' 

getSampleTimes <- function(site, start, end, tz = "UTC") {
  qw <- try({
    dataRetrieval::readNWISqw(site, "All", startDate = start, endDate = end, reshape = TRUE, tz = tz)
    }, silent=TRUE)
  if(class(qw) == "try-error") {
    message(paste("Call to NWIS (readNWISqw) did not work for", 
                  site, "from", start, "to", end, 
                  "with reshape = TRUE"))
    return(0)
  }
  cols <- c("startDateTime", "result_va_00061", "result_va_00065")
  keep <- names(qw)[names(qw) %in% cols]
  qw <- qw[,keep]
  if(length(keep) == 1) {
    qw <- data.frame(datetime = qw)
    return(qw)
  }
  names(qw)[names(qw) == "startDateTime"] <- "datetime"
  if("result_va_00061" %in% keep)
    names(qw)[names(qw) == "result_va_00061"] <- "QW_Q"
  if("result_va_00065" %in% keep)
    names(qw)[names(qw) == "result_va_00065"] <- "QW_GHT"
  return(qw)
}

#' Get continuous streamflow and gage height from NWIS web services
#' 
#' @param site USGS site number
#' @param start the start date of interest, as a string in the form YYYY-MM-DD
#' @param end the start date of interest, as a string in the form YYYY-MM-DD
#' @param tz the time zone for output, e.g. "America/Chicago", "America/New_York", 
#' see \code{OlsonNames()} for more details
#' 
#' @return a data frame with a time series of streamflow (00060) and gage height
#' (00065), and the associated codes for each time series point (e.g. "A" for approved,
#' e for estimated).
#' 
#' @export
#' 

getQGHT <- function(site, start, end, tz = "UTC") {
  Qname <- "X_00060_00000"
  QGHT <- dataRetrieval::readNWISuv(site, c("00060","00065"), start, end, tz = tz)
  if(nrow(QGHT) == 0) {
    return(data.frame(datetime = vector(), TS_Q = vector(), TS_GHT = vector()))
  }
  keep <- names(QGHT)[names(QGHT) %in% c("dateTime", "X_00060_00000", "X_00060_00000_cd", 
                                         "X_00065_00000", "X_00065_00000_cd")]
  QGHT <- QGHT[,keep]
  if("X_00060_00000" %in% keep) {
    names(QGHT)[names(QGHT) == "X_00060_00000"] <- "TS_Q"
  }
  if("X_00060_00000_cd" %in% keep) {
    names(QGHT)[names(QGHT) == "X_00060_00000_cd"] <- "TS_Q_cd"
  }
  if("X_00065_00000" %in% keep) {
    names(QGHT)[names(QGHT) == "X_00065_00000"] <- "TS_GHT"
  }
  if("X_00065_00000_cd" %in% keep) {
    names(QGHT)[names(QGHT) == "X_00065_00000_cd"] <- "TS_GHT_cd"
  }
  names(QGHT)[names(QGHT) == "dateTime"] <- "datetime"
  return(QGHT)
}

#' Merge sample times and continuous data by the closest point
#' 
#' Not to be used directly, but for \code{getSampleQ}
#' 
#' @param sample_data sample times from \code{getSampleTimes}
#' @param cont_data continuous streamflow and gage height data from \code{getQGHT}
#' @param maxDiff the maximum gap, in hours, in the continuous data that 
#' will be used for merging. Any gaps greater than this will show NA for the
#' continuous data
#' 
#' @importFrom magrittr %>%
#' 
#' @return sample data joined with the closest (in time) continuous time series
#' point

closestQGHT <- function(sample_data, cont_data, maxDiff = 4) {
  
  datetime.qw <- datetime.ts <- ".dplyr.var"
  
  merged <- smwrBase::mergeNearest(sample_data, dates.left = "datetime", all.left = TRUE, suffix.left = "qw",
                         cont_data, dates.right = "datetime", suffix.right = "ts",
                         max.diff = paste(maxDiff, "hours")) %>%
    dplyr::rename(datetime = datetime.qw, datetime_TS = datetime.ts)
  row.names(merged) <- NULL
  merged$datetime_TS <- as.character(merged$datetime_TS, format="%Y-%m-%d %H:%M %Z")
  return(merged)
  
}

#' Merge sample times and continuous data by interpolation
#' 
#' Not to be used directly, but for \code{getSampleQ}
#' 
#' @param sample_data sample times from \code{getSampleTimes}
#' @param cont_data continuous streamflow and gage height data from \code{getQGHT}
#' @param maxDiff the maximum gap, in hours, in the continuous data that 
#' will be used for merging. Any gaps greater than this will show NA for the
#' continuous data
#' 
#' @return sample data with continuous data interpolated for each sample time
#' 
#' @importFrom magrittr %>%

intQGHT <- function(sample_data, cont_data, maxDiff = 4) {

  s_datetimes <- sample_data$datetime
  cont_vars <- names(cont_data)[names(cont_data) != "datetime"]
  for(i in cont_vars) {
    intData <- cont_data[,c("datetime",i)] %>%
      stats::na.omit()
    intData$gap <- (dplyr::lead(intData$datetime) - intData$datetime) %>%
      as.numeric(units = "hours")
    if(class(intData[,i]) == "numeric") {
      sd <- stats::approx(
        x = intData$datetime,
        y = intData[,i],
        xout = sample_data$datetime
      )$y
    } else {
      intData[,i] <- factor(intData[,i])
      key <- data.frame(
        num = as.numeric(intData[,i]),
        lvl = as.character(intData[,i])
      ) %>%
        unique()
      sd <- stats::approx(
        x = intData$datetime,
        y = intData[,i],
        xout = sample_data$datetime
      )$y
      sd <- factor(sd, levels = key$num, labels = key$lvl) %>%
        as.character()
    }    
    sd_gap <- stats::approx(
      x = intData$datetime,
      y = intData$gap,
      xout = sample_data$datetime,
      method = "constant",
      f = 0
    )$y
    sd[sd_gap > maxDiff] <- NA
    sample_data[,i] <- sd
  }
  return(sample_data)
}

#' Get a table of sample times, flows and gage heights
#' 
#' The table includes QW sample times, as well as any associated gage heights
#' and discharge values associated with the sample. Discharge and gage height
#' are interpolated form the relevant continuous time series for comparison.
#' 
#' @param site USGS site number
#' @param start the start date of interest, as a string in the form YYYY-MM-DD
#' @param end the start date of interest, as a string in the form YYYY-MM-DD
#' @param maxDiff the maximum gap, in hours, in the continuous data that 
#' will be used for merging. Any gaps greater than this will show NA for the
#' continuous data.
#' @param method the method to use for merging the continuous data. If "interpolate",
#' the continuous value will be interpolated for the sample time between the time
#' series points immediately preceding and following. If "closest", the continuous
#' point nearest the sample time will be used.
#' @param tz the time zone for output, e.g. "America/Chicago", "America/New_York", 
#' see \code{OlsonNames()} for more details
#' 
#' @return a data frame of QW sample times, streamflow, and discharge merged with 
#' continuous streamflow and discharge for comparison.
#' 
#' @export
#' 

getSampleQ <- function(site, start, end, maxDiff = 4, 
                       method = c("interpolate", "closest"), 
                       tz = "UTC") {
  
  samples <- getSampleTimes(site, start, end, tz = tz)
  if(length(samples) == 0) {
    return("No samples found")
  }
  qght <- getQGHT(site, start, end, tz = tz)
  if(nrow(qght) == 0) {
    return("No continuous data found")
  }
  if(method == "closest") {
    sampleQ <- closestQGHT(samples, qght, maxDiff = maxDiff)
  } else if (method == "interpolate") {
    sampleQ <- intQGHT(samples, qght, maxDiff = maxDiff)
  }
  sampleQ$datetime <- as.character(sampleQ$datetime, format="%Y-%m-%d %H:%M %Z", tz = tz)
  return(sampleQ)
  
}