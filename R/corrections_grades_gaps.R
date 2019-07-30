
#' An S4 class to represent a USGS multipoint correction
#' 
#' @slot startTime the beginning time of the correction, a POSIXct object
#' @slot endTime the ending time of the correction, a POSIXct boject
#' @slot set the set the correction was applied in, an integer - either 1, 2, or 3
#' @slot startValues the values to which the starting offsets are applied.
#' Must be the same length as startOffsets
#' @slot startOffsets the starting offsets of the correction. Must be the same
#' length as startValues
#' @slot endValues the values to which the ending offsets are applied.
#' Must be the same length as endOffsets
#' @slot endOffsets the ending offsets of the correction. Must be the same
#' length as endValues
#' 
multiPointCorrection <- setClass(
  "multiPointCorrection",
  slots = c(
    startTime = "POSIXct",
    endTime = "POSIXct",
    set = "numeric",
    startValues = "numeric",
    startOffsets = "numeric",
    endValues = "numeric",
    endOffsets = "numeric"
  )
)

setValidity("multiPointCorrection",
  function(object) {
    object@startTime <= object@endTime &&
    length(object@set) == 1 &&
    object@set %in% 1:3 &&
    length(object@startValues) <= 3 &&
    length(object@startValues) == length(object@startOffsets) &&
    length(object@endValues) <= 3 &&
    length(object@endValues) == length(object@endOffsets) 
  }
)

#' Determine whether two intervals overlap
#' 
#' @param start1 the start of the first interval
#' @param end1 the end of the first interval
#' @param start2 the start of the second interval
#' @param end2 the end of the second interval
#' 
#' @return a logical indicating whether the two intervals overlap
#' 

isOverlap <- function(start1, end1, start2, end2) {
  if(end1 <= start2 | start1 >= end2) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Find the overlap of two overlapping intervals
#' 
#' @param start1 the start of the first interval
#' @param end1 the end of the first interval
#' @param start2 the start of the second interval
#' @param end2 the end of the second interval
#' 
#' @return a vector of length 2 giving the start and end of the overlap
#' between the two intevals
#' 

whatOverlap <- function(start1, end1, start2, end2) {
  if(start1 >= start2 & start1 < end2) {
    overlapStart <- start1
  } else {
    overlapStart <- start2
  }
  if(end1 > start2 & end1 <= end2) {
    overlapEnd <- end1 
  } else {
    overlapEnd <- end2
  }
  return(c(overlapStart, overlapEnd))
}

#' Find periods that have been set back from approved
#' 
#' Given a list of approval transactions (from \code{getApprovalList}), give any 
#' periods that had been set to approved, but were later set to a lower status, 
#' whether working or in review
#' 
#' @param approvalList a data frame of approval transaction sfrom getApprovalList()
#' 
#' @export
#' 

findDisapproval <- function(approvalList) {
  
  unAppStart <- vector()
  unAppEnd <- vector()
  unAppDateApplied <- vector()
  unAppUser <- vector()
  
  #Loop through each approval transaction with a level less than 1200 see 
  #if it overlaps any periods that were previously 1200
  toCheck <- approvalList[approvalList$ApprovalLevel < 1200,]
  approved <- approvalList[approvalList$ApprovalLevel == 1200,]
  if(nrow(toCheck) > 0 & nrow(approved) > 0) {
    for(i in 1:nrow(toCheck)) {
      checkStart <- toCheck$StartTime[i]
      checkEnd <- toCheck$EndTime[i]
      #Check this start and end against every entry in the approved list
      for(j in 1:nrow(approved)) {
        appStart <- approved$StartTime[j]
        appEnd <- approved$EndTime[j]
        overlap <- isOverlap(checkStart, checkEnd, appStart, appEnd)
        #See if the approval was applied before the lower rating
        later <- toCheck$DateAppliedUtc[i] > approved$DateAppliedUtc[j]
        if(overlap & later) {
          period <- whatOverlap(checkStart, checkEnd, appStart, appEnd)
          unAppStart[length(unAppStart) + 1] <- as.character(period[1], format="%Y-%m-%d %H:%M")
          unAppEnd[length(unAppEnd) + 1] <- as.character(period[2], format="%Y-%m-%d %H:%M")
          unAppDateApplied[length(unAppDateApplied) + 1] <- 
            as.character(toCheck$DateAppliedUtc[i], format="%Y-%m-%d %H:%M")
          unAppUser[length(unAppUser) + 1] <- toCheck$User[i]
        }
      }
    }
  }
  unApprovedPeriods <- data.frame(unAppStart, unAppEnd, unAppDateApplied, unAppUser)
  names(unApprovedPeriods) <- c("StartDate", "EndDate", "WhenUnapproved", "User")
  dup <- duplicated(unApprovedPeriods[,1:2])
  unApprovedPeriods <- unApprovedPeriods[!dup,]
  return(unApprovedPeriods)
  
}

#' Apply a correction to a time series
#' 
#' @param datetime a vector of datetimes, as POSIXct
#' @param raw a numeric vector of raw time series data - should be the same length
#' as datetime
#' @param correction a multiPointCorrection object
#' 
#' @return a numeric data of the value of the correction at each point
#' 
#' @export
#' 

applyCorrection <- function(datetime, raw, correction) {
  
  #Find the time factor for each raw value
  tf <- approx(x = c(correction@startTime, correction@endTime),
               y = c(0, 1),
               xout = datetime,
               yleft = NA,
               yright = NA)$y
  #Interpolate correction start points for each raw value
  if(length(correction@startOffsets) > 1) {
    sc <- approx(x = correction@startValues, 
                 y = correction@startOffsets, 
                 xout = raw,
                 yleft = correction@startOffsets[which.min(correction@startValues)],
                 yright = correction@startOffsets[which.max(correction@startValues)])$y
  } else if(length(correction@startOffsets) == 1){
    sc <- rep(correction@startOffsets[1], length(raw))
  } else if (length(correction@startOffsets) == 0) {
    sc <- rep(0, length(raw))
  }
  #Interpolate correction end points for each raw values
  if(length(correction@endOffsets) > 1) {
    ec <- approx(x = correction@endValues, 
                 y = correction@endOffsets, 
                 xout = raw,
                 yleft = correction@endOffsets[which.min(correction@endValues)],
                 yright = correction@endOffsets[which.max(correction@endValues)])$y
  } else if(length(correction@endOffsets == 1)) {
    ec <- rep(correction@endOffsets[1], length(raw))
  } else if(length(correction@endOffsets == 0)) {
    ec <- rep(0, length(raw))
  }
  #Calculate the corrected value for each raw value
  correction_value <- ((1 - tf) * sc) + (tf * ec)
  correction_value[is.na(correction_value)] <- 0
  return(correction_value)
}

#' Apply all fouling corrections from a list of corrections
#' 
#' Apply each correction from a list of multiPointCorrections that is in Set 1 to a
#' time series from \code{getRawData}
#' 
#' @param timeSeries a time series of raw data from \code{getRawData}
#' @param corrections a list of USGS multi-point corrections from \code{getCorrections}
#' 
#' @return a numeric vector of the combined effect of all the fouling corrections in the
#' list
#' 

applyFouling <- function(timeSeries, corrections) {
  timeSeries$foulingCorrection <- 0
  for(i in corrections) {
    if((!all(c(i@startOffsets, i@endOffsets)==0)) & i@set == 1) {
      timeSeries$foulingCorrection <- timeSeries$foulingCorrection + 
        applyCorrection(timeSeries$datetime, timeSeries$raw, i)
    }
  }
  return(timeSeries$foulingCorrection)
}

#' Apply all drift corrections from a list of corrections
#' 
#' Apply each correction from a list of multiPointCorrections that is in Set 2 to a
#' time series from \code{getRawData}
#' 
#' @param timeSeries a time series of raw data from \code{getRawData}
#' @param corrections a list of USGS multi-point corrections from \code{getCorrections}
#' 
#' @return a numeric vector of the combined effect of all the fouling drift in the
#' list
#' 

applyDrift <- function(timeSeries, corrections) {
  timeSeries$driftCorrection <- 0
  for(i in corrections) {
    if((!all(c(i@startOffsets, i@endOffsets)==0)) & i@set == 2) {
      timeSeries$driftCorrection <- timeSeries$driftCorrection + 
        applyCorrection(timeSeries$datetime, timeSeries$foulingCorrected, i)
    }
  }
  return(timeSeries$driftCorrection)
}

#' Create a data frame of raw time series data with corrections annotated
#' 
#' @param tsID the time series unique identifier
#' @param startDate the start date of interest as a string in the form YYYY-MM-DD
#' @param endDate the end date of interest as a string in the form YYYY-MM-DD
#' 

corrApply <- function(tsID, startDate, endDate) {
  
  #Get raw time series 
  timeSeries <- getRawData(tsID, startDate, endDate)
  #Download corrections
  corrections <- getCorrections(tsID, startDate, endDate)
  #Calculate the fouling corrections
  timeSeries$fouling <- applyFouling(timeSeries, corrections)
  timeSeries$foulingPercent <- (timeSeries$fouling/timeSeries$raw) * 100
  timeSeries$foulingCorrected <- timeSeries$raw + timeSeries$fouling
  #Calculate the drift corrections
  timeSeries$drift <- applyDrift(timeSeries, corrections)
  timeSeries$driftPercent <- (timeSeries$drift/timeSeries$foulingCorrected) * 100
  timeSeries$netCorrection <- timeSeries$fouling + timeSeries$drift
  timeSeries$netPercent <- timeSeries$netCorrection/timeSeries$raw * 100
  timeSeries$sumNumerical <- abs(timeSeries$drift) + abs(timeSeries$fouling)
  timeSeries$sumPercent <- abs(timeSeries$foulingPercent) + abs(timeSeries$driftPercent)
  timeSeries$Final <- timeSeries$raw + timeSeries$netCorrection
  #Round the columns
  timeSeries$fouling <- signif(timeSeries$fouling, 3)
  timeSeries$foulingPercent <- signif(timeSeries$foulingPercent, 3)
  timeSeries$foulingCorrected <- signif(timeSeries$foulingCorrected, 3)
  timeSeries$drift <- signif(timeSeries$drift, 3)
  timeSeries$driftPercent <- signif(timeSeries$driftPercent, 3)
  timeSeries$netCorrection <- signif(timeSeries$netCorrection, 3)
  timeSeries$netPercent <- signif(timeSeries$netPercent, 3)
  timeSeries$sumNumerical <- signif(timeSeries$sumNumerical, 3)
  timeSeries$sumPercent <- signif(timeSeries$sumPercent, 3)
  timeSeries$Final <- signif(timeSeries$Final, 3)
  return(timeSeries)
}

#' Apply grades based on TM-1D3
#' 
#' @param parameter the parameter name
#' @param raw the raw time series data
#' @param percent the total percent correction
#' @param numerical the total numerical correction
#' 
#' @return a character vector of grades, one of "Excellent", "Good", 
#' "Fair", "Poor", or "Consider Deletion"
#' 

wagnerGrade <- function(parameter, raw, percent, numerical) {
  if(parameter == "Specific cond at 25C") {
    pPoint <- 0
    pExcellent <- c(0, 0.03)
    pGood <- c(0.03, 0.10)
    pFair <- c(0.10, 0.15)
    pPoor <- c(0.15, 0.30)
    pDel <- c(0.30, Inf)
    nExcellent <- c(-1, -1)
    nGood <- c(-1, -1)
    nFair <- c(-1, -1)
    nPoor <- c(-1, -1)
    nDel <- c(-1, -1)
  } else if(parameter == "Turbidity, FNU") {
    pPoint <- 10
    pExcellent <- c(0, 0.05)
    pGood <- c(0.05, 0.10)
    pFair <- c(0.10, 0.15)
    pPoor <- c(0.15, 0.30)
    pDel <- c(0.30, Inf)
    nExcellent <- c(0, 0.5)
    nGood <- c(0.5, 1.0)
    nFair <- c(1.0, 1.5)
    nPoor <- c(1.5, 3)
    nDel <- c(3, Inf)
  } else if(parameter == "pH") {
    pPoint <- Inf
    pExcellent <- c(-1, -1)
    pGood <- c(-1, -1)
    pFair <- c(-1, -1)
    pPoor <- c(-1, -1)
    pDel <- c(-1, -1)
    nExcellent <- c(0, 0.2)
    nGood <- c(0.2, 0.5)
    nFair <- c(0.5, 0.8)
    nPoor <- c(0.8, 2)
    nDel <- c(2, Inf)
  } else if(parameter == "Dissolved oxygen") {
    pPoint <- 6
    pExcellent <- c(0, 0.05)
    pGood <- c(0.05, 0.10)
    pFair <- c(0.10, 0.15)
    pPoor <- c(0.15, 0.20)
    pDel <- c(0.20, Inf)
    nExcellent <- c(0, 0.3)
    nGood <- c(0.3, 0.5)
    nFair <- c(0.5, 0.8)
    nPoor <- c(0.8, 2)
    nDel <- c(2, Inf)
  } else if(parameter == "Temperature, water") {
    pPoint <- Inf
    pExcellent <- c(-1, -1)
    pGood <- c(-1, -1)
    pFair <- c(-1, -1)
    pPoor <- c(-1, -1)
    pDel <- c(-1, -1)
    nExcellent <- c(0, 0.2)
    nGood <- c(0.2, 0.5)
    nFair <- c(0.5, 0.8)
    nPoor <- c(0.8, 2)
    nDel <- c(2, Inf)
  }
  num_correction <- abs(numerical)
  per_correction <- abs(percent) / 100
  use_percent <- raw > pPoint
  use_numeric <- !use_percent
  grade <- rep("Unassigned", length(num_correction))
  #Apply percentage grades
  grade[use_percent & per_correction <= pExcellent[2]] <- "Excellent"
  grade[use_percent & per_correction > pGood[1] & per_correction <= pGood[2]] <- "Good"
  grade[use_percent & per_correction > pFair[1] & per_correction <= pFair[2]] <- "Fair"
  grade[use_percent & per_correction > pPoor[1] & per_correction <= pPoor[2]] <- "Poor"
  grade[use_percent & per_correction > pDel[1]] <- "Consider Deletion"
  #Apply numeric grades
  grade[use_numeric & num_correction <= nExcellent[2]] <- "Excellent"
  grade[use_numeric & num_correction > nGood[1] & num_correction <= nGood[2]] <- "Good"
  grade[use_numeric & num_correction > nFair[1] & num_correction <= nFair[2]] <- "Fair"
  grade[use_numeric & num_correction > nPoor[1] & num_correction <= nPoor[2]] <- "Poor"
  grade[use_numeric & num_correction > nDel[1]] <- "Consider Deletion"
  
  return(grade)
}

#' Make a complete table of correction data and grades
#' 
#' @param tsID the unique time series identifier
#' @param start the start date of interest as a string in the form YYYY-MM-DD
#' @param end the end date of interest as a string in the form YYYY-MM-DD
#' @param parm  the parameter name
#' 

makeTable <- function(tsID, start, end, parm) {
  #Get data and apply corrections
  output <- corrApply(tsID, start, end)
  #Only keep data that's in the final data
  correctedData <- getCorrectedData(tsID, start, end)
  output <- output[output$datetime %in% correctedData$datetime,]
  #Give the data a grade
  output <- na.omit(output)
  grade <- wagnerGrade(parm, output$raw, output$sumPercent, output$sumNumeric)
  output$Grade <- grade
  return(output)
}

#' Wrapper for makeTable, with internal API authentication
#' 
#' Used for asynchronous programming in the shiny application.
#' 
#' @param tsID the unique time series identifier
#' @param start the start date of interest as a string in the form YYYY-MM-DD
#' @param end the end date of interest as a string in the form YYYY-MM-DD
#' @param parm  the parameter name
#' @param id the username to use for authentication
#' @param pw the encrypted passowrd to use for authentication
#' 
#' @export
#' 

makeTableConnect <- function(tsID, start, end, parm, id, pw) {
  
  tkn <- retryToken(id, pw)
  out <- makeTable(tsID, start, end, parm)
  
}

#' Summarize grades from the complete table
#' 
#' @param dataTable a table output from \code{makeTable}
#' 

summarizeGrades <- function(dataTable) {
  
  if(nrow(dataTable) != 0) {
    excellentPercent <- nrow(dataTable[dataTable$Grade == "Excellent",])/nrow(dataTable)
    goodPercent <- nrow(dataTable[dataTable$Grade == "Good",])/nrow(dataTable)
    fairPercent <- nrow(dataTable[dataTable$Grade == "Fair",])/nrow(dataTable)
    poorPercent <- nrow(dataTable[dataTable$Grade == "Poor",])/nrow(dataTable)
    delPercent <- nrow(dataTable[dataTable$Grade == "Consider Deletion",])/nrow(dataTable)
    
    grades <- c(excellentPercent, goodPercent, fairPercent, poorPercent, delPercent)
    grades <- grades * 100
    grades <- round(grades, 2)
    grades <- paste0(as.character(grades), "%")
    rows <- c("Excellent", "Good", "Fair", "Poor", "Consider Deletion")
    summary <- data.frame(rows, grades)
    names(summary) <- c("Grade", "Percent")
  } else {
    summary <- data.frame()
  }
  return(summary)
  
}

#' Information for evaluating record completeness
#' 
#' @param datetimes a vector of datetimes as POSIXct
#' @param start the start date of interest, as POSIXct. If "auto", the first date
#' in datetimes will be used
#' @param end the end date of itnerest, as POSIXct. If "auto", the last date
#' in datetimes will be used
#' @param freq the time series frequency, in minutes. If "auto", the most common
#' frequency will be used
#' 
#' @return a data frame with information about record completeness
#' 

recordCompleteness <- function(datetimes, start = "auto", end = "auto", freq = "auto") {
  
  if(freq == "auto") {
    diff <- vector()
    diff[1] <- 0
    for(i in 2:length(datetimes)) {
      diff[i] <- difftime(datetimes[i], datetimes[i-1], units="mins")
    }
    freq_use <- unique(diff)[which.max(tabulate(match(diff, unique(diff))))]
  } else {
    freq_use <- freq
  }
  if(start == "auto") {
    start_use <- min(datetimes)
  } else {
    start_use <- start
  }
  if(end=="auto") {
    end_use <- max(datetimes)
  } else {
    end_use <- end
  }
  time_span <- as.numeric(difftime(end_use, start_use, units="mins"))
  ifComplete <- floor(time_span/freq_use) + 1
  observed <- length(datetimes)
  completeness <- length(datetimes) / ifComplete
  comp_character <- paste(round(completeness * 100, 1), "%")
  
  output <- data.frame(
    start_date = as.character(start_use),
    end_date = as.character(end_use),
    completeness = comp_character,
    frequency_minutes = round(freq_use, 0),
    points_expected = round(ifComplete, 0),
    points_observed = round(observed, 0)
  )
  return(output)
}

#' Find gaps in a time series record
#' 
#' @param datetimes a vector of datetimes
#' @param gapTol the gap tolerance, in minutes or a data frame of gap
#' tolerances from \code{getGapTolerance}
#' 
#' @return a data frame of gaps
#' 

findGaps <- function(datetimes, gapTol = 120) {
  
  diff <- (datetimes - lag(datetimes, 1)) %>%
    as.numeric(units = "mins")
  diff[1] <- 0
  gap <- rep(FALSE, length(datetimes))
  gapTimes <- data.frame(datetime = datetimes, diff = diff, gap = gap)
  if(class(gapTol) == "data.frame") {
    for(i in 1:nrow(gapTol)) {
      temp <- ifelse(gapTimes$datetime >= gapTol$StartTime[i] & 
                       gapTimes$datetime <= gapTol$EndTime[i] & 
                       gapTimes$diff > gapTol$ToleranceInMinutes[i], 
                     TRUE, FALSE)
      gapTimes$gap <- gapTimes$gap | temp
    }
  } else if(class(gapTol) == "numeric") {
    gapTimes$gap <- gapTimes$diff > gapTol
  }
  return(gapTimes)
}

#' Summarize and tabulate the results of findGaps
#' 
#' Give a data frame of information about how much of a time period is classified
#' as a gap in the data record
#' 
#' @param gapTest the results of \code{findGaps}
#' @param gapTol the gap tolerance, in minutes or a data frame of gap
#' tolerances from \code{getGapTolerance}
#' 
#' @return a data frame summary of gaps
#' 
#' @export

summarizeGaps <- function(gapTest, gapTol) {
  
  start <- min(gapTest$datetime)
  start_char <- as.character(start)
  end <- max(gapTest$datetime)
  end_char <- as.character(end)
  time_span <- as.numeric(difftime(end, start, units="mins"))
  gaps <- length(gapTest$gap[gapTest$gap==TRUE])
  gap_time <- sum(gapTest$diff[gapTest$gap==TRUE])
  gap_percent <- round((gap_time / time_span) * 100, 1)
  gap_percent <- paste(gap_percent, "%")
  if(class(gapTol) == "numeric") {
    tolerance <- gapTol
  } else {
    if(min(gapTol$ToleranceInMinutes) == max(gapTol$ToleranceInMinutes)) {
      tolerance <- gapTol$ToleranceInMinutes[1]
    } else {
      tolerance <- "multiple"
    }
  }
  
  out <- data.frame(start_date = start_char, 
                    end_date = end_char, 
                    gap_percent, 
                    gaps, 
                    gap_time,
                    time_span,
                    tolerance)
  return(out)
}