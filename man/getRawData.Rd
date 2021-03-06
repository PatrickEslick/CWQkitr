% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_tools.R
\name{getRawData}
\alias{getRawData}
\title{Get raw (uncorrected) time series data}
\usage{
getRawData(tsID, start, end)
}
\arguments{
\item{tsID}{the time series unique identifier}

\item{start}{the starting date of interest as a string in the form YYYY-MM-DD}

\item{end}{the ending date of interest as a string in the form YYYY-MM-DD}
}
\value{
a data frame of the raw time series data. If no data is
found for the time series and date range, return a data frame with no columns
or rows.
}
\description{
Return a time series of data from AQUARIUS without any corrections applied
}
