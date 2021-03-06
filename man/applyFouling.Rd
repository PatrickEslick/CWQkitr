% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corrections_grades_gaps.R
\name{applyFouling}
\alias{applyFouling}
\title{Apply all fouling corrections from a list of corrections}
\usage{
applyFouling(timeSeries, corrections)
}
\arguments{
\item{timeSeries}{a time series of raw data from \code{getRawData}}

\item{corrections}{a list of USGS multi-point corrections from \code{getCorrections}}
}
\value{
a numeric vector, the same length as the number of rows of \code{timeSeries} 
showing the combined effect of all the fouling corrections in the list
}
\description{
Apply each Set 1 correction from a list of multiPointCorrections that is in Set 1 to a
time series from \code{getRawData}. (Should not be used directly, only from 
within corrApply)
}
