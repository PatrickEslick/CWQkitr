% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_flows.R
\name{getSampleTimes}
\alias{getSampleTimes}
\title{Find QW sample times, with streamflow and gage height, if available from NWIS web services}
\usage{
getSampleTimes(site, start, end, tz = "UTC")
}
\arguments{
\item{site}{USGS site number}

\item{start}{the start date of interest, as a string in the form YYYY-MM-DD}

\item{end}{the start date of interest, as a string in the form YYYY-MM-DD}

\item{tz}{the time zone for output, e.g. "America/Chicago", "America/New_York", 
see \code{OlsonNames()} for more details}
}
\value{
a data frame of QW sample times. For each QW sample, the sample start time,
and associated streamflow (00061) and gage height (00065) are given.
}
\description{
Find QW sample times, with streamflow and gage height, if available from NWIS web services
}
