---
title: "Information"
author: "PatrickEslick"
date: "July 30, 2019"
---

## Corrections/Grades/Gaps

The function loads each raw time series point, then downloads and applies any applicable fouling and drift corrections. The criteria in TM-1D3 are used to determine a grade for each point from "Excellent" to "Consider deletion". The percentages given in the summary table indicate what fraction of the points in the date range belong to each category.

### Instructions

1. Select Corrections/Grades/Gaps from the sidebar menu.
2. Enter the site number for the location.
3. Select the parameter of interest from the Parameter drop-down.
4. Select the time period of interest from the "Start" and "End" boxes. The maximum date range is 18 months. You can type a date or select it from the calendar pop-up.
5. Select the specific time series of interest from the Time series drop-down. If this drop-down menu doesn't appear, close the app and try again in a few minutes. If a message appears saying "Argument is of length zero", no time series were found matching this location and parameter.
6. Click "Go"  

### Details   

#### Time Series

The table in the time series tab shows the raw value, values of fouling and drift corrections, the final computed value, and a grade for each point. Definitions for each column are given below:

**datetime** - the date and time of each point, in GMT.  
**raw** - the raw value from AQUARIUS.  
**fouling** - the numeric value of the fouling correction applied to each point.  
**foulingPercent** - the value of the fouling correction applied to each point as a percent of the raw value.  
**foulingCorrected** - the value of the measurement after the fouling correction has been applied.  
**drift** - the numeric value of the drift correction applied to each point.  
**driftPercent** - the value of the drift correction applied to each point as a percent of the raw value.  
**netCorrection** - the net numeric correction applied to each point.  
**netPercent** - the net correction applied to each point as a percent of the raw value.  
**sumNumerical** - the sum of the absolute values of the fouling and drift correction.  
**sumPercent** - the sum of the absolute value of the fouling and drift corrections as a percent of the raw value.  
**Final** - the value of the measurement after all fouling and drift corrections have been applied.  
**Grade** - the grade of the measurement based on the criteria given in TM-1D3.  

#### Record completeness  

##### Gap table

Shows information about gaps and deleted points. The gap tolerance and observation frequency can 
be selected from the drop-down menus. If AQUARIUS gap tolerance is selected, gap tolerances associated
with the selected time series will be used.

**start_date** - the start date of the period selected
**end_date** - the end date of the period selected
**gap_percent** - the percentage of time that is in a gap
**gap_time** - the total time, in minutes, that falls into a gap
**time_span** - the total time span of the period, in minutes
**tolerance** - the gap tolerance, in minutes
**completeness** - the percentage of points present in the time series relative to the maximum number of points
that could have been in the time series if none were deleted or otherwise missing
**frequency_minutes** - the time series frequency used for the completeness calculation
**points_expected** - the maximum number of points that could have been in the time series given the frequency and
time span selected
**points_observed** - the total number of points in the final corrected data for the time series

#### Approval

Shows the date range for any periods that were approved, and then unapproved. If no periods have been unapproved, the table will be empty.

**StartDate** - the beginning of the unapproved period.  
**EndDate** - the end of the unapproved period.  
**WhenUnapproved** - the date and time the period was unapproved.  
**User** - the user who unapproved the period.

## Download Time Series

This function allows the user to download multiple time series from a particular location, aligned by
date and time. Missing points in any of the time series are not interpolated, and all points from all 
the selected time series are provided (if the interval is set to "All"). If all the time series are
missing at a particular datetime, no point is show in the output (unless an interval other than "All"
is selected). This is contrary to the standard functionality in AQUARIUS, where a target time series
is selected, and other time series are aligned to the target.

### Instructions

1. Select "Download Time Series" from the sidebar menu.
2. Fill in the location and date range fields.
3. If you want to limit the time series to only "Published" time series, select the "Publish" box.
4. Click "Find Time Series." The application will find any available time series matching the location,
date range and publish flag and populate the time series box.
5. Select all the time series you want from the "Time Series" box.
6. Select the time zone for the output. "Eastern", "Central", "Mountain", and "Pacific" time zones account for
daylight savings time in the output. "UTC" and "Arizona" use a constant offset.
7. Select an interval. If "All", all points from each time series will be shown. Wherever a time series
does not have a corresponding point "NA" will be shown. Whenever all time series are missing, no point will be
shown in the output. If a particular interval is selected, only time series points corresponding to that interval
will be shown, and even if all the selected time series are missing, a point with that time will still be shown.
8. Click "Download."

## Sample Flows  

This function gets a list of sample start times from the [QW NWIS web service](https://nwis.waterdata.usgs.gov/nwis/qwdata), along with any discharge (Q) and gage height (GHT) measurements associated with the sample in QWDATA. The sample start times are then used to interpolate a value for discharge and gage height between the continuous time series points immediately before and after the sample. The app can also be used to find the discharge and gage height time series points closest in time to the reported sample time.

### Instructions  

1. Click "Sample Flows" in the sidebar
2. Type the USGS site number in the **Location** box.
3. Type (or select) start and end dates in the Dates boxes.
4. Select the maximum gap to use for interpolation. Time series values will only be interpolated if there are two points before and after the sample that are within this many hours of each other.
5. Select the method used to merge the sample times with the discharge and gage height data. By default, the app will interpolate between the two closest points. The other option is to simply use the closest time series point to each sample time.
6. Click "Get data" to display results.
7. Click "Download" to download a csv file of the results.

### Details  

**datetime** - the start time of the sample from the NWIS web service  
**QW_Q** - the discharge associated with the sample from the [QW NWIS web service](https://nwis.waterdata.usgs.gov/nwis/qwdata)    
**QW_GHT** - the gage height associated with the sample in QW NWIS web service  
**datetime_TS** - this column will be included if the "Use closest time series point" option is selected. This indicates the date and time of the nearest time series point to the sample  
**TS_Q** - the discharge interpolated (if using interpolation) from the continuous discharge time series on the [NWIS web service](https://waterservices.usgs.gov/)  
**TS_Q_cd** - the remark code associated with the discharge time series. If using interpolation, this is the remark code from the later point used for interpolation  
**TS_GHT** - the gage height interpolated (if using interpolation) from the continuous gage height time series on the NWIS web service  
**TS_GHT_cd** - the remark code associated with the gage height time series. If using interpolation, this is the remark code from the later point used for interpolation  

