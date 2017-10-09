#' NOAA earthquake data
#'
#' A sample dataset containing the locations and information of earthquakes from the
#' NOAA (National Oceanic and Atmospheric Administration) Significant Earthquake
#' Database. It contains information on destructive earthquakes since
#' 2150 B.C. which meet at least one of the following criteria: (i) moderate
#' damage ($1 million or more), (ii) 10 or more deaths, (iii) magnitude 7.5+,
#' (iv) modified Mercally Intensity X or greate or (iv) generated a tsunami.
#'
#'National Geophysical Data Center / World Data Service (NGDC/WDS): Significant Earthquake Database. National Geophysical Data Center, NOAA.
#'
#' @format A data frame with 5945 rows and 9 variables:
#' \describe{
#'   \item{YEAR}{Year of the earthquake}
#'   \item{MONTH}{Month of the earthquake}
#'   \item{DAY}{Day of the earthquake}
#'   \item{COUNTRY}{Country where the earthquake happened}
#'   \item{LOCATION_NAME}{Name of the location of the earthquake}
#'   \item{LATITUDE}{Latitude of the epicenter of the earthquake}
#'   \item{LONGITUDE}{Longitude of the epicenter of the earthquake}
#'   \item{EQ_PRIMARY}{Magnitude of the earthquake, based on Richter scale}
#'   \item{TOTAL_DEATHS}{Number of deaths as a result of the earthquake}
#' }
#' @source \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
"eq_data"
