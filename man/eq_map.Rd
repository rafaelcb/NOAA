% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maps.R
\name{eq_map}
\alias{eq_map}
\title{Draw earthquake locations and annotate them.}
\usage{
eq_map(df, annot_col = NULL)
}
\arguments{
\item{df}{Dataframe with NOAA data, including  at least earthquake
longitude and latitude.}

\item{annot_col}{Unquoted column names with information to annotate the popups of the map.}
}
\description{
Draw earthquake locations and annotate them.
}
\examples{
library(dplyr)
eq_data \%>\% eq_clean_data() \%>\% eq_location_clean() \%>\%
dplyr::filter(COUNTRY == "CHINA" & lubridate::year(DATE) >= 2000) \%>\%
eq_map(annot_col = DATE)

}
