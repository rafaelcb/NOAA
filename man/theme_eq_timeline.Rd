% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geoms.R
\docType{data}
\name{theme_eq_timeline}
\alias{theme_eq_timeline}
\title{Theme for geom_timeline plot}
\format{An object of class \code{theme} (inherits from \code{gg}) of length 7.}
\usage{
theme_eq_timeline
}
\description{
This theme helps visualize the information from a geom_timeline
plot better.
}
\examples{
\dontrun{eq_data \%>\% eq_clean_data() \%>\%
   filter(COUNTRY \%in\% c("GREECE", "ITALY"), YEAR > 2000) \%>\%
   ggplot(aes(x = DATE, y = COUNTRY)) +
   geom_timeline() + theme_eq_timeline()}

}
\keyword{datasets}
