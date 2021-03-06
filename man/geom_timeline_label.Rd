% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geoms.R
\name{geom_timeline_label}
\alias{geom_timeline_label}
\title{Timeline labels}
\usage{
geom_timeline_label(mapping = NULL, data = NULL, stat = "identity",
  position = "identity", ..., na.rm = FALSE, n_max = NULL,
  show.legend = NA, inherit.aes = TRUE)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{aes} or \code{aes_}.}

\item{data}{The data to be displayed.  If specified and \code{inherit.aes = TRUE}
(the default), it is combined with the default mapping at the top level of
the plot. You must supply mapping if there is no plot mapping.}

\item{stat}{The statistical transformation to use on the data for this layer,
as a string.}

\item{position}{Position adjustment, either as a string, or the result of a call to a position adjustment function.}

\item{...}{other arguments passed on to layer.}

\item{na.rm}{If FALSE, the default, missing values are removed with a warning.
If TRUE, missing values are silently removed.}

\item{n_max}{The maximum number of points to be labelled. If a size aesthetic}

\item{show.legend}{logical. Should this layer be included in the legends? NA,
the default, includes if any aesthetics are mapped. FALSE never includes,
and TRUE always includes.}

\item{inherit.aes}{If FALSE, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. borders.}
}
\value{
A GeomTimelineLabel layer
}
\description{
This geom adds labels to the points in a timeline chart, up to \code{n_max} points.
}
\examples{
\dontrun{eq_data \%>\% eq_clean_data() \%>\% filter(EQ_PRIMARY > 6, DATE > "2000-01-01") \%>\%
ggplot(aes(x = DATE, size = EQ_PRIMARY, color = DEATHS, y = COUNTRY)) +
geom_timeline()}
}
