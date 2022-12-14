% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pieGrob.R
\name{.calc_arc}
\alias{.calc_arc}
\title{Generate coordinates along an arc.}
\usage{
.calc_arc(.x, .y, .r, .theta0, .theta1, n, go_ccw = TRUE)
}
\arguments{
\item{.r}{The radius of the arc}

\item{.theta0}{Start of the arc.}

\item{n}{The number of points to sample. Note, this is the number to used for the entire circle}

\item{go_cww}{Should the points be calculate in CounterClockWise order?}
}
\value{
A named list of length 2, specifying the x and y coordinates for the arc required.
}
\description{
Generate coordinates along an arc.
}
\keyword{internal}
