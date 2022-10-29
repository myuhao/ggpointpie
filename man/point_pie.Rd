% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/point_pie.R
\docType{data}
\name{geom_point_pie}
\alias{geom_point_pie}
\alias{GeomPointPie}
\title{Draw pie chart at any x,y coordinates.}
\format{
An object of class \code{GeomPointPie} (inherits from \code{GeomPolygon}, \code{Geom}, \code{ggproto}, \code{gg}) of length 5.
}
\usage{
geom_point_pie(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  r0 = 0L,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)

GeomPointPie
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}} or
\code{\link[ggplot2:aes_]{aes_()}}. If specified and \code{inherit.aes = TRUE} (the
default), it is combined with the default mapping at the top level of the
plot. You must supply \code{mapping} if there is no plot mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this
layer, as a string.}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function.}

\item{r0}{The radius (0-1) of the inner circle, in case a donut plot is needed.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{...}{skip for now

todo: think about edge case where there is only 1 observation
todo: deal with empty factor levels, what do we return?}
}
\description{
This geom is inherits from on \link[ggplot2:ggplot2-ggproto]{ggplot2::GeomPolygon}. W
e used polygon geom to create pie chart than can be mapped to
arbitrary x and y coordinates.
A advantage of this geom over other implementation is that is keeps its
aspect ratio so the point is always a perfect circle.
It can also be mapped to both continuous and discrete position scales.

ggproto class that descripe combination of pie chart and point.

ggplot always map size to 1-6. Use scale_size_contineuous to deal with it.
In the future, maybe get our own scale...
}
\section{Aesthetics}{

geom_point_pie understand the following aesthetics (required aesthetics are in
bold):
\itemize{
\item \strong{x}
\item \strong{y}
\item \strong{group}
\item color: line color
\item fill
\item r1: the size of the pie chart.
\item linetype
\item alpha
}
}

\examples{
data = tibble::tibble(
    x = c(1, 1, 1, 2, 2, 2, 2),
    y = c(1, 1, 1, 2, 2, 2, 2),
    grp = c(a, a, b, a, a, b, b)
)
ggplot(data, aes(x = x, y = y)) +
  geom_point_pie(aes(group = grp, fill = grp))
}
\keyword{datasets}