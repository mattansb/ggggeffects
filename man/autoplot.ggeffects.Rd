% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot.R
\name{autoplot.ggeffects}
\alias{autoplot.ggeffects}
\title{Create a (blank) ggplot from ggeffects}
\usage{
\method{autoplot}{ggeffects}(object, mapping = NULL, rows = waiver(), cols = waiver(), ...)
}
\arguments{
\item{object}{A \code{ggeffects} object.}

\item{mapping}{List of aesthetic mappings to use for plot. Defaults (which
can be (individually) overridden) are:
\itemize{
\item \code{predicted} is mapped to \code{y}
\item first term is mapped to \code{x}
\item second term is mapped to \code{group}, \code{color} and \code{fill}
\item \code{conf.high}/\code{conf.low} are mapped onto \code{ymax}/\code{ymin}
}}

\item{cols, rows}{Specs for the facetting of the plot (See \code{cols} and \code{rows}
args in \code{\link[=facet_grid]{facet_grid()}}). Defaults to third and forth terms, respectively.
Disable facetting by setting both to \code{NULL}.}

\item{...}{Args passed to \code{\link[=facet_grid]{facet_grid()}}, sush as \code{labeller}, etc.}
}
\description{
Create a (blank) ggplot from ggeffects
}
