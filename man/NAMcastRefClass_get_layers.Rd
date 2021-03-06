% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NAMcast.R
\name{NAMcastRefClass_get_layers}
\alias{NAMcastRefClass_get_layers}
\title{Retrieve a stack of numerous layers for a single parameter}
\arguments{
\item{...}{parameters for \code{get_layer()}}

\item{times}{the time indices or POSIXct values, by default all times
or a character flag of 'first', 'last', or 'all'}
}
\value{
a RasterStack with Z set to time or NULL
}
\description{
This is a convenience wrapper around repeated \code{get_layer()} for a single
 parameter
}
