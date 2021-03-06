% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nc.R
\name{ncdim_get}
\alias{ncdim_get}
\title{Retrieve a list defining a single dimension}
\usage{
ncdim_get(
  NC,
  dimname = "some dimension name",
  what = c("vals", "units", NA)[1]
)
}
\arguments{
\item{NC}{ncdf4 object}

\item{dimname}{character the name of the dimension}

\item{what}{character, an optional subelement to return}
}
\value{
a lst of dimension elements
}
\description{
Retrieve a list defining a single dimension
}
