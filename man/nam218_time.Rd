% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{nam218_time}
\alias{nam218_time}
\title{From a two vectors of forecast period ('ftime') and forecast
 ahead ('ahead') determine the number of seconds to the forecast period from
 midnight}
\usage{
nam218_time(
  ftime = c("0000", "0600", "1200", "1800"),
  ahead = c("000", "001", "003", "006")
)
}
\arguments{
\item{ftime}{one or more forecast periods in hhhh}

\item{ahead}{one or more ahead forecast times in hhh}
}
\value{
numeric number of seconds into a day
}
\description{
From a two vectors of forecast period ('ftime') and forecast
 ahead ('ahead') determine the number of seconds to the forecast period from
 midnight
}
