% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grib1.R
\name{Grib1}
\alias{Grib1}
\title{Create a Grib1RefClass object}
\usage{
Grib1(
  filename = "/mnt/ecocast/projects/devel/grib/namanl_218_20040303_0000_000.grb"
)
}
\arguments{
\item{filename}{the fully qualified filename}
}
\value{
a Grib1RefClass object
}
\description{
There are just two methods of interest for retrieving a single band or a brick
}
\details{
\code{raster = ref$read_band(var = "", level = "", bb = [NULL | c(left, right, bottom, top)])}
\code{brick = ref$read_brick()}

The latter you an just manage on your own by using \code{brick = raster::brick(filename)}, but
{ref$read_brick()} does assign better names for each layer.
}
