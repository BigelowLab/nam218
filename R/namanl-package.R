#' A simplified interface to NOMADS NAM-ANL forecast data and archives.
#'
#' @name namanl-package
#' @docType package
#' @import methods
#' @examples
#'  \dontrun{
#'  url = "http://nomads.ncdc.noaa.gov/thredds/dodsC/namanl/200606/20060601/namanl_218_20060601_0000_000.grb"
#'  X <- NAMANL(url)
#'  RH <- X$get_layer("Relative_humidity")
#'  SWG <- X$get_layer("Surface_wind_gust")
#' }
NULL