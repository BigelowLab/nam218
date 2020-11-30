#' @description A simplified interface to NOMADS NAM 218 forecasts and archives.
#' @title nam218
#' @name nam218-package
#' @docType package
#' @importFrom magrittr %>%
#' @importFrom methods new
#' @importFrom utils glob2rx download.file
#' @importFrom rlang .data
#' @examples
#'  \dontrun{
#'  url = file.path("http://nomads.ncdc.noaa.gov/thredds/dodsC/namanl",
#'                  "/200606/20060601/namanl_218_20060601_0000_000.grb")
#'  X <- NAM218(url)
#'  RH <- X$get_layer("Relative_humidity")
#'  SWG <- X$get_layer("Surface_wind_gust")
#' }
NULL
