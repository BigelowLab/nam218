#' Retrieve the base url for either HTML or OPENDAP
#'
#' @export
#' @param type character, either "html" or "thredds" (default)
#' @return URL
namanl_base_url <- function(type = c("html","thredds")[2]){

  switch(tolower(type[1]),
         "html" = "https://www.ncdc.noaa.gov/thredds/catalog/namanl/catalog.html",
         "https://www.ncdc.noaa.gov/thredds/dodsC/namanl")
}

#' Construct thredds base urls for provided date(s)
#'
#' Completed URLs have the form
#' "https://www.ncdc.noaa.gov/thredds/dodsC/namanl/201812/20181218/namanl_218_20181218_0000_000.grb2"
#' which is the same as                  "base_url/YYYYmm/YYYYmmdd/namanl_218_YYYYmmdd_ffff_aaa.grb2"
#'
#' @export
#' @param dates Date-class, one or more dates
#' @param base_url character, the base URL
#' @param ftime 4 character forecast period time stamp ('0000', '0006', etc.)
#' @param ahead 3 character cycle timestamp ('000' now cast, '003 three hours ahead, etc)
#' @return character URLs
namanl_url <- function(dates = "2018-12-18",
                       base_url = namanl_base_url(),
                       ftime = c('0000', '0600','1200','1800')[1],
                       ahead = c('000', '003', '006', '084')[1]) {

  if (!inherits(dates, "Date")) dates <- as.Date(dates)
  if (inherits(ftime, "numeric")) ftime <- sprintf("%0.4i", ftime[1])
  if (inherits(ahead, "numeric")) ahead <- sprintf("%0.4i", ahead[1])
  Ym <- format(dates, "%Y%m")
  Ymd <- format(dates, "%Y%m%d")

  filename <- sprintf("namanl_218_%s_%s_%s.grb2",
                      Ymd, ftime[1], ahead[1])

  file.path(base_url, Ym, Ymd, filename)
}



#' Open an ncdf4 connection to an NAMANL uri
#'
#' @export
#' @param uri character the URL
#' @param verbose logical, if TRUE voice warnings
#' @return either ncdf4 object or NULL
namanl_open <- function(uri, verbose = FALSE){

  # first see if the web page is available - cheap and quick
  htmluri <- paste0(uri,".html")
  ok <- crul::ok(htmluri)
  if (!ok){
    if (verbose) warning("opendap html fail:", htmluri)
    return(NULL)
  }
  x <- try(ncdf4::nc_open(uri))
  # the follwoing duplicates the error checking above... just because
  if (inherits(x,"try-error")){
    if (verbose) warning("nc_open fail:", uri)
    x <- NULL
  }
  x
}
