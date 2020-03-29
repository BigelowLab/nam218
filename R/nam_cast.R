#' Retrieve the base url for either HTML or OPENDAP
#'
#' @export
#' @param type character, either "html" or "thredds" (default)
#' @return URL
namcast_base_url <- function(type = c("html","thredds")[2]){

  switch(tolower(type[1]),
         "html" = "https://www.ncdc.noaa.gov/thredds/catalog/nam218/catalog.html",
         "https://www.ncdc.noaa.gov/thredds/dodsC/nam218")
}

#' Construct thredds base urls for provided date(s)
#'
#' Completed URLs have the form
#' "https://www.ncdc.noaa.gov/thredds/dodsC/nam218/202002/20200229/nam_218_20200229_0000_000.grb2"
#' which is the same as                  "base_url/YYYYmm/YYYYmmdd/nam_218_YYYYmmdd_ffff_aaa.grb2"
#'
#' @export
#' @param dates Date-class, one or more dates
#' @param base_url character, the base URL
#' @param ftime 4 character forecast period time stamp ('0000', '0006', etc.)
#' @param ahead 3 character cycle timestamp ('000' now cast, '003 three hours ahead, etc)
#' @return character URLs one for each pairing of ftime and ahead
namcast_url <- function(dates = "2018-12-18",
                       base_url = namanl_base_url(),
                       ftime = c('0000', '0600','1200','1800'),
                       ahead = c('000','006')) {

  if (!inherits(dates, "Date")) dates <- as.Date(dates)
  if (inherits(ftime, "numeric")) ftime <- sprintf("%0.4i", ftime[1])
  if (inherits(ahead, "numeric")) ahead <- sprintf("%0.4i", ahead[1])

  f <- rep(ftime, times = length(ahead))
  a <- rep(ahead, each = length(ftime))
  Ym <- format(dates, "%Y%m")
  Ymd <- format(dates, "%Y%m%d")

  filename <- sprintf("namanl_218_%s_%s_%s.grb2", Ymd, f, a)

  file.path(base_url, Ym, Ymd, filename)
}

#' Open an ncdf4 connection to an NAM(cast) uri
#'
#' @export
#' @param uri character the URL
#' @param verbose logical, if TRUE voice warnings
#' @return either ncdf4 object or NULL
namcast_open <- function(uri, verbose = FALSE){

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
