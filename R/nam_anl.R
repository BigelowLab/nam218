#' Retrieve the NAM-ANL inclusive threshold that defines 'recent' data
#'
#' @export
#' @return Date class object
namanl_threshold_date <- function() as.Date("2020-05-18", format = "%Y-%m-%d")

#' Retrieve the base url for either HTML, XML or OPENDAP resource
#'
#' As of 2020-05-18 the resources have been split into "model-namanl-old" and "model-namanl"
#' with 2 days missing (2020-05-16 and 2020-05-17).  I wish they had simply made the catalog
#' have a year-level instead of an arbitrary threshold.
#'
#' @export
#' @param type character, one of the following
#' \itemize{
#' \item{html - to get a URL for the human readable catalog page}
#' \item{thredds - to get the URL for the XML catalog page - this is the default}
#' \item{xml - same as \code{thredds} to get the URL for the XML catalog page}
#' \item{opendap - to get the URL to the opendap (grib2) resource}
#' }
#' @param threshold Date or cast-able as Date, inclusive threshold identifies start of 'recent' records
#' @param date night Date or cast-able to Date, the date(s) to search for. The default is
#'        a 2 element vector with the last 'old' and first 'recent' dates.
#' @return URL
namanl_base_url <- function(type = c("html","thredds", "xml", "opendap")[2],
                            threshold = namanl_threshold_date(),
                            date = threshold - c(3,0)){

  if (!inherits(threshold, "Date")) threshold <- as.Date(threshold)
  if (!inherits(date, "Date")) date <- as.Date(date)
  old <- date < threshold

  uri <- sapply(seq_along(date),
                function(i){
                switch(tolower(type[1]),
                  "html"    = "https://www.ncdc.noaa.gov/thredds/catalog/model-namanl/catalog.html",
                  "xml"     = "https://www.ncdc.noaa.gov/thredds/catalog/model-namanl/catalog.xml",
                  "thredds" = "https://www.ncdc.noaa.gov/thredds/catalog/model-namanl/catalog.xml",
                              "https://www.ncdc.noaa.gov/thredds/dodsC/model-namanl")
                }, simplify = TRUE)
  uri[old] <- gsub("model-namanl", "model-namanl-old", uri[old], fixed = TRUE)
  uri
}

#' Construct thredds base urls for provided date(s)
#'
#' Completed URLs have the form
#' "https://www.ncdc.noaa.gov/thredds/dodsC/namanl/201812/20181218/namanl_218_20181218_0000_000.grb2"
#' which is the same as                  "base_url/YYYYmm/YYYYmmdd/namanl_218_YYYYmmdd_ffff_aaa.grb2"
#'
#' @export
#' @param dates Date-class, one or more dates
#' @param ftime 4 character forecast period time stamps ('0000', '0006', etc.)
#' @param ahead 3 character cycle timestamps ('000' now cast, '003 three hours ahead, etc)
#' @param ... other arguments for \code{namanl_base_url}
#' @return character URLs,  one for each pairing of ftime and ahead
namanl_url <- function(dates = "2018-12-18",
                       ftime = c('0000', '0600','1200','1800'),
                       ahead = c('000','006'),
                       ...) {

  base_url = namanl_base_url(type = "opendap", date = dates, ...)
  if (!inherits(dates, "Date")) dates <- as.Date(dates)
  if (inherits(ftime, "numeric")) ftime <- sprintf("%0.4i", ftime[1])
  if (inherits(ahead, "numeric")) ahead <- sprintf("%0.3i", ahead[1])

  f <- rep(ftime, times = length(ahead))
  a <- rep(ahead, each = length(ftime))
  Ym <- format(dates, "%Y%m")
  Ymd <- format(dates, "%Y%m%d")

  filename <- sprintf("namanl_218_%s_%s_%s.grb2",Ymd, f, a)

  file.path(base_url, Ym, Ymd, filename)
}

#' Query the catalog at the specified URL for the specified date
#'
#' @export
#' @param date Date or character, cast-able to Date class
#' @param uri character, the base uri for the top level catalog
#' @param pattern character, the regular expression used to match the
#'        to just one of the opendap resources.
#' @return relative URL for the resource or NA if not found
query_namanl_catalog <- function(date, uri,
                                 pattern = "^.*0000_000\\.grb2$"){
  r <- NA_character_
  Top <- thredds::get_catalog(uri)
  if (is.null(Top)) return(r)
  if (!inherits(date, "Date")) date <- as.Date(date)
  ym <- format(date[1], "%Y%m")
  Ym <- Top$get_catalogs(ym)[[ym]]
  if (is.null(Ym)) return(r)
  ymd <- format(date[1], "%Y%m%d")
  Ymd <- Ym$get_catalogs(ymd)[[ymd]]
  if (is.null(Ymd)) return(r)
  dnames <- Ymd$get_dataset_names()
  ix <- grepl(pattern, dnames)
  D <- Ymd$get_datasets(dnames[ix][1])[[dnames[ix][1]]]
  if (is.null(D)) return(r)
  D$get_url()
}


#' Query NAM-ANL catalog for opendap resources by date
#'
#' @export
#' @param dates Date or castable to Date, the date(s) to query
#' @param ftime character or integer, 4 digit forecast hour (or castable to such)
#' @param ahead character or integer, 4 digit forecast ahead hours (or castable to such)
#' @param threshold Date or castable to Date, the division date betwene "old" and "recent"
#' @return one per input date, a URL for .grb2 (opendap) resources, possibly NA
query_namanl <- function(dates = c("2020-05-15", "2020-05-18"),
                         ftime = "0000",
                         ahead = "000",
                         threshold = namanl_threshold_date()){

  if (FALSE){
    dates = namanl_threshold_date() + c(-3, 0)
    ftime = "0000"
    ahead = "000"
    threshold = namanl_threshold_date()
  }
  if (!inherits(dates, "Date")) dates <- as.Date(dates)
  if (!inherits(threshold, "Date")) threshold <- as.Date(threshold[1])
  pattern <- sprintf("^.*%s_%s\\.grb2$", ftime[1], ahead[1])

  # make uris for catalog and for opendap
  # search the catalogs
  thredds_uri <- namanl_base_url(type = "xml",
                                 threshold = threshold,
                                 date = dates)
  opendap_uri <- namanl_base_url(type = "opendap",
                                 threshold = threshold,
                                 date = dates)

  x <- sapply(seq_along(dates),
              function(i){
                query_namanl_catalog(dates[i], thredds_uri[i], pattern = pattern)
              })
  uri <- file.path(dirname(opendap_uri), x)
  uri[is.na(x)] <- NA
  uri
}


#' Open an ncdf4 connection to an NAM-ANL uri
#'
#' @export
#' @param uri character the URL
#' @param verbose logical, if TRUE voice warnings
#' @return either ncdf4 object or NULL
namanl_open <- function(uri, verbose = FALSE){

  x <- try(ncdf4::nc_open(uri))
  # the following duplicates the error checking above... just because
  if (inherits(x,"try-error")){
    if (verbose) warning("nc_open fail:", uri)
    x <- NULL
  }
  x
}
