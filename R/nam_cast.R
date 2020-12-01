#' Retrieve the NAM-CAST inclusive threshold that defines 'recent' data
#'
#' @export
#' @return Date class object
namcast_threshold_date <- function() as.Date("2020-05-18", format = "%Y-%m-%d")

#' Retrieve the base url for either HTML, XML or OPENDAP resource
#'
#' As of 2020-05-18 the resources have been split into "model-nam218-old" and "model-nam218"
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
namcast_base_url <- function(type = c("html","thredds", "xml", "opendap")[2],
                             threshold = namcast_threshold_date(),
                             date = namcast_threshold_date() + c(-3,0)){
  if (!inherits(threshold, "Date")) threshold <- as.Date(threshold)
  if (!inherits(date, "Date")) date <- as.Date(date)
  old <- date < threshold

  uri <-switch(tolower(type[1]),
    "html"    = "https://www.ncei.noaa.gov/thredds/catalog/model-nam218/catalog.html",
    "thredds" = "https://www.ncei.noaa.gov/thredds/catalog/model-nam218/catalog.xml",
    "xml"     = "https://www.ncei.noaa.gov/thredds/catalog/model-nam218/catalog.xml",
                 "https://www.ncei.noaa.gov/thredds/dodsC/model-nam218")
  uri <- rep(uri, length(old))
  uri[old] <- gsub("model-nam218", "model-nam218-old", uri[old], fixed = TRUE)
  uri
}

#' Construct thredds base urls for provided date(s)
#'
#' Completed URLs have the form
#' "https://www.ncdc.noaa.gov/thredds/dodsC/nam218/202002/20200229/nam_218_20200229_0000_000.grb2"
#' which is the same as                  "base_url/YYYYmm/YYYYmmdd/nam_218_YYYYmmdd_ffff_aaa.grb2"
#'
#' @export
#' @param dates Date-class, one or more dates
#' @param ftime 4 character forecast period time stamp ('0000', '0006', etc.)
#' @param ahead 3 character cycle timestamp ('000' now cast, '003 three hours ahead, etc)
#' @param ... other arguments for \code{namanl_base_url}
#' @return character URLs one for each pairing of ftime and ahead
namcast_url <- function(dates = "2018-12-18",
                       ftime = c('0000', '0600','1200','1800'),
                       ahead = c('000','006'),
                       ...) {

  if (!inherits(dates, "Date")) dates <- as.Date(dates)
  if (inherits(ftime, "numeric")) ftime <- sprintf("%0.4i", ftime[1])
  if (inherits(ahead, "numeric")) ahead <- sprintf("%0.3i", ahead[1])


  base_url <- namcast_base_url(type = "xml", date = dates, ...)

  f <- rep(ftime, times = length(ahead))
  a <- rep(ahead, each = length(ftime))
  Ym <- format(dates, "%Y%m")
  Ymd <- format(dates, "%Y%m%d")

  filename <- sprintf("namanl_218_%s_%s_%s.grb2", Ymd, f, a)

  file.path(base_url, Ym, Ymd, filename)
}

#' Query the catalog at the specified URL for the specified date
#'
#' @export
#' @param date Date or character, cast-able to Date class
#' @param uri character, the base uri for the top level catalog
#' @param pattern character, one or more regular expressions used to match the
#'        to just one of the opendap resources.
#' @return relative URL for the resource or NA if not found
query_namcast_catalog <- function(date, uri,
                                 pattern = c(
                                   "^.*0000_000\\.grb2$",
                                   "^.*0000_006\\.grb2$",
                                   "^.*0600_000\\.grb2$",
                                   "^.*0600_006\\.grb2$",
                                   "^.*1200_000\\.grb2$",
                                   "^.*1200_006\\.grb2$",
                                   "^.*1800_000\\.grb2$",
                                   "^.*1800_006\\.grb2$")){
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
  ix <- mgrepl(pattern, dnames)
  if (!any(ix)) return(r)
  ix <- which(ix)
  duri <- sapply(seq_along(ix),
    function(i){
      D <- Ymd$get_datasets(dnames[ix[i]])[[1]]
      D$get_url()
    })
  duri
}


#' Query NAM-ANL catalog for opendap resources by date
#'
#' @export
#' @param dates Date or castable to Date, the date(s) to query
#' @param ftime character or integer, 4 digit forecast statement hour(s) (or castable to such)
#' @param ahead character or integer, 4 digit forecast ahead hours (or castable to such)
#' @param threshold Date or castable to Date, the division date betwene "old" and "recent"
#' @return one per input date, a URL for .grb2 (opendap) resources, possibly NA
query_namcast <- function(dates = c("2020-05-15", "2020-05-18"),
                         ftime = c("0000", "0600", "1200", "1800"),
                         ahead = c("000", "006"),
                         threshold = namcast_threshold_date()){

  if (FALSE){
    dates = namanl_threshold_date() + c(-3, 0)
    ftime = c("0000", "0600", "1200", "1800")
    ahead = c("000", "006")
    threshold = namcast_threshold_date()
  }
  if (!inherits(dates, "Date")) dates <- as.Date(dates)
  if (!inherits(threshold, "Date")) threshold <- as.Date(threshold[1])

  # Build the regular expressions for filename patterns
  # @param ftime one or more 4-digits forecast statement hours
  # @param ahead one or more 3 digit forecast hours
  # @return a vector of regular expressions on for each combo of ftime_ahead
  build_patterns <- function(ftime, ahead, ext = ".grb2"){
    pattern = "^.*%s_%s\\%s$"
    pp <- sapply(ftime,
           function(s){
             sapply(ahead,
                    function(a){
                      sprintf(pattern, s, a, ext)
                    })
           })
    as.vector(pp)
  }

  pattern <- build_patterns(ftime, ahead)

  # make uris for catalog and for opendap
  # search the catalogs
  thredds_uri <- namcast_base_url(type = "xml",
                                 threshold = threshold,
                                 date = dates)
  opendap_uri <- namcast_base_url(type = "opendap",
                                 threshold = threshold,
                                 date = dates)

  x <- sapply(seq_along(dates),
              function(i){
                query_namcast_catalog(dates[i], thredds_uri[i], pattern = pattern)
              }) %>%
    unlist()

  uri <- file.path(dirname(opendap_uri), x)
  uri[is.na(x)] <- NA
  uri
}


#' Open an ncdf4 connection to an NAM(cast) uri
#'
#' @export
#' @param uri character the URL
#' @param verbose logical, if TRUE voice warnings
#' @return either ncdf4 object or NULL
namcast_open <- function(uri, verbose = FALSE){

  x <- try(ncdf4::nc_open(uri))
  # the follwoing duplicates the error checking above... just because
  if (inherits(x,"try-error")){
    if (verbose) warning("nc_open fail:", uri)
    x <- NULL
  }
  x
}
