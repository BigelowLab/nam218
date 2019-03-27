# nc.R

#' Test if a NAM218 OpenDAP URL points to accessible datasets.
#'
#' @export
#' @param x character URL or DatasetsRefClass object
#' @return logical TRUE if the URL fails to point to an available dataset
nam218_url_error <- function(x){
    if (length(x) > 1) return(sapply(x, httr::http_error))
    if (inherits(x, 'DatasetsRefClass')) x <- nam218_url(x)
    httr::http_error(paste0(x, ".html"))
}


#' Retrieve a named list of global attributes
#'
#' @export
#' @param NC a ncdf4 object
#' @param rm_pattern character a pattern of characters to remove from the
#'    attribute names.  By default 'NC_GLOBAL.'.  Set to "" or NA to skip
#' @param fixed logical by default TRUE but see \code{grepl}
#' @return named vector of global attributes
ncglobal_atts <- function(NC, rm_pattern = 'NC_GLOBAL.', fixed = TRUE){
   d <- if (!is.null(NC)) ncdf4::ncatt_get(NC, varid = 0) else NULL
   if (!is.null(d)){
      if (!is.na(rm_pattern) && (nchar(rm_pattern) > 0)){
         names(d) <- gsub("NC_GLOBAL.", "", names(d), fixed = TRUE)
      }
   }
   return(d)
}

#' Retrieve a list defining a single dimension
#'
#' @export
#' @param NC ncdf4 object
#' @param dimname character the name of the dimension
#' @param what character, an optional subelement to return
#' @return a lst of dimension elements
ncdim_get <- function(NC, dimname = 'some dimension name',
    what = c('vals', 'units', NA)[1]){
    x <- NC$dim[[dimname[1]]]
    if (!is.na(what) && !is.null(x)) x <- x[[what[1]]]
    x
}

#' Retrieve a vector of pretty dimensions
#'
#' @export
#' @param NC a ncdf4 object
#' @return a named vector of dimesnions
ncdim_list <- function(NC){
   stopifnot('dim' %in% names(NC))
   sapply(NC[['dim']], '[[', 'len')
}

#' Retrieve a vector of pretty dimensions
#'
#' @export
#' @param NC a ncdf4 object
#' @return a named vector of dimesnions
ncdim_pretty <- function(NC){
   stopifnot('dim' %in% names(NC))
   x <- sapply(NC[['dim']], '[[', 'len')
   sprintf("%s[%i]", names(x), x)
}

#' Make a pretty list of variables "varname [dim1,dim2,...]"
#'
#' Adapted from ncdf4::print.ncdf4()
#' @export
#' @param nc ncdf4 object
#' @param rm.names logical, if TRUE remove element names
#' @return character vector of prettified varnames
ncvarname_pretty <- function(nc, rm.names = TRUE){
        ss <- sapply(nc[['var']],
            function(x){
                s <- x$name
                nd <- x$ndims
                if( x$ndims > 0 ) {
                    d <- sapply(x$dim, "[[", 'name')
                    s <- sprintf("%s [%s]",s, paste(d, collapse = ","))
                }
                s
            })
        if (rm.names) ss <- unname(ss)
        ss
}

#' Retrieve a named vector of variable dimensions as a string.  If a variable
#'  has dimensions [x, y, isobaric, time] then the value will be
#'  'x_y_isobaric_time'.
#'
#' @export
#' @param nc a ncdf4 object
#' @param sep character used to separate the dimension names
#' @return a named vector of variable dimension names
ncvar_dim_name <- function(nc,sep = "_"){
    ss <- sapply(nc[['var']],
            function(x){
                nd <- x$ndims
                if( x$ndims > 0 ) {
                    s <- paste(sapply(x$dim, "[[", 'name'), collapse = sep)
                } else {
                    s =""
                }
                s
            }
        )
    ss
}



#' Retrieve a vector of variable names
#'
#' @export
#' @param NC a ncdf4 object
#' @return a named vector variable names
ncvarname_get <- function(NC){
   stopifnot('dim' %in% names(NC))
   names(NC[['var']])
}


#' Retrieve a list of dimension vectors, one for each variable
#'
#' @export
#' @param NC a ncdf4 object
#' @return a named list of variable dimension vectors
ncvardim_get <- function(NC){
   stopifnot('dim' %in% names(NC))
   vn <- names(NC[['var']])
   names(vn) <- vn
   get_vardim <- function(nm, NC = NULL){
      d <- NC[['var']][[nm]]
      get_vardim_one <- function(x){
         len <- x[['len']]
         names(len) <- x[['name']]
         return(len)
      }

      dims <- d[['dim']]
      sapply(dims, get_vardim_one)
   }
   lapply(vn, get_vardim, NC = NC)
}

#' Retrieve a vector of timestamps for a multilayer NC object or NULL otherwise
#'
#' @export
#' @param NC a ncdf4 class object
#' @param name the name of the time-associated variable, by default 'time'
#' @param as_POSIXct logical, if TRUE then convert to POSIXct
#' @return a numeric vector of timestamps (possibly POSIXct) or NULL if none
nctime_get <- function(NC, name = 'time', as_POSIXct = TRUE){

   d <- ncdim_get(NC)
   if (!("time" %in% names(d)) ) return(NULL)

   v <- NC[["dim"]][['time']][['vals']]

   if (as_POSIXct){
      u <- NC[["dim"]][['time']][['units']]

      if (grepl(" since ", u, fixed = TRUE)){
         secsperday <- 24 * 60 * 60
         spaces <- gregexpr(" since ", u, fixed = TRUE)[[1]]
         incr <- substring(u, 1, spaces[1]-1)
         dt <- substring(u, spaces[1] + attr(spaces, 'match.length'))
         t0 <- as.POSIXct(dt, tz = "UTC")
         v <- switch(incr,
            "days" =  t0 + (v * secsperday),
            "seconds" = t0 + v,
            t0 + v)
      } else {
         cat("nctime_get: unknown time format for conversion to POSIXct\n")
      }
   }
   invisible(v)
}


#' Retrieve a matrix generic information about each variable
#'
#' @export
#' @param NC a ncdf4 class object
#' @param sep character, used to separate dimensions if any, deafult is a space " "
#' @return a character matrix of
#'  \itemize{
#'  \item{longname the name of the variable}
#'  \item{name the shortname of the variable}
#'  \item{unit the units - possibly ""}
#'  \item{dim the dimensions separated by underscores possibly ""}
#' }
ncvar_info <- function(NC, sep = " "){
    name <- names(NC$var)
    unit  <- sapply(name, function(vn) NC$var[[vn]][['units']])
    longname  <- sapply(name, function(vn) NC$var[[vn]][['longname']])
    shortname  <- sapply(name, function(vn) NC$var[[vn]][['name']])
    dim   <- ncvar_dim_name(NC, sep = sep)
    cbind(name, longname, shortname, unit, dim)
}
