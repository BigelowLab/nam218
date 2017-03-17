# http://www.nco.ncep.noaa.gov/pmb/docs/on388/tableb.html#GRID218
# Grid over the Contiguous United States (used by the 12-km NAM Model) (Lambert Conformal)
# 	
# Nx 	614
# Ny 	428
# La1 	12.190N
# Lo1 	226.514E = 133.459W
# Res. & Comp. Flag 	0 0 0 0 1 0 0 0
# Lov 	265.000E = 95.000W
# Dx 	12.19058 km
# Dy 	12.19058 km
# Projection Flag (bit 1) 	0 (not biPolar)
# Scanning Mode (bits 1 2 3) 	0 1 0
# Lat/Lon values of the corners of the grid
# (1,1) 	12.190N, 133.459W
# (1,428) 	54.564N, 152.878W
# (614,428) 	57.328N, 49.420W
# (614,1) 	14.342N, 65.127W
# Pole point
# (I,J) 	(347.668, 1190.097)

# example thredds
# https://nomads.ncdc.noaa.gov/thredds/dodsC/namanl/200807/20080704/namanl_218_20080704_1200_000.grb

#' Perform grepl on multiple patterns; it's like  AND-ing or OR-ing successive grepl statements.
#' 
#' @param pattern character vector of patterns
#' @param x the character vector to search
#' @param op logical vector operator back quoted, defaults to `|`
#' @param ... further arguments for \code{grepl} like \code{fixed} etc.
#' @return logical vector
mgrepl <- function(pattern, x, op = `|`, ... ){
   Reduce(op, lapply(pattern, grepl, x, ...))
}

#' Run a query for NAM-218 datasets
#'
#' @export
#' @param what character specifies 'analysis' or 'forecast'
#' @param date either POSIXct or an 8 character date (YYYYmmdd)
#' @param ftime 4 character forecast period time stamp ('0000', '0006', etc.) or NA
#' @param ahead 3 character cycle timestamp ('000' now cast, '003 three hours ahead, etc) or NA
#' @return DatasetsRefClass or an empty list or NULL 
nam218_query <- function(what = c("analysis", "forecast")[1],
    date = c('20060601', format(as.POSIXct(Sys.time(), format = "%Y%m%d")))[1] ,
    ftime =  c(NA, '0000', '0600','1200','1800')[1],
    ahead = c(NA, '000', '003', '006', '084')[1]){
    
    if (FALSE){
        what = c("analysis", "forecast")[1]
        date = c('20150101', format(as.POSIXct(Sys.time(), format = "%Y%m%d")))[1]
        ftime =  c(NA, '0000', '0600','1200','1800')[1]
        ahead = c(NA, '000', '003', '006', '084')[1]
    }
    
    what <- tolower(what[1])
    topuri <- switch(what,
        'analysis' = "https://nomads.ncdc.noaa.gov/thredds/catalog/namanl/catalog.xml",
        'forecast' = "https://nomads.ncdc.noaa.gov/thredds/catalog/nam218/catalog.xml",
        stop("what must be 'analysis' or 'forecast'") )
        
    if (inherits(date, "POSIXt")) date <- format(date[1], format = "%Y%m%d")
    
    Top <- threddscrawler::get_catalog(topuri)
    if (is.null(Top)) return(Top)
    
    CC <- Top$get_catalogs()
    if (is.null(CC)) return(CC)
    
    yyyymm <- substring(date[1], 1,6)
    C1 <- CC[[yyyymm]]$get_catalog()
    if (is.null(C1)) return(C1)
    
    C2 <- C1$get_catalogs()[[date]]
    if (is.null(C2)) return(C2)
    
    C3 <- C2$get_catalog()
    if (is.null(C3)) return(C3)
    
    DD <- C3$get_datasets()
    if (is.null(DD)) return(DD)
    
    # example names
    # "namanl_218_20150101_0000_000.grb" "namanl_218_20150101_0000_001.grb"
    if (length(DD) > 0){
        if (!all(is.na(ftime))){
             pattern <- sprintf("_%0.4i_", as.numeric(ftime))
             ix <- nam218:::mgrepl(pattern, names(DD), fixed = TRUE)
             if (any(ix)) DD <- DD[ix]
        }
    }
    if (length(DD) > 0){
        if (!all(is.na(ahead))){
             pattern <- sprintf("_%0.3i.grb", as.numeric(ahead))
             ix <- nam218:::mgrepl(pattern, names(DD), fixed = TRUE)
             if (any(ix)) DD <- DD[ix]
        }
    }
    DD
}



#' Run a query for NAM-218 datasets
#'
#' @export
#' @param what character specifies 'analysis' or 'forecast'
#' @param day either POSIXct or an 8 character date (YYYYmmdd)
#' @param time 4 character forecast period time stamp ('0000', '0006', etc.)
#' @param ahead 3 character cycle timestamp ('000' now cast, '003 three hours ahead, etc)
#' @return DatasetsRefClass or NULL 
nam_query <- function(what = c("analysis", "forecast")[1],
    day = c('20060601', format(as.POSIXct(Sys.time(), format = "%Y%m%d")))[1] , 
    time = c(NA, '0000', '0600','1200','1800')[3],
    ahead = c(NA, '000', '003', '006', '084')[1]
    ){
    
    what <- tolower(what[1])
    topuri <- switch(what,
        'analysis' = "https://nomads.ncdc.noaa.gov/thredds/catalog/namanl/catalog.xml",
        'forecast' = "https://nomads.ncdc.noaa.gov/thredds/catalog/nam218/catalog.xml",
        stop("what must be 'analysis' or 'forecast'") )
    
    if (inherits(time, 'numeric')) time <- sprintf("%0.4i", time[1])
    if (inherits(ahead, 'numeric')) ahead <- sprintf("%0.3i", ahead[1])
    
    Top <- threddscrawler::get_catalog(topuri)
    CC <- Top$get_catalogs()
    if (inherits(day, "POSIXt")) day <- format(day[1], format = "%Y%m%d")
    yyyymm <- substring(day[1], 1,6)
    C1 <- CC[[yyyymm]]$get_catalog()
    C2 <- C1$get_catalogs()[[date]]
    C3 <- C2$get_catalog()
    DD <- C3$get_datasets()
    ix <- grepl(paste0(time[1],'_',ahead[1]), names(DD), fixed = TRUE)
    if (any(ix)) {
        x <- DD[ix][[1]]
    } else {
        x <- NULL
    }
}
 
#' Retrieve a URL for various resources
#'
#' @export
#' @param X DatasetsRefClass object
#' @param what character - one of following
#' \itemize{
#'  \item{OPeNDAP for ncdf::nc_open()}
#'  \item{OPeNDAP_form for httr::BROWSE() OPeNDAP Dataset Access Form}
#'  \item{HTTPServer for utils::download.file()}
#'  \item{WCS not sure what one does with this}
#'  \item{WMS ditto}
#'  \item{NetcdfServer for httr::BROWSE() NetCDF Subset Service for Grids (interactive)}
#'  } 
#' @return character, URL
nam_url <- function(X, 
    what = c("OPeNDAP",     # for ncdf::nc_open()
        "OPeNDAP_form",     # for httr::BROWSE() OPeNDAP Dataset Access Form
        "HTTPServer",       # for utils::download.file()
        "WCS",              # not sure what one does with this
        "WMS",              # ditto
        "NetcdfServer")[1]  # for httr::BROWSE() NetCDF Subset Service for Grids (interactive) 
        ){
        
    stopifnot(inherits(X, 'DatasetsRefClass'))
    wh <- tolower(what[1])
        #> orig
        #http://nomads.ncdc.noaa.gov/thredds/catalog/namanl/200606/20060601/namanl_218_20060601_0000_000.grb
    switch(wh,
            # OPENDAP  thredds -> dodsC
            # http://nomads.ncdc.noaa.gov/dodsC/catalog/namanl/200606/20060601/namanl_218_20060601_0000_000.grb
        "opendap"  =  sub("catalog", "dodsC", X$url, fixed = TRUE),
            # OPENDAP form
            # http://nomads.ncdc.noaa.gov/thredds/dodsC/namanl/200606/20060601/namanl_218_20060601_0000_000.grb.html
        "opendap_form" = paste0(sub("catalog", "dodsC", X$url,fixed = TRUE), ".html"),
            # http://nomads.ncdc.noaa.gov/thredds/fileServer/namanl/200606/20060601/namanl_218_20060601_0000_000.grb
        "httpserver" = sub("catalog", "fileServer", X$url, fixed = TRUE),
            # http://nomads.ncdc.noaa.gov/thredds/wms/namanl/200606/20060601/namanl_218_20060601_0000_000.grb?service=WMS&version=1.3.0&request=GetCapabilities
        "wms"  =  paste0(sub("catalog", "wms", X$url,fixed = TRUE),
            "?service=WMS&version=1.3.0&request=GetCapabilities"),
            #http://nomads.ncdc.noaa.gov/thredds/ncss/grid/namanl/200606/20060601/namanl_218_20060601_0000_000.grb/dataset.html"
        "netcdfserver" =  paste0(gsub("catalog", "ncss/grid", X$url, fixed = TRUE),
            "/dataset.html"),
        "" ) 
}

#' Browse the OPeNDAP resources online
#' 
#' @export
#' @param X DatasetsRefClass object
#' @param what character, which format do you prefer?
#' @return the value of httr::BROWSE
nam_browse <- function(X, what = c("opendap_form", "netcdfserver")[1]){
    stopifnot(inherits(X, 'DatasetsRefClass'))
    uri <- nam_url(X, what = what[1])
    httr::BROWSE(uri)
}

#' Retrieve a ncdf4 object - only if ncdf4 package is available
#' 
#' @export
#' @param X DatasetsRefClass object 
#' @return value returned by ncdf4::nc_open()
nam_nc_open <- function(X){
    stopifnot(inherits(X, 'DatasetsRefClass'))

    stopifnot(require(ncdf4))
    ncdf4::nc_open(nam_url(X, "opendap"))
}

#' Download a GRIB file
#' 
#' @export
#' @param X DatasetsRefClass object
#' @param dest the destination file name, if not provided then it is borrowed from X
#' @return value returned by utils::download.file()
nam_download <- function(X, dest = NULL){
    stopifnot(inherits(X, 'DatasetsRefClass'))
    if (is.null(dest)) dest <- basename(X$url)
    utils::download.file(nam_url(X, "httpserver"), dest = dest, mode = 'wb')
}