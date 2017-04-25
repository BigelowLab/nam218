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

#' From a two vectors of forecast period ('ftime') and forecast 
#'  ahead ('ahead') determine the number of seconds to the forecast period from 
#'  midnight
#'
#' @export
#' @param ftime one or more forecast periods in hhhh
#' @param ahead one or more ahead forecast times in hhh
#' @return numeric number of seconds into a day
nam218_time <- function(
    ftime = c("0000", "0600", "1200", "1800"), 
    ahead = c('000', '001', '003', '006')){
    
    stopifnot(length(ftime) == length(ahead))
    
    f <- as.numeric(substring(ftime, 1, 2)) + 
         as.numeric(substring(ftime, 3,4))/60
    a <- as.numeric(ahead)
    
    (f + a) * 3600
}

#' Decompose a NAM218 filename into constituent parts. 
#' The directory name is not retained.
#'
#' @export
#' @param x character vector
#' @return a tibble with the following
#' \itemize{
#'  \item{D POSIXct date}
#'  \item{Y character year as YYYY}
#'  \item{m character month as mm}
#'  \item{d character day as dd}
#'  \item{p character forecast period in hours as HHHH, aka 'ftime'}
#'  \item{a character forecast time ahead in hours as hhh, aka 'ahead'}
#'  \item{n character name of parameter}
#'  \item{f character the basename of the input file}
#' }
decompose_uri <- function(x = c(
    "https://nomads.ncdc.noaa.gov/thredds/dodsC/namanl/201702/20170225/namanl_218_20040331_1800_000.grb",
    "https://nomads.ncdc.noaa.gov/thredds/dodsC/namanl/201702/20170225/namanl_218_20040331_1800_006.grb")){
    
    f <- basename(x)
    ss <- strsplit(gsub(".grb", "", f, fixed = TRUE), "_", fixed = TRUE)
    D <- sapply(ss, '[[', 3)  # YYYYmmdd
    Y <- substring(D, 1,4)
    m <- substring(D, 5, 6)
    d <- substring(D, 7, 8)
    p <- sapply(ss, '[[', 4)  # HHHH
    a <- sapply(ss, '[[', 5)  # hhh
    t <- nam218_time(p,a)
    D <- as.POSIXct(paste(D, "00:00:00"), "%Y%m%d", tz = "UTC")
    tibble::data_frame(D, Y, m, d, t, p, a, f)
    
}


#' Perform grepl on multiple patterns; it's like  AND-ing or OR-ing successive grepl statements.
#' 
#' @export
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
query_nam218 <- function(what = c("analysis", "forecast")[1],
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
    date <- gsub("[-/]", "", date)
    
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


#' Query the latest forecast datasets
#'
#' @export
#' @param ftime character, the forecast time to retrieve
#' \itemize{
#'  \item{NA this disable filtering and gets the lot for the day}
#'  \item{'latest' get just the latest (the default)}
#'  \item{'0000', '0600', '1200''1800' one or more specific hours}
#'  }
#' @param ahead character, the 'forecast ahead' times as hhh.  This makes the
#'  most sense when the value of ftime points to one time, such as 'latest', 
#'      '0600', etc.
#'  \itemize{
#'  \item{NA if NA, the default then all are returned}
#'  \item{ any combination of 000, 001, 002, etc}
#'  }
#' @param a list of DatasetRefClass, possibly empty or NULL
query_latest_forecast <- function(
    ftime = c(NA, 'latest', '0000', '0600', '1200', '1800')[2],
    ahead = c(NA, '000', '006', '084')[1]){
        
    if (FALSE){
        ftime = NA
        ahead = NA
    }
    topuri <- "https://nomads.ncdc.noaa.gov/thredds/catalog/nam218/catalog.xml"
    
    Top <- threddscrawler::get_catalog(topuri)
    if (is.null(Top)) return(Top)
    
    CC <- Top$get_catalogs()
    if (is.null(CC)) return(CC)
    
    # the last should be the yyyymm directory we seek
    C1 <- CC[[length(CC)]]$get_catalog()
    if (length(C1) == 0) return(C1)

    C2 <- C1$get_catalogs()
    if (length(C2) == 0) return(C2)
    
    # presumably the latest by yyyymmdd
    C3 <- C2[[length(C2)]]$get_catalog()
    
    DD <- C3$get_datasets()
    if (is.null(DD)) return(DD)
    
    dd <- nam218::decompose_uri(sapply(DD, '[[', 'url'))
    
    # now we filter by name 
    # ftime = NA then get them all
    # latest = filter by the 'p' field
    # 0000, 0600, ... filter by one or more of those
    if (!any(sapply(ftime, is.na))){
        if ('latest' %in% ftime){
            mx <- sprintf("%0.4i", max(as.numeric(dd[['p']])) )
            dd <- dd %>%
                dplyr::filter(p %in% mx)
            DD <- DD[ dd[['f']] ]
        } else {
            dd <- dd %>%
                dplyr::filter(p %in% ftime)
            DD <- DD[ dd[['f']] ]
        }
    } 
    if (!any(sapply(ahead, is.na))){
        dd <- dd %>% 
            dplyr::filter(a %in% ahead)
        DD <- DD[ dd[['f']] ]
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
nam218_url <- function(X, 
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
nam218_browse <- function(X, what = c("opendap_form", "netcdfserver")[1]){
    stopifnot(inherits(X, 'DatasetsRefClass'))
    uri <- nam_url(X, what = what[1])
    httr::BROWSE(uri)
}

#' Retrieve a ncdf4 object - only if ncdf4 package is available
#' 
#' @export
#' @param X DatasetsRefClass object 
#' @return value returned by ncdf4::nc_open()
nam218_nc_open <- function(X){
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
nam218_download <- function(X, dest = NULL){
    stopifnot(inherits(X, 'DatasetsRefClass'))
    if (is.null(dest)) dest <- basename(X$url)
    utils::download.file(nam_url(X, "httpserver"), dest = dest, mode = 'wb')
}


#' Query the namcast resources 
#' 
#' @export
#' @param charcater uri, the base uri for NOMADS NCEP DODS NAM server
#' @param day POSIXct or character in the form of 'YYYYmmdd', Default to today()
#' @param ftime character or numeric, on or more of the forecast time one or
#'  more of [0, 6, 12, 18]
#' @return character vector of uri
query_namcast <- function(
    day = format(Sys.time(), "%Y%m%d"),
    ftime = c(0,6,12,18),
    uri = "http://nomads.ncep.noaa.gov/dods/nam.xml"){
    
    if (FALSE){
        day = format(Sys.time(), "%Y%m%d")
        ftime = c(0,6,12,18)
        uri = "http://nomads.ncep.noaa.gov/dods/nam.xml"
    }
        
    namcast_uri <- function(x, nm = c('dods','dds', 'das')[1]){
        xml2::xml_find_first(x, nm) %>% xml2::xml_text()
    }
    if (inherits(day, 'POSIXt')) day <- format(day, '%Y%m%d')
    ftime <- sprintf("nam_%0.2iz", as.numeric(ftime))
    #path <- file.path(uristub,
    #    paste0("nam", day),
    #    sprintf("nam_%0.2iz.xml", as.numeric(ftime))[1]
    
    x <- try(xml2::read_xml(uri))   
    if (inherits(x, 'try-error')){
        cat("unable to read path:", uri)
        return(NULL)
    }
    dd <- x %>% 
        xml2::xml_find_all('dataset') 
    if (length(dd) == 0){
        cat("no datasets available:", path)
        return(NULL)
    }    
    
    nm <- dd %>%
        xml2::xml_find_first("name") %>% 
        xml2::xml_text()
        
    ix <- nam218::mgrepl(c(paste0("nam",day),'nam_[0-9].z'), nm, op = '|')
    #ix <- grepl(file.path(paste0("nam",day),ftime), nm, fixed = TRUE)
    
    dd[ix] %>% namcast_uri()
}

