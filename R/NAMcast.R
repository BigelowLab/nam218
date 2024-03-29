NAMcastRefClass <- setRefClass("NAMcastRefClass",
    fields = list(
        NC = 'ANY',
        template = 'ANY',
        vardim = 'character',
        BB = 'ANY',
        proj = 'character',
        res = 'numeric',
        time = 'ANY'
        ), #fields
    methods = list(
        initialize = function(nc = NULL){
            if (!is.null(nc) && inherits(nc, 'ncdf4')){
                .self$field("NC", nc)
            } else {
                nc <- try(ncdf4::nc_open(nc))
                if (!inherits(nc, 'try-error')) .self$field("NC",nc)
            }
            if (.self$is_open()) {
                .self$field("vardim",ncvar_dim_name(.self$NC))
                .self$field("time", .self$get_time())
            }

            .self$proj <- c(
                # see http://www.gdal.org/gdalsrsinfo.html
                lcc = "+proj=lcc +lat_1=25 +lat_0=25 +lon_0=-95 +k_0=1 +x_0=0 +y_0=0 +a=6367470.21484375 +b=6367470.21484375 +units=km +no_defs",
                longlat =  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

            .self$template <- raster::raster(
                nrows=443,
                ncols=913,
                crs = .self$proj['longlat'],
                xmn = -152.878623,
                xmx = -49.47263081081,
                ymn = 12.219908,
                ymx = 61.20556254545,
                #ext = raster::extent(c(
                #    xmin = -152.878623,
                #    xmax = -49.47263081081,
                #    ymin = 12.219908,
                #    ymax = 61.20556254545)),
                vals=1)

            .self$res <- raster::res(.self$template)

            if (!is.null(.self$NC)){
                bb = c(
                    raster::xFromCol(.self$template, 1),
                    raster::xFromCol(.self$template, ncol(.self$template)),
                    raster::yFromRow(.self$template, nrow(.self$template)),
                    raster::yFromRow(.self$template, 1) )

                .self$BB <- bbox_to_polygon(bb, .self$proj['longlat'])
            }
        },
        finalize = function(){
            if (.self$is_open()) .self$close()
            },
        is_open = function(){
            !is.null(.self$NC) && inherits(.self$NC, 'ncdf4')
            },
        close = function(){ ncdf4::nc_close(.self$NC) },
        show = function(){
            cat("Reference Class:", classLabel(class(.self)), "\n")
            opn <- .self$is_open()
            cat("  state:", if(opn) "open" else "closed", "\n")
            if (opn){
                cat("file: ", .self$NC$file, "\n")
                v <- ncvarname_pretty(.self$NC)
                cat("\n VARS:\n")
                cat(paste("   ", v), sep = "\n")
                d <- ncdim_pretty(.self$NC)
                cat("\n  DIMS:\n")
                cat(paste("   ", d), sep = "\n")
            }
            }
    ) # methods
)

#' Compute the POSIXct timestamps
#'
#' As of summer 2020 time has been stored relative to the day of the forecast
#' embedded in the units of the time variable.  Prior to that it was relative to
#' 0001-01-01 which required a 2-day shift to account for some mistakes made
#' prior to current conventions.
#'
#' @name NAMcastRefClass_get_time
#' @param shift numeric, the raw day numbers and epoch conspire to cause a two day
#' difference from what is expected. So this param is used to correct that.
#' @seealso \url{https://github.com/dankelley/oce/issues/738}
#' @seealso \url{http://www.epic.noaa.gov/java/ncBrowse/mail/msg00059.html}
#' @return POSIXct vector or NULL
NAMcastRefClass$methods(
    get_time = function(shift = -2){
      if (!.self$is_open()) return(NULL)
      if (grepl("Hour since",.self$NC$dim$time$units)){
        t0 <- as.POSIXct(.self$NC$dim$time$units,
          format = "Hour since %Y-%m-%dT%H:%M:%SZ",tz = "UTC")
        tm <- t0 + .self$NC$dim$time$vals * 60 * 60
      } else {
        if (length(.self$time) ==0) return(NULL)
        secsperday <- 24 * 60 * 60
        tm <- as.POSIXct("1-01-01 00:00:0.0", tz = 'UTC') +
            (.self$NC$dim$time$vals + shift) * secsperday
      }
      return(tm)
    })

#' Convert from longlat bounding box to start, count for x and y
#'
#' @name NAMcastRefClass_bb_to_xy
#' @param bb SpatialPolygons object to project to Lamber Conformal Conic
#' @return list of start[x,y] and count[x,y], bbox (original), ext (Extent coordinates)
NAMcastRefClass$methods(
    bb_to_xy = function(bb){

        if (missing(bb)) bb <- .self$BB
        if (sp::proj4string(bb) != .self$proj['longlat']) {
            x <- sp::spTransform(bb, .self$proj['longlat'])
            b <- sp::bbox(x)
        } else {
            b <- sp::bbox(bb)
        }
        r <- .self$res/2
        xb <- find_interval(b[c(1,3)], .self$NC$dim$lon$val)
        xb[xb <= 0] <- 1
        yb <- find_interval(b[c(2,4)], .self$NC$dim$lat$val)
        yb[yb <= 0] <- 1

        bb <- b[c(1,3,2,4)]
        Ext <- bb + c(-r[1], r[1], -r[2], r[2])
        list(
            start = c( x = xb[1], y = yb[1] ),
            count = c( x = xb[2]-xb[1]+1, y = yb[2]-yb[1]+1 ),
            bbox = bb,
            ext =  Ext)
    } )

#' Retrieve a parameter
#'
#' @name NAMcastRefClass_get_layer
#' @param name the name of the layer
#' @param bb a 4-element vector as [xmin,xmax,ymin,ymax] defining
#'  the subset region. If NULL then the full extent of the
#'  NAMcastRefClass object is used.  Should be in the coordinate system
#'  identified by the \code{from_proj} argument - defaults to native.
#' @param from_proj character, indicates the input projection of bb
#'  \itemize{
#'      \item{native - the default, the native projection of the NAMcast dataset}
#'      \item{lcc - the lamber conic conformal}
#'      \item{longlat - the same as native}
#'      \item{other proj4string possibilities}
#'  }
#' @param to_proj character, indicates the desired output projection
#'  It has the same options as \code{from_proj}
#' @param ... further arguments for NAMcast_x_y_time() and NAMcast_x_y_something_time()
#' @return RasterLayer object or NULL
NAMcastRefClass$methods(
    get_layer = function(name = 'tmpsfc', bb = NULL,
        from_proj = c('native', 'lcc', 'longlat')[1],
        to_proj = c('native', 'lcc', 'longlat')[1],
        flip = 'y', ...){

    if (FALSE){
        # for devel purposes
        name = 'tmpsfc'
        bb = NULL
        from_proj = c('native', 'lcc', 'longlat')[1]
        to_proj = c('native', 'lcc', 'longlat')[1]
        flip = 'y'
    }


    if (is.null(bb)) bb <- as.vector(raster::extent(.self$BB))

    from_p <- switch(tolower(from_proj[1]),
        'longlat' = .self$proj['longlat'],
        'native' = .self$proj['longlat'],
        'lcc' = .self$proj['lcc'],
        from_proj)

    to_p <- switch(tolower(to_proj[1]),
        'longlat' = .self$proj['longlat'],
        'native' = .self$proj['longlat'],
        'lcc' = .self$proj['lcc'],
        to_proj)

    if(!(name[1] %in% names(.self$vardim))) {
        cat("variable not found:", name[1], "\n")
        return(NULL)
    }

    bb <- bbox_to_polygon(bb, proj = from_p)

    xy <- .self$bb_to_xy(bb)

    something <- x_y_something(.self$vardim[[name[1]]])
    x <- try(
        switch(.self$vardim[[name[1]]],
            'lon_lat_time' =
                NAMcast_x_y_time(.self, name[1], xy=xy, ...),
            'lon_lat_lev_time' =
                NAMcast_x_y_something_time(.self, name[1], xy=xy,...))
        )


    R <- NULL
    if (inherits(x, 'try-error')){
        cat("error encountered - returning NULL \n")
    } else if(is.null(x)){
        msg <- c(
            sprintf("variable %s requires %s dimensions", name[1], .self$vardim[[name[1]]]),
            sprintf("the function to extract %s is not defined yet",.self$vardim[[name[1]]]),
            "returning NULL")
        cat(msg, sep = "\n")
    } else {
        R <- raster::raster(t(x),
            xmn = xy$ext[1], xmx = xy$ext[2],
            ymn = xy$ext[3], ymx = xy$ext[4],
            crs = .self$proj['longlat'])
        if (tolower(flip[1]) == 'y') R <- raster::flip(R, 'y')
        if (to_p != .self$proj['longlat'])
            R <- raster::projectRaster(from = R, crs = to_p)
        names(R) <- name
    }
    return(R)
    })


#' Retrieve a stack of numerous layers for a single parameter
#'
#' This is a convenience wrapper around repeated \code{get_layer()} for a single
#'  parameter
#'
#' @name NAMcastRefClass_get_layers
#' @param ... parameters for \code{get_layer()}
#' @param times the time indices or POSIXct values, by default all times
#'  or a character flag of 'first', 'last', or 'all'
#' @return a RasterStack with Z set to time or NULL
NAMcastRefClass$methods(
    get_layers = function(..., times = 'all'){

        if (length(.self$time) == 0) return(NULL)

        if (inherits(times, 'POSIXt')){
            ix <- closest_index(.self$time, times)
        } else if (inherits(times, 'character')){
            ix <- switch(tolower(times[1]),
                'first' = 1,
                'last' = length(.self$time),
                seq_along(.self$time))
        } else {
            ix <- as.integer(times)
        }

        PP <- raster::stack(lapply(ix, function(i) .self$get_layer(..., time = i)))
        raster::setZ(PP, .self$time[ix])
    })
################################################################################

#' Create an instance of NAMcastRefClass
#'
#' @export
#' @param nc either a url, ncdf4 object or null
#' @return NAMcastRefClass object or NULL
NAMcast <- function(nc =
    format(Sys.Date(), "http://nomads.ncep.noaa.gov:80/dods/nam/nam%Y%m%d/nam_00z")){
    if (is.null(nc)) return(NULL)
    if (inherits(nc, 'character')){
        if (nam218_url_error(nc)){
            cat("error accessing URL:", nc, "\n")
            cat("please cross check uri\n")
            return(NULL)
        }
    }
    NAMcastRefClass$new(nc)
}

#' Retrieve a variable based upon [x,y,time] as a matrix
#'
#' @export
#' @param X NAMcastRefClass object
#' @param name character variable name
#' @param xy a two element list providing start and count values
#' @param time_index the default is to get just the first time index if there is
#   more than one to choose from
#' @param n_tries numeric, the number attempts to make before failing
#' @param sleep numeric, the number of seconds to wait after a fail
#' @param ... further arguments for \code{\link[ncdf4]{ncvar_get}}
#' @return numeric matrix or try-error
NAMcast_x_y_time <- function(X, name,
    xy = list(start = c(1,1), count = c(-1, -1)),
    time_index = 1,
    n_tries = 3,
    sleep = 10,
    ...){

    nt <- 1
    r <- try(log("this is a dummy error in NAMcast_x_y_time"), silent = TRUE)
    while(nt <= n_tries){
      r <- try(ncdf4::ncvar_get(X$NC, name,
                     start = c(xy$start, time_index[1]),
                     count = c(xy$count, 1), ...))
      if (!inherits(r, "try-error")){
        break
      } else {
        warning(sprintf("ncvar_get try %i of %i failed", nt, n_tries))
        Sys.sleep(10)
        nt <- nt + 1
      }
    } # while
    r
}

#' Retrieve a variable based upon [x,y,lev,time] as a matrix
#'
#' @export
#' @param X NAMcastRefClass object
#' @param name character variable name
#' @param xy a two element list providing start and count values
#' @param lev numeric the level to extract
#' @param time_index the default is to get just the first time index if there is
#   more than one to choose from
#' @param n_tries numeric, the number attempts to make before failing
#' @param sleep numeric, the number of seconds to wait after a fail
#' @param ... further arguments for \code{\link[ncdf4]{ncvar_get}}
#' @return numeric matrix or try-error
NAMcast_x_y_lev_time <- function(X, name,
    lev = 1000,
    time_index = 1,
    xy = list(start = c(1,1), count = c(-1, -1)),
    n_tries = 3,
    sleep = 10,
    ...){
    ix  <- closest_index(ncdim_get(X$NC, 'lev'), lev[1])
    nt <- 1
    r <- try(log("this is a dummy error in NAMcast_x_y_lev_time"), silent = TRUE)
    while(nt <= n_tries){
      r <- try(ncdf4::ncvar_get(X$NC, name,
          start = c(xy$start, ix, time_index[1]),
          count = c(xy$count,1,1), ...))
      if (!inherits(r, "try-error")){
        break
      } else {
        warning(sprintf("ncvar_get try %i of %i failed", nt, n_tries))
        Sys.sleep(10)
        nt <- nt + 1
      }
    } # while
    r
}
