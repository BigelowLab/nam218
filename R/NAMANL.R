NAMANLRefClass <- setRefClass("NAMANLRefClass",
    fields = list(
        NC = 'ANY',
        proj = 'character',
        lccR = 'ANY',
        longlatR = 'ANY'
        ), #fields
    methods = list(
        initialize = function(nc = NULL, proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"){
            .self$field("proj", proj[1])
            if (!is.null(nc) && inherits(nc, 'ncdf4')){
                .self$field("NC", nc)
            } else {
                .self$field("NC", try(ncdf4::nc_open(nc)))
            }
            
            lccProj <- "+proj=lcc +lat_1=25 +lat_2=25 +lat_0=25 +lon_0=-95 +x_0=0 +y_0=0 +a=6371200 +b=6371200 +units=m +no_defs"
            longlatProj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
         
            .self$lccR <- raster::raster(
                nrows=428, 
                ncols=614, 
                crs = lccProj, 
                ext = raster::extent(c(
                    xmin = -4232183.26089321,
                    xmax = 3253090.73910679,
                    ymin = -838789.970814831,
                    ymax = 4378958.02918517)), 
                resolution = c(12191, 12191), 
                vals=1)
          
            .self$longlatR <- raster::projectRaster(from = lccR,
                crs = longlatProj)

            },
        finalize = function(){
            if (.self$is_open()) .self$close()
            },
        is_open = function(){
            !is.null(.self$NC) && inherits(.self$NC, 'ncdf4')
            },
        close = function(){ nc_close(.self$NC) },
        show = function(){
            cat("Reference Class:", classLabel(class(.self)), "\n")
            opn <- .self$is_open()
            cat("  state:", if(opn) "open" else "closed", "\n")
            
            if (opn){
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

#' Convert a matrix into a RasterLayer object
#'
#' @name NAMANLRefClass_RasterLayer
#' @param x matrix
#' @param name character name of the layer
#' @param flip_it character ('y' the default, 'x' or '') for flip-y, flip-x or no-flip
#' @return RasterLayer object 
NAMANLRefClass$methods(
    RasterLayer = function(x, name = 'layer', flip_it = 'y'){
            R <- raster::raster(t(x), template = .self$lccR)
            R <- raster::projectRaster(from = R, to = X$longlatR)
            names(R) <- name
            if (nchar(flip_it) > 0) R <- raster::flip(R, 'y')
        })

#' Retrieve a parameter
#' 
#' @name NAMANLRefClass_get_layer
#' @param name the name of the layer
#' @param ... further arguments for the parameter
#' @return RasterLayer object or NULL
NAMANLRefClass$methods(
    get_layer = function(name = 'Temperature'){
    
    if(!(name[1] %in% ncvarname_get(.self$NC))) {
        cat("variable not found:", name[1], "\n")
        return(NULL)
    }
    
    x <- try(eval(call(name[1], .self)))
    if (inherits(x, 'try-error')){
        cat("that function is not defined:", name[1], "\n")
        R <- NULL
    } else {
        R <- .self$RasterLayer(x, name = name[1])
    }
    return(R)
    })
    

######## METHODS ABOVE
######## FUNCTIONS BELOW

#' Create an instance of NAMANLRefClass
#'
#' @export
#' @param nc either a path, url, ncdf4 object or null
#' @param proj projection string for automatic transformation from NAM-ANL's native 'lcc' projection
#' @return NAMANLRefClass object
NAMANL <- function(nc, proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"){
    NAMANLRefClass$new(nc, proj = proj)
}

# Temperature [x,y,isobaric,time]
Temperature <- function(X, isobaric = 1000){
    ix  <- which.min(abs(ncdim_get(X$NC, "isobaric")- isobaric[1]))
    ncdf4::ncvar_get(X$NC, "Temperature", count = c(-1,-1,ix,-1))
}

Relative_humidity <- function(X, isobaric = 1000){
    ix  <- which.min(abs(ncdim_get(X$NC, "isobaric")- isobaric[1]))
    ncdf4::ncvar_get(X$NC, "Relative_humidity", count = c(-1,-1,ix,-1))
}

# Surface_wind_gust[x,y,time]
Surface_wind_gust <- function(X){
    ncdf4::ncvar_get(X$NC, "Surface_wind_gust")
}

# Total_precipitation [x,y,time1]
Total_precipitation <- function(X){
    ncdf4::ncvar_get(X$NC, "Total_precipitation")
}