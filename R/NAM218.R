NAM218RefClass <- setRefClass("NAM218RefClass",
    fields = list(
        NC = 'ANY',
        lccR = 'ANY', 
        vardim = 'character',
        BB = 'ANY',
        proj = 'character'
        ), #fields
    methods = list(
        initialize = function(nc = NULL){
            if (!is.null(nc) && inherits(nc, 'ncdf4')){
                .self$field("NC", nc)
            } else {
                 nc <- try(ncdf4::nc_open(nc))
                if (!inherits(nc, 'try-error')) .self$field("NC",nc)
            }
            if (!is.null(.self$NC)) {
                .self$field("vardim",ncvar_dim_name(.self$NC))
            }
            
            .self$proj <- c(
                lcc = "+proj=lcc +lat_1=25 +lon_0=-95 +lat_0=25 +ellps=WGS84 +lat_2=45 +units=km",
                longlat =  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
         
            .self$lccR <- raster::raster(
                nrows=428, 
                ncols=614, 
                crs = .self$proj['lcc'], 
                ext = raster::extent(c(
                    xmin = -4232.20247114135,
                    xmax = 3253.07152885866,
                    ymin = -838.793818334546,
                    ymax = 4378.95418166546)), 
                vals=1)
            
            if (!is.null(.self$NC)){
                bb = c(
                    raster::xFromCol(.self$lccR, 1),
                    raster::xFromCol(.self$lccR, ncol(.self$lccR)),
                    raster::yFromRow(.self$lccR, nrow(.self$lccR)),
                    raster::yFromRow(.self$lccR, 1) )
                
                .self$BB <- bbox_to_polygon(bb, .self$proj['lcc'])
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


#' Convert from longlat bounding box to start, count for x and y 
#'
#' @name NAM218RefClass_bb_to_xy
#' @param bb SpatialPolygons object to project to Lamber Conformal Conic
#' @return list of start[x,y] and count[x,y]
NAM218RefClass$methods(
    bb_to_xy = function(bb){
    
        if (missing(bb)) bb <- .self$BB
        if (sp::proj4string(bb) != .self$proj['lcc']) {
            x <- sp::spTransform(bb, .self$proj['lcc'])
            b <- sp::bbox(x)
        } else {
            b <- sp::bbox(bb)
        }
        
        xb <- find_interval(b[c(1,3)], .self$NC$dim$x$val)
        xb[xb <= 0] <- 1
        yb <- find_interval(b[c(2,4)], .self$NC$dim$y$val)
        yb[yb <= 0] <- 1        
        list(
            start = c( x = xb[1], y = yb[1] ),
            count = c( x = xb[2]-xb[1]+1, y = yb[2]-yb[1]+1 ),
            bbox = b[c(1,3,2,4)] ) 
    } )   
        
        

#' Retrieve a parameter
#' 
#' @name NAM218RefClass_get_layer
#' @param name the name of the layer
#' @param bb a 4-element vector as [xmin,xmax,ymin,ymax] defining 
#'  the subset region. If NULL then the full extent of the  
#'  NAM218RefClass object is used.  Should be in the coordinate system
#'  identified by the \code{from_proj} argument - defaults to native.
#' @param from_proj character, indicates the input projection of bb
#'  \itemize{
#'      \item{native - the default, the native projection of the NAM218 dataset}
#'      \item{lcc - the same as native}
#'      \item{longlat - use longlat projection}
#'      \item{other proj4string possibilities}
#'  }
#' @param to_proj character, indicates the desired output projection
#'  It has the same options as \code{from_proj}
#' @return RasterLayer object or NULL
NAM218RefClass$methods(
    get_layer = function(name = 'Temperature', bb = NULL, 
        from_proj = c('native', 'lcc', 'longlat')[1],
        to_proj = c('native', 'lcc', 'longlat')[1],
        flip = 'y', ...){
    
    if (is.null(bb)) bb <- as.vector(raster::extent(.self$BB))
    
    from_p <- switch(tolower(from_proj[1]),
        'longlat' = .self$proj['longlat'],
        'native' = .self$proj['lcc'],
        'lcc' = .self$proj['lcc'],
        from_proj)
    
    to_p <- switch(tolower(to_proj[1]),
        'longlat' = .self$proj['longlat'],
        'native' = .self$proj['lcc'],
        'lcc' = .self$proj['lcc'],
        to_proj)
    
    if(!(name[1] %in% names(.self$vardim))) {
        cat("variable not found:", name[1], "\n")
        return(NULL)
    }

    #bb <- bbox_to_polygon(bb, proj = .self$proj['longlat'])
    bb <- bbox_to_polygon(bb, proj = from_p)
    
    xy <- .self$bb_to_xy(bb)
    
    something <- x_y_something(.self$vardim[[name[1]]])
    x <- try(
        switch(.self$vardim[[name[1]]],
            'x_y_time' = 
                NAM218_x_y_time(.self, name[1], xy=xy, ...), 
            'x_y_time1' = 
                NAM218_x_y_time(.self, name[1], ...), 
            'x_y_isobaric_time' = 
                NAM218_x_y_something_time(.self, name[1], dimname = something, xy=xy,  ...),
            'x_y_isobaric1_time' = 
                NAM218_x_y_something_time(.self, name[1], dimname = something, xy=xy, ...),
            'x_y_isobaric2_time' = 
                NAM218_x_y_something_time(.self, name[1], dimname = something,xy=xy,  ...),
            'x_y_depth_below_surface_time' = 
                NAM218_x_y_something_time(.self, name[1], dimname = something, xy=xy, ...),
            'x_y_height_above_ground_time' = 
                NAM218_x_y_something_time(.self, name[1], dimname = something, xy=xy, ...),
            'x_y_height_above_ground1_time' = 
                NAM218_x_y_something_time(.self, name[1], dimname = something, xy=xy, ...),
            'x_y_hybrid_time' = 
                NAM218_x_y_something_time(.self, name[1], dimname = something, xy=xy, ...),
            'x_y_layer_between_two_depths_below_surface_time' = 
                NAM218_x_y_something_time(.self, name[1], dimname = something, xy=xy, ...),
            'x_y_layer_between_two_depths_below_surface1_time' = 
                NAM218_x_y_something_time(.self, name[1], dimname = something, xy=xy, ...),
            'x_y_layer_between_two_depths_below_surface2_time' = 
                NAM218_x_y_something_time(.self, name[1], dimname = something,xy=xy,  ...),
            'x_y_layer_between_two_pressure_difference_from_ground_time' = 
                NAM218_x_y_something_time(.self, name[1], dimname = something, xy=xy, ...),
            'x_y_layer_between_two_pressure_difference_from_ground1_time' = 
                NAM218_x_y_something_time(.self, name[1], dimname = something, xy=xy, ...),
            'x_y_layer_between_two_pressure_difference_from_ground2_time' = 
                NAM218_x_y_something_time(.self, name[1], dimname = something, xy=xy, ...), 
            
            'bounds_dim_layer_between_two_depths_below_surface' =  NULL,
            'bounds_dim_layer_between_two_depths_below_surface1' = NULL,
            'bounds_dim_layer_between_two_depths_below_surface2' = NULL,
            'bounds_dim_layer_between_two_pressure_difference_from_ground' = NULL,
            'bounds_dim_layer_between_two_pressure_difference_from_ground1' = NULL,
            'bounds_dim_layer_between_two_pressure_difference_from_ground2' = NULL,  
            'bounds_dim_time1' = NULL,
            'maxStrlen64' = NULL)
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
            xmn = xy$bbox[1], xmx = xy$bbox[2],
            ymn = xy$bbox[3], ymx = xy$bbox[4],
            crs = .self$proj['lcc'])
        if (to_p != .self$proj['lcc'])
            R <- raster::projectRaster(from = R, crs = to_p)
        if (tolower(flip[1]) == 'y') R <- raster::flip(R, 'y')
        names(R) <- name
    }
    return(R)
    })
    

######## METHODS ABOVE
######## FUNCTIONS BELOW

#' Create an instance of NAM218RefClass
#'
#' @export
#' @param nc either a path, url, ncdf4 object or null
#' @return NAM218RefClass object
NAM218 <- function(nc){
    NAM218RefClass$new(nc)
}

#' Retrieve a variable based upon [x,y,time] as a matrix
#'
#' @export
#' @param X NAM218RefClass object
#' @param name character variable name
#' @param xy a two element list providing start and count values
#' @param ... further arguments (unused)
#' @return numeric matrix
NAM218_x_y_time <- function(X, name, bb = X$BB, 
    xy = list(start = c(1,1), count = c(-1, -1)) ,...){
    ncdf4::ncvar_get(X$NC, dim, start = c(xy$start, 1), count = c(xy$count, -1))
}

#' Retrieve a variable based upon [x,y,something,time] as a matrix
#' @export
#' @param X NAM218RefClass object
#' @param name character variable name
#' @param dimname the dimension name (isobaric, isobaric1, "depth_below_surface", etc...)
#' @param value numeric the value to retrieve (we take the closest match)
#' @param xy a two element list providing start and count values
#' @param ... further arguments (unused)
#' @return numeric matrix
NAM218_x_y_something_time <- function(X, name, dimname = 'isobaric', 
    value = 1000, bb = X$BB, 
    xy = list(start = c(1,1), count = c(-1, -1)), ...){
    ix  <- closest_index(ncdim_get(X$NC, dimname), value[1])
    ncdf4::ncvar_get(X$NC, name,  start = c(xy$start, ix, 1), count = c(xy$count,1,-1))
}
