#' Computes the index that places the value in the vector
#'
#' This is not like \code{findInterval} which places by order.  Here order
#' is irrelevant.
#'
#' @export
#' @param vec vector of numeric values to search
#' @param val the val to match into vec
#' @return 1-based index of the closest match in vec
closest_index <- function(vec, val){
    which.min(abs(vec - val))[1]
}

#' Given a string x_y_something_time extract the 'something'
#'
#' @export
#' @param x the string to parse
#' @return the 'something' string if x doesn't start with x_y_ or starts with x_y_time then "" is returned
x_y_something <- function(
    x = c("x_y_layer_between_two_depths_below_surface_time", "x_y_time2", "bounds_foo_bar", "x_y_lev_time", "x_y_time")){
    ss <- strsplit(x, "_", fixed = TRUE)
    s <- sapply(ss, function(x) paste(x[3:(length(x)-1)], collapse = "_"))
    s[!grepl("x_y_", x, fixed = TRUE)] <- ""
    s[grepl("x_y_time", x, fixed = TRUE)] <- ""
    s
}


#' Convert a bbox to sp::polygon, sp::Polygons, or sp::SpatialPolygons object
#'
#' @export
#' @param bb numeric, a 4-element bbox vector [left, right, bottom, top]
#' @param proj character sutiable to pass to \code{sp::CRS}
#' @param id character, the polygon ID, by default 'bbox'
#' @param output_class character, either "SpatialPolygons", "Polygons" or "Polygon"
#' @return sp::SpatialPolygons object or NULL
bbox_to_polygon <- function(bb = c(-153.681, -48.59701 , 11.37212, 62.08712),
   proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
   id = 'bbox',
   output_class = c("SpatialPolygons", "Polygons",  "Polygon")[1]){


   # clockwise to form an 'island'
   bb <- cbind(
      x = c(bb[1], bb[1], bb[2], bb[2], bb[1]),
      y = c(bb[3], bb[4], bb[4], bb[3], bb[3]) )
   # make a Polygon
   Poly <- sp::Polygon(bb)
   if (output_class == 'Polygon') return(Poly)
   # make into Polygons
   Polys <- sp::Polygons(list(Poly), id)
   if (output_class == 'Polygons') return(Polys)
   sp::SpatialPolygons(list(Polys), proj4string = sp::CRS(proj))
}


#' A wrapper around base::findInterval() that allows decreasing values in the
#' value of the vector within which we wish to place values of x.
#'
#' When \code{vec} is in ascending order we use \code{base::findInterval()}, but
#' when \code{vec} is in descending order we implement an adaptation of the
#' \code{locate()} function from Numerical Recipes for C \url{http://apps.nrbook.com/c/index.html}
#'
#' @export
#' @param x numeric values we wish to located within \code{vec}
#' @param vec numeric vector of sorted values (ascending or descending order)
#'    within which we wish to find the placement of \code{x}
#' @param rightmost.closed see \link{findInterval}
#' @param all.inside see \link{findInterval}
#' @return see \link{findInterval}
find_interval <- function(x, vec, rightmost.closed = FALSE, all.inside = FALSE){

    # locate one value of x within v
    # param v ordered numeric vector
    # param x one numeric lo locate within v
    # return index into v
    locate_one <- function(v, x){
    n <- length(v)
    ascnd <- v[n] >= v[1]
    iL <- 1
    iU <- n
    while((iU-iL) > 1){
        iM <- bitwShiftR((iU+iL),1)
        if (ascnd){
           if (x >= v[iM]){
              iL <- iM
           } else {
              iU <- iM
           }
        } else {
           if (x <= v[iM]){
              iL <- iM
           } else {
              iU <- iM
           }
        }
    }

    if (ascnd) {
        if ( x < v[1]) {
            index <- 0
        } else if (x >= v[n]) {
            index <- n
        } else {
            index <- iL
        }
    } else {
        if ( x > v[1]) {
            index <- 0
        } else if (x <= vec[n]) {
            index <- n
        } else {
            index <- iL
        }
    }
    return(index)
    }  # locate_one

    ascending <- vec[length(vec)] >= vec[1]

    if (!ascending) {
        # here we do our own implementation (with a performance hit)
        j <- sapply(x, function(x, v=NULL) locate_one(v,x), v = vec)
        nv <- length(vec)
        if (all.inside){
            j[j < 1] <- 1
            j[j >= nv] <- nv - 1
        }
        if (rightmost.closed){
            j[x <= vec[nv]] <- nv - 1
        }
    } else {
       # this is plain vanilla stuff we pass to findInterval
        j <- base::findInterval(x, vec,
            rightmost.closed = rightmost.closed, all.inside = all.inside)
    }
    j
} # find_interval
