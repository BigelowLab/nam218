#' Get the total precipitation for two days.
#'
#' @export
#' @return Raster*
example_get_total_precipitation <- function(
    param = 'Total_precipitation',
    date = as.POSIXct('2015-01-01', "%Y-%m-%d", tz = 'UTC'),
    fun = 'sum',
    bb = c(-74,  -57, 40, 51.5),
    ...){
    
    
    if (FALSE){
        param = 'Total_precipitation'
        date = as.POSIXct('2015-01-01', "%Y-%m-%d", tz = 'UTC')
        fun = 'sum'
        bb = c(-74,  -57, 40, 51.5)
    }
    DD <- nam218_query(date = date,
        ftime = c("0000", "0600", "1200", "1800"),
        ahead = "006")
    XX <- lapply(DD, function(D) NAM218(nam218_url(D)))
    
    RR <- lapply(XX, function(X) 
        X$get_layer(name = param, bb = bb, from_proj = 'longlat') )
    RR <- raster::stack(RR)
    
    if (!is.null(fun) && !is.na(fun)){
        RR <- switch(tolower(fun[1]),
            'sum' = raster:::calc(RR, sum, ...),
            'mean' = raster::calc(RR, mean, ...),
            'min' = raster::calc(RR, min, ...),
            'max' = raster::calc(RR, max, ...))
    }
    RR    
}