#ftp://nomads.ncdc.noaa.gov/NAM/analysis_only/200403/20040303/namanl_218_20040303_0000_000.grb
# https://nomads.ncdc.noaa.gov/data/namanl/200403/20040303/namanl_218_20040303_0000_000.grb


#' Determine by date whether to use grib1 or grib2
#'
#' @export
#' @param date one or more dates as character ("YYYY-mm-dd"), Date or POSIXt
#' @param origin the date at which NAM218 switches from grib1 to grib2
#' @return character vector of length date with elements either 'grib1' or 'grib2'
which_grib <- function(date = c("2017-04-05", "2017-04-06"), origin = as.Date("2017-04-06")){

    if (!inherits(date, 'Date')) date = as.Date(date)

    x = rep("grib1", length(date))
    x[date >= origin] <- 'grib2'
    x
}


#' Parse a NAMANL grib filename
#'
#' @export
#' @param name the name in 'namanl_218_YYYYmmdd_ffff_aaa' pattern
#' @return named character vector
#' \itemize{
#'   \item{name the original name stripped of path and extension}
#'   \item{product namanl_218 (fist two parts joined with "_")}
#'   \item{ymd YYYYmmdd}
#'   \item{ym  YYYYmm}
#'   \item{y YYYY}
#'   \item{m mm}
#'   \item{d dd}
#'   \item{ftime  forecast statement time as 24h}
#'   \item{atime  forecast hours ahead relative to statement time}
#' }
namanl_grib1_name_parse <- function( name = 'namanl_218_20040303_0000_000'){

    name    = basename(name)
    ext     = raster::extension(name)
    if (nchar(ext) > 1) name = gsub(ext, "", name , fixed = TRUE)

    ss      = strsplit(name, '_', fixed = TRUE)[[1]]
    ymd     = ss[3]
    ym      = substring(ymd, 1,6)
    y       = substring(ymd, 1,4)
    m       = substring(ymd, 5,6)
    d       = substring(ymd, 7,8)
    c(
        name = name,
        product = paste(ss[1:2], collapse = "_"),
        ymd     = ymd,
        ym      = ym,
        y       = y,
        m       = m,
        d       = d,
        ftime   = ss[4],
        atime   = ss[5])
}


#' Construct a https uri given the name of a GRIB1 file
#'
#' @export
#' @param name character, the name stub in the form of namanl_218_YYYYmmdd_ffff_aaa
#'    where ffff is the forecast statement time and aaa is the forecast hours ahead of the statement
#' @param root the root uri
#' @return two element vector of *.grb and *.inv uri
namanl_grib1_https_uri <- function(
    name = 'namanl_218_20040303_0000_000',
    root = 'https://nomads.ncdc.noaa.gov/data/namanl'){

    p       = namanl_grib1_name_parse(name)
    x       = paste0(file.path(root, p[['ym']], p[['ymd']], p[['name']]), c(".grb", ".inv"))
    names(x) = c("grb", "inv")
    x
}

#' Fetch a grb and companion inv file via https
#'
#' @export
#' @param name the name of the file to retrieve in namanl_218_YYYYmmdd_ffff_aaa pattern
#' @param path the destination path to save the file
#' @param what character vector of  grb, inv or both
#' @param ... further arguments for \code{namanl_grib1_ftp_uri()}
#' @return character vector of the filenames downloaded.
#'  \itemize{
#'      \item{zero elements means nothing downloaded}
#'      \item{one element means one file downloaded}
#'      \item{two elements mean both grb and inv files downloaded}
#'  }
namanl_grib1_https <- function(name = 'namanl_218_20040303_0000_000',
    path = tempdir(),
    what = c('grb', 'inv')[1],
    ...){

    uri     = namanl_grib1_https_uri(name, ...)
    uri     = uri[what]
    ofiles  = file.path(path, basename(uri))
    names(ofiles) = names(uri)
    ok <- sapply(names(uri),
        function(n) {
            ok = try(download.file(uri[[n]], ofiles[[n]]))
            if (inherits(ok, 'try-error')) ok = 1
            ok
        })

    ofiles[ok == 0]
}

#' Read the grib1 inventory from a grib1 file
#'
#' @export
#' @param filename the name of the grib1 file
#'  @return a tibble of 0 or more rows
read_grib1_inv <- function(filename = "/mnt/ecocast/projects/devel/grib/namanl_218_20040303_0000_000.grb"){

    if (!file.exists(filename[1])) stop("file not found:", filename)
    nm <- c(
        "band"     = 'integer',
        "offset"    = 'integer',
        "date"      = 'character',
        "var"       = "character",
        "z"         = 'character',
        "type"      = 'character',
        "descr"     = 'character' )

    cmd     <- sprintf("wgrib -s -4yr %s", filename)
    x       <- system(cmd, intern = TRUE)
    x       <- do.call(rbind, strsplit(x, ":", fixed = TRUE))
    x       <- dplyr::as_tibble(x)
    colnames(x)  <- names(nm)
    x <- x %>%
        dplyr::mutate(band = as.integer(.data$band), offset = as.integer(.data$offset))
    x
}


#' Given a GRIB1 inventory table, select the band number
#'
#' @export
#' @param x data.frame of a GRIB1 inventory
#' @param name the variavle name
#' @param level the z level desired
#' @return zero or more band numbers that match the name and level
select_band <- function(x, name = "TMP", level = "sfc"){
    (x %>% dplyr::filter((.data$var %in% name) & (.data$z %in% level)))$band
}




#' Grib1RefClass
#'
#' @field filename the name of the GRIB1 file
#' @field inv a tibble of the GRIB1 inventory
Grib1RefClass <- setRefClass("Grib1RefClass",
    fields = list(
        filename    = "character",
        inv         = "ANY"),
    methods = list(
        initialize = function(file){
            .self$field("filename", file[1])
            if (file.exists(.self$filename))
                .self$field("inv", read_grib1_inv(.self$filename))
            },
        read_grib1 = function(
                band = select_band(.self$inv),
                bb = NULL){
            "Read a raster band"
            R = raster::raster(.self$filename, band = band)
            if (!is.null(bb)) R <- raster::crop(R, bb)
            R
            },
        read_band = function(..., bb = NULL){
            "Read a raster band"
            read_grib1(band = select_band(.self$inv,...), bb = bb)
        },
        read_brick = function(){
            R <- raster::brick(.self$filename)
            names(R) <- paste(.self$inv$var, .self$inv$z, sep = "_")
            R
        }
    )
)

#' Create a Grib1RefClass object
#'
#' There are just two methods of interest for retrieving a single band or a brick
#'
#' \code{raster = ref$read_band(var = "", level = "", bb = [NULL | c(left, right, bottom, top)])}
#' \code{brick = ref$read_brick()}
#'
#' The latter you an just manage on your own by using \code{brick = raster::brick(filename)}, but
#' {ref$read_brick()} does assign better names for each layer.
#'
#' @export
#' @param filename the fully qualified filename
#' @return a Grib1RefClass object
Grib1 <- function(filename = "/mnt/ecocast/projects/devel/grib/namanl_218_20040303_0000_000.grb"){
    Grib1RefClass$new(filename)
}

