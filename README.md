# nam218

A simple [R language](https://www.r-project.org/) interface to [NOMADS NAM](https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/north-american-mesoscale-forecast-system-nam) forecasts and archives.

Parameters vary by the forecast hour. 

[000](http://www.nco.ncep.noaa.gov/pmb/products/nam/nam.t00z.awphys00.grb2.tm00.shtml)

[006](http://www.nco.ncep.noaa.gov/pmb/products/nam/nam.t00z.awphys06.grb2.tm00.shtml)


#### Catalog-dataset mishaps

There are some entires days missing from the catalog. While slightly annoying this generally is not an issue as the catalogs will properly match the missingness of the datasets ( ala catalog == dataset). Unfortunately, sometimes the catalog will indicate a dataset is present when in fact it is not available ( ala catalog != dataset). Some attempt has been made to resolve the latter issue - trying to open an unavailable OPeNDAP dataset that is listed in the THREDDS catalog will cause the main object class to return as NULL.  At the time of this writing you can see an example [here at 0000_051.grib](https://nomads.ncdc.noaa.gov/thredds/catalog/nam218/201704/20170401/catalog.html)


#### Requirements

+ [R](https://www.r-project.org/) version 3.0 or higher

And packages...

+ [raster](https://cran.r-project.org/web/packages/raster/index.html) 

+ [ncdf4](https://cran.r-project.org/web/packages/ncdf4/index.html)

+ [threddscrawler](https://github.com/BigelowLab/threddscrawler)

+ [httr](https://cran.r-project.org/web/packages/httr/index.html)

+ [tibble](https://cran.r-project.org/web/packages/tibble/index.html)

+ [magrittr](https://cran.r-project.org/web/packages/magrittr/index.html)

#### Installation

It's fairly easy to install using Hadley Wickham's [devtools](http://cran.r-project.org/web/packages/devtools/index.html).

```r
library(devtools)
install_github("BigelowLab/threddscrawler")
install_github('BigelowLab/nam218')
```

#### Examples

Finding data with a query...

```R
library(nam218)
dataset <- query_nam218(what = 'analysis', date = '20080704', ftime = '1200')
dataset[[1]]
# Reference Class: "DatasetsRefClass"
#   verbose_mode: FALSE
#   url: https://nomads.ncdc.noaa.gov/thredds/catalog/namanl/200807/20080704/namanl_218_20080704_1200_000.grb
#   children: dataSize date
#   datasets: NA
```

Once you have identified the resource then you can instantiate a NAM reference class to get data.

```R
# extract an OPeNDAP url
uri <- nam218_url(dataset[[1]], what = 'OPeNDAP')
uri
# [1] "https://nomads.ncdc.noaa.gov/thredds/dodsC/namanl/200807/20080704/namanl_218_20080704_1200_000.grb"

# create the NetCDF handler 
X <- NAM218(uri)

# retrieve a layer and show it
RH <- X$get_layer("Relative_humidity")
RH
  # class       : RasterLayer 
  # dimensions  : 428, 614, 262792  (nrow, ncol, ncell)
  # resolution  : 12.17114, 12.16252  (x, y)
  # extent      : -4226.107, 3246.976, -832.6983, 4372.859  (xmin, xmax, ymin, ymax)
  # coord. ref. : +proj=lcc +lat_1=25 +lat_0=25 +lon_0=-95 +k_0=1 +x_0=0 +y_0=0 +a=6367470.21484375 +b=6367470.21484375 +units=km +no_defs 
  # data source : in memory
  # names       : Relative_humidity 
  # values      : 4, 100  (min, max)

library(raster)
sp::spplot(RH)
```

As a convenience a spatial subset can be requested with a bounding box specified as [left, right, bottom, top].  The inout and output subset coordinate system can be specified in NAM218 native coordinates `lambert conformal conic` or in `longlat` using the `from_proj` and `to_proj` arguments.

```R
BBOX <- c(-72,-63,39,46)

R_lcc <- X$get_layer("Relative_humidity", bb = BBOX, from_proj = 'longlat', to_proj = 'native')
R_lcc
  # class       : RasterLayer 
  # dimensions  : 80, 77, 6160  (nrow, ncol, ncell)
  # resolution  : 11.99663, 11.9678  (x, y)
  # extent      : 1901.858, 2825.599, 1745.543, 2702.967  (xmin, xmax, ymin, ymax)
  # coord. ref. : +proj=lcc +lat_1=25 +lat_0=25 +lon_0=-95 +k_0=1 +x_0=0 +y_0=0 +a=6367470.21484375 +b=6367470.21484375 +units=km +no_defs 
  # data source : in memory
  # names       : Relative_humidity 
  # values      : 56, 100  (min, max)

R_ll <- X$get_layer("Relative_humidity", bb = BBOX, from_proj = 'longlat', to_proj = 'longlat')
R_ll
  # class       : RasterLayer 
  # dimensions  : 110, 104, 11440  (nrow, ncol, ncell)
  # resolution  : 0.136, 0.1  (x, y)
  # extent      : -74.21129, -60.06729, 36.93508, 47.93508  (xmin, xmax, ymin, ymax)
  # coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
  # data source : in memory
  # names       : Relative_humidity 
  # values      : 56.18131, 100  (min, max)
```