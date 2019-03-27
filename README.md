# nam218

A simple [R language](https://www.r-project.org/) interface to
[NCEP NAM](https://www.nco.ncep.noaa.gov/pmb/products/nam/) forecasts and archives.

Parameters vary by the forecast hour. (Note - some of the docs seems to not match
served data file contents.  Check carefully for discrepancies.)

[000](https://www.nco.ncep.noaa.gov/pmb/products/nam/nam.t00z.awphys00.tm00.grib2.shtml)
[006](https://www.nco.ncep.noaa.gov/pmb/products/nam/nam.t00z.awphys06.tm00.grib2.shtml)

#### Catalog-dataset mishaps

There are some entires days missing from the catalog. While slightly annoying this
generally is not an issue as the catalogs will properly match the missingness of
the datasets ( ala catalog == dataset). Unfortunately, sometimes the catalog will
indicate a dataset is present when in fact it is not available ( ala catalog != dataset).
Some attempt has been made to resolve the latter issue - trying to open an unavailable
OPeNDAP dataset that is listed in the THREDDS catalog will cause the main object
class to return as NULL.

#### Requirements

+ [R](https://www.r-project.org/) version 3.0 or higher

And packages...

+ [raster](https://cran.r-project.org/web/packages/raster/index.html)

+ [ncdf4](https://cran.r-project.org/web/packages/ncdf4/index.html)

+ [thredds](https://github.com/BigelowLab/thredds)

+ [httr](https://cran.r-project.org/web/packages/httr/index.html)

+ [tibble](https://cran.r-project.org/web/packages/tibble/index.html)

+ [magrittr](https://cran.r-project.org/web/packages/magrittr/index.html)

#### Installation

It's fairly easy to install using Hadley Wickham's [devtools](http://cran.r-project.org/web/packages/devtools/index.html).

```r
library(devtools)
install_github("BigelowLab/thredds")
install_github('BigelowLab/nam218')
```

### Realtime Forecasts aka 'namcast'

An [example](http://nomads.ncep.noaa.gov/dods/nam) and more specifically we
typically use the NAM every 3 hours fcst staring from 00Z - select a day from the
page lineked to above and then select `nam_00z.info`.
Be aware that the date identified in the link should be updated as needed.

Grid node locations are given by lat,lon coordinates. We use the `NAMcast`
reference object to handle this data source. Note that the variable names and
content organization are different than for **Analyses** ans **Archived Forecasts**


```
uri = format(Sys.Date(),"http://nomads.ncep.noaa.gov/dods/nam/nam%Y%m%d/nam_00z" )
Z = NAMcast(uri)
Z
Reference Class: "NAMcastRefClass"
  state: open
file:  http://nomads.ncep.noaa.gov/dods/nam/nam20180221/nam_00z

 VARS:
    absvprs [lon,lat,lev,time]
    no4lftx180_0mb [lon,lat,time]
    acpcpsfc [lon,lat,time]
    ...
    weasdsfc [lon,lat,time]
    wiltsfc [lon,lat,time]

  DIMS:
    lat[443]
    lev[42]
    lon[913]
    time[29]
```


### Archived Forecasts

An [example](https://www.ncei.noaa.gov/thredds/catalog/nam218/201802/20180219/catalog.html)

Note that these grids are projected Lamber Conformal Conic `lcc`.  Data covers
2016-06 through present - typically lagging 'current' by **2 days**.
Forecasts are available for forecast statement times `ftime = 0000, 0600, 1200, 1800`
with look-ahead times `ahead = (hourly) 000-036, (3 hourly) 039-084`

Contents accessed using the `NAM218` reference class and are arranged as shown
above for *Analyses*

### Analyses

Not all variables extend through the entire 2004-present period (surface vis and cloud cover examples)
Somewhere along the line the format changed from GRB (ugg) to GRB2 (readable by ncdf4)
We need a switch to  navigate the GRB/GRB2 change
    if GRB use rNOMADS
    if GRB2 use ncdf4


An [example](https://www.ncei.noaa.gov/thredds/dodsC/nam218/201902/20190219/nam_218_20190219_0000_000.grb2.html)

Note that these grids are projected Lamber Conformal Conic `lcc`.  Data covers 2004-03 through present - typically lagging 'current' by **2 days**.
Forecasts are available for forecast statement times `ftime = 0000, 0600, 1200, 1800` with look-ahead times `ahead = 000, 001, 002, 002, 006`

```
Y = NAM218("http://www.ncei.noaa.gov/thredds/dodsC/nam218/201902/20190219/nam_218_20190219_0000_000.grb2")
Y
Reference Class: "NAM218RefClass"
state: open
file:  http://www.ncei.noaa.gov/thredds/dodsC/nam218/201902/20190219/nam_218_20190219_0000_000.grb2

 VARS:
    LambertConformal_Projection
    reftime
    height_above_ground_layer_bounds [height_above_ground_layer_bounds_1,height_above_ground_layer]
    ...
    Dewpoint_temperature_height_above_ground [x,y,height_above_ground,time]
    Geopotential_height_isobaric [x,y,isobaric1,time]
    ...
    v-component_of_wind_tropopause [x,y,time]
    v-component_of_wind_planetary_boundary [x,y,time]
    v-component_of_wind_pressure_difference_layer [x,y,pressure_difference_layer2,time]
  DIMS:
    depth_below_surface[1]
    depth_below_surface_layer[4]
    ...
    sigma_layer[1]
    sigma_layer_bounds_1[2]
    time[1]
    x[614]
    y[428]
```

### Examples

Finding data with a query...

```R
library(nam218)
dataset <- query_nam218(what = 'analysis', day = '20180704', ftime = '1200', ahead = '000')
dataset[[1]]
# Reference Class: "DatasetRefClass"
#   verbose_mode: FALSE
#   tries: 3
#   url: https://www.ncei.noaa.gov/thredds/catalog/namanl/201807/20180704/namanl_218_20180704_1200_000.grb2
#   children: dataSize date
#   datasets: NA
#   dataSize: 50.86
#   date: 2018-07-09T22:43:47Z
#   serviceName:
#   urlPath:
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
RH <- X$get_layer("Relative_humidity_height_above_ground")
RH
# class       : RasterLayer
# dimensions  : 427, 614, 262178  (nrow, ncol, ncell)
# resolution  : 12.191, 12.21955  (x, y)
# extent      : -4232.202, 3253.072, -838.7938, 4378.954  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=lcc +lat_1=25 +lat_0=25 +lon_0=-95 +k_0=1 +x_0=0 +y_0=0 +a=6367470.21484375 +b=6367470.21484375 +units=km +no_defs
# data source : in memory
# names       : Relative_humidity_height_above_ground
# values      : 7.132321, 100.0323  (min, max)


library(raster)
sp::spplot(RH)

```

As a convenience a spatial subset can be requested with a bounding box specified as [left, right, bottom, top].
The inout and output subset coordinate system can be specified in NAM218 native coordinates
`lambert conformal conic` or in `longlat` using the `from_proj` and `to_proj` arguments.

```R
BBOX <- c(-72,-63,39,46)

R_lcc <- X$get_layer("Relative_humidity_height_above_ground", bb = BBOX, from_proj = 'longlat', to_proj = 'native')
R_lcc
  # class       : RasterLayer
  # dimensions  : 80, 77, 6160  (nrow, ncol, ncell)
  # resolution  : 11.99663, 11.9678  (x, y)
  # extent      : 1901.858, 2825.599, 1745.543, 2702.967  (xmin, xmax, ymin, ymax)
  # coord. ref. : +proj=lcc +lat_1=25 +lat_0=25 +lon_0=-95 +k_0=1 +x_0=0 +y_0=0 +a=6367470.21484375 +b=6367470.21484375 +units=km +no_defs
  # data source : in memory
  # names       : Relative_humidity
  # values      : 56, 100  (min, max)

R_ll <- X$get_layer("Relative_humidity_height_above_ground", bb = BBOX, from_proj = 'longlat', to_proj = 'longlat')
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
