# nam218

A simple [R language](https://www.r-project.org/) interface to
[NAM catalogs](https://www.ncei.noaa.gov/products/weather-climate-models/north-american-mesoscale) forecasts and archives.

Products are organized by date ("day"), forecast statement time ("ftime") and forecast time ("ahead"). Parameters vary by the forecast hour. (Note - some of the docs seems to not match
served data file contents.  Check carefully for discrepancies.)

[Parameters at 000](https://www.nco.ncep.noaa.gov/pmb/products/nam/nam.t00z.awphys00.tm00.grib2.shtml)

[Parameters at 006](https://www.nco.ncep.noaa.gov/pmb/products/nam/nam.t00z.awphys06.tm00.grib2.shtml)


#### Requirements

+ [R](https://www.r-project.org/) version 4.0 or higher

And packages...


  + [ncdf4](https://CRAN.R-project.org/package=ncdf4)
  
  + [thredds](https://github.com/BigelowLab/thredds)
  
  + [httr](https://CRAN.R-project.org/package=httr)
  
  + [rlang](https://CRAN.R-project.org/package=rlang)
  
  + [magrittr](https://CRAN.R-project.org/package=magrittr)
  
  + [raster](https://CRAN.R-project.org/package=raster)
  
  + [dplyr](https://CRAN.R-project.org/package=dplyr)
  
  + [readr](https://CRAN.R-project.org/package=readr)
  
  + [sp](https://CRAN.R-project.org/package=sp)
  
  + [tibble](https://CRAN.R-project.org/package=tibble)
  
  + [xml2](https://CRAN.R-project.org/package=xml2)
    

#### Installation

Use the [remotes](https://CRAN.R-project.org/package=remotes) package to install.
```r
remotes::install_github("BigelowLab/thredds")
remotes::install_github('BigelowLab/nam218')
```


From the [NAM catalogs](https://www.ncei.noaa.gov/products/weather-climate-models/north-american-mesoscale) can query for [forecasts](https://www.ncei.noaa.gov/thredds/catalog/model-nam218/catalog.html), [analyses](https://www.ncei.noaa.gov/thredds/catalog/model-namanl/catalog.html) or [historical analyses](https://www.ncei.noaa.gov/thredds/catalog/model-namanl-old/catalog.html).  

**Warning** It seems that analyses are served back to 2006 while historical analyses are served back to 2004.  In fact, the public entry into the historical analyses are now hidden from the [user entry page](https://www.ncei.noaa.gov/products/weather-climate-models/north-american-mesoscale). I suspect that we are being encouraged to drop that use of historical analyses and just use the available analyses. On the other hand, some years are missing in the analyses public catalog that are available in the historical analyses catalog. 

**N.B.** This package is old-but-still useful, but it needs to be refactored and simplified. Ultimately, it should be used only for locating available NAM218 resources. That's the perfect use for this package.  There are a lot of other old functions within the package that were used for accessing specific datasets that were specific to a project, and, to be frank, weren't such a good idea.  These other functions will likely be deprecated in the future.  



### Query for datasets

This is a query we build to get a resource for `analysis` products for a date 4 days prior to present.  Note the forecast statement hour is noon.  When a statement is produced it really is a forecast statement going up to 84h ahead of the forecast statement time. 

```
library(nam218)
what = "analysis"
day = format(Sys.Date() - 4, format = "%Y%m%d")
ftime = '1200'
ahead = '000'
print(query_nam218(what = what, day = day, ftime = ftime, ahead = ahead))
# $nam_218_20211110_1200_000.grb2
# DatasetNode (R6): 
#   verbose: FALSE    tries: 3    namespace prefix: d1
#   url: model-namanl/202111/20211110/nam_218_20211110_1200_000.grb2
#   name: nam_218_20211110_1200_000.grb2
#   dataSize: 54.65
#   date: 2021-11-10T14:40:10.105Z
```

A list of objects, each a `DatasetNode` class object, is produced even though we requested a single day, and time. There is a lot of stuff associated with the dataset, but the above prints some useful metadata. Alternatively, you could request that a simple URL be returned.

```
print(query_nam218(what = what, day = day, ftime = ftime, ahead = ahead, form = "uri"))
# [1] "https://www.ncei.noaa.gov/thredds/dodsC/model-namanl/202111/20211110/nam_218_20211110_1200_000.grb2"
```

Whoa!  That is what 99% of use cases require - "please, just give me the path to the data I need and never mind all that coding cruft." 

Here are examples for `forecast` and `analysis-historical` where we retrieve just URLs.  In the first case we request all of the forecasts associated with the `1200` statement; we do this by setting `ahead=NA` which returns all available forecast products available for each statement time.
```
what = "forecast"
day = format(Sys.Date() - 4, format = "%Y%m%d")
ftime = '1200'
ahead = NA
print(query_nam218(what = what, day = day, ftime = ftime, ahead = ahead, form = 'uri'))
#  [1] "https://www.ncei.noaa.gov/thredds/dodsC/model-nam218/202111/20211110/nam_218_20211110_1200_084.grb2"
#  [2] "https://www.ncei.noaa.gov/thredds/dodsC/model-nam218/202111/20211110/nam_218_20211110_1200_081.grb2"
#  [3] "https://www.ncei.noaa.gov/thredds/dodsC/model-nam218/202111/20211110/nam_218_20211110_1200_078.grb2"
#  .
#  .<blah><blah><blah>
#  .
# [51] "https://www.ncei.noaa.gov/thredds/dodsC/model-nam218/202111/20211110/nam_218_20211110_1200_002.grb2"
# [52] "https://www.ncei.noaa.gov/thredds/dodsC/model-nam218/202111/20211110/nam_218_20211110_1200_001.grb2"
# [53] "https://www.ncei.noaa.gov/thredds/dodsC/model-nam218/202111/20211110/nam_218_20211110_1200_000.grb2"
```

In this next case, we will request all of the statements on a given date for a given `ahead` hour.

```
what = 'analysis-historical'
day = '20180704'
ftime = NA
ahead = '000'
print(query_nam218(what = what, day = day, ftime = ftime, ahead = ahead, form = 'uri'))
# [1] "https://www.ncei.noaa.gov/thredds/dodsC/model-namanl-old/201807/20180704/namanl_218_20180704_1800_000.grb2"
# [2] "https://www.ncei.noaa.gov/thredds/dodsC/model-namanl-old/201807/20180704/namanl_218_20180704_1200_000.grb2"
# [3] "https://www.ncei.noaa.gov/thredds/dodsC/model-namanl-old/201807/20180704/namanl_218_20180704_0600_000.grb2"
# [4] "https://www.ncei.noaa.gov/thredds/dodsC/model-namanl-old/201807/20180704/namanl_218_20180704_0000_000.grb2"
```

Please note that the listings are not in a natural order, but you could sort to your own liking.

### Accessing a dataset

Once you have successfully queried for a dataset, then you can access it's URL and open the resource 
with the [ncdf4](https://CRAN.R-project.org/package=ncdf4) package. Remember that a list (or a character vector) is returned (since you could on a given `day` query for multiple values `ftime` and `ahead`), so be sure to index into the list (or character vector) even if you only ask for one dataset.

```
what = 'analysis-historical'
day = '20180704'
ftime = '1200'
ahead = '000'
qry_uri <- query_nam218(what = what, day = day, ftime = ftime, ahead = ahead, form = 'uri')
X <- ncdf4::nc_open(query_uri[1])
X
#File https://www.ncei.noaa.gov/thredds/dodsC/model-namanl-old/201807/20180704/namanl_218_20180704_1200_000.grb2 (NC_FORMAT_CLASSIC):
#
#     110 variables (excluding dimension variables):
#        int LambertConformal_Projection[]   
#            grid_mapping_name: lambert_conformal_conic
#            latitude_of_projection_origin: 25
#            longitude_of_central_meridian: 265
#            standard_parallel: 25
#            earth_radius: 6371229
#        double reftime[]   
#            units: Hour since 2018-07-04T12:00:00Z
#            standard_name: forecast_reference_time
#            long_name: GRIB reference time
#            calendar: proleptic_gregorian
#        float height_above_ground_layer_bounds[height_above_ground_layer_bounds_1,height_above_ground_layer]   
#            units: m
#            long_name: bounds for height_above_ground_layer
#    .
#    . <blah><blah><blah>
#    .
#    9 global attributes:
#        Originating_or_generating_Center: US National Weather Service, National Centres for Environmental Prediction (NCEP)
#        Originating_or_generating_Subcenter: 0
#        GRIB_table_version: 2,1
#        Type_of_generating_process: Forecast
#        Analysis_or_forecast_generating_process_identifier_defined_by_originating_centre: MESO NAM Model (currently 12 km)
#        file_format: GRIB-2
#        Conventions: CF-1.6
#        history: Read using CDM IOSP GribCollection v3
#        featureType: GRID
```

The [ncdf4](https://CRAN.R-project.org/package=ncdf4) package is not the onlt way to open and access the data. But we find it very useful and fast.  Whatever approach you use, be sure to close the resource when you are done.

```
ncdf4::nc_close(X)
```
