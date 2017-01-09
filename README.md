# nam218
A simple [R language](https://www.r-project.org/) interface to [NOMADS NAM](https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/north-american-mesoscale-forecast-system-nam) forecasts and archives.


#### Requirements

+ [R](https://www.r-project.org/) version 3.0 or higher

+ [raster](https://cran.r-project.org/web/packages/raster/index.html) R package

+ [ncdf4](https://cran.r-project.org/web/packages/ncdf4/index.html) R package

+ [threddscrawler](https://github.com/BigelowLab/threddscrawler) R package

#### Installation

It's fairly easy to install using Hadley Wickham's [devtools](http://cran.r-project.org/web/packages/devtools/index.html).

```r
library(devtools)
install_github('BigelowLab/nam218')
```

#### Examples

Finding data with a query...

```R
library(nam218)
dataset <- nam_query(what = 'analysis', day = '20080704', time = '1200')
dataset
# Reference Class: "DatasetsRefClass"
#   verbose_mode: FALSE
#   url: https://nomads.ncdc.noaa.gov/thredds/catalog/namanl/200807/20080704/namanl_218_20080704_1200_000.grb
#   children: dataSize date
#   datasets: NA
```

Once you have identified the resource then you can instantiate a NAM reference class to get data.

```R
# extract an OPeNDAP url
uri <- nam_url(dataset, what = 'OPeNDAP')
uri
# [1] "https://nomads.ncdc.noaa.gov/thredds/dodsC/namanl/200807/20080704/namanl_218_20080704_1200_000.grb"

# create the NetCDF handler 
X <- NAM(uri)

# retrieve a layer and show it
RH <- X$get_layer("Relative_humidity")

library(raster)
raster::spplot(RH)
```
