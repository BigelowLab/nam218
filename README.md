# namanl
A simplified [R language](https://www.r-project.org/) interface to [NOMADS NAM-ANL](https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/north-american-mesoscale-forecast-system-nam) forecast data and archives.


#### Requirements

+ [R](https://www.r-project.org/) version 3.0 or higher

+ [raster](https://cran.r-project.org/web/packages/raster/index.html) R package

+ [ncdf4](https://cran.r-project.org/web/packages/ncdf4/index.html) R package

+ [threddscrawler](https://github.com/BigelowLab/threddscrawler) R package

#### Installation

It's fairly easy to install using Hadley Wickham's [devtools](http://cran.r-project.org/web/packages/devtools/index.html).

```r
library(devtools)
install_github('BigelowLab/namanl')
```

#### Examples

Finding data with a query...

```R
library(namanl)
dataset <- namanl_query(day = '20080704', time = '1200')
dataset
# Reference Class: "DatasetsRefClass"
#   verbose_mode: FALSE
#   url: http://nomads.ncdc.noaa.gov/thredds/catalog/namanl/200807/20080704/namanl_218_20080704_1200_000.grb
#   children: dataSize date
#   datasets: NA
```

Once you have identified the resource then you can instantiate a NAMANL reference class to get data.

```R
# extract an OPeNDAP url
url <- namanl_url(dataset, what = 'OPeNDAP')
url
# [1] "http://nomads.ncdc.noaa.gov/thredds/dodsC/namanl/200807/20080704/namanl_218_20080704_1200_000.grb"

# create the NetCDF handler 
X <- NAMANL(url)

# retrieve a layer and show it
RH <- X$get_layer("Relative_humidity")

library(raster)
raster::spplot(RH)
```

Not every variable has a simplified access method, current ones include: Temperature at different isobaric levels, Surface_wind_gust, Total_precipitation, and Relative_humidity at different isobaric levels.  

