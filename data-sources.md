# NAM218 data-sources: forecasts, archived-forecast and analyses

[NCEI](https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/north-american-mesoscale-forecast-system-nam)
provides multiple data access methods: thredds, FTP, HTTPS and HAS.  We use
the thredds catalog to access OpeNDAP resources using the [ncdf4](https://cran.r-project.org/package=ncdf4) R package.

**NOTE** as of this writing top level urls and variable names have changed twice
since we started using NAM in 2016. It's not really possible (or meaningful) to
maintain backwards compatibility since the server-side offerings change all served data.
We skirt this issue by storing local subsets of the resources for select variable,
and by separate packages for accessing online data and for accessing locally stored data.

## Analyses aka `namanl`

An [example](https://www.ncei.noaa.gov/thredds/catalog/namanl/201802/20180219/catalog.html?dataset=namanl/201802/20180219/namanl_218_20180219_0600_000.grb2)

Note that these grids are projected Lamber Conformal Conic `lcc`.  Data covers 2004-03 through present - typically lagging 'current' by **2 days**.
Forecasts are available for forecast statement times `ftime = 0000, 0600, 1200, 1800` with look-ahead times `ahead = 000, 001, 002, 002, 006`

```
Y = NAM218('https://www.ncei.noaa.gov/thredds/dodsC/namanl/201802/20180219/namanl_218_20180219_0600_000.grb2')
Y
Reference Class: "NAM218RefClass"
  state: open
 file:  https://www.ncei.noaa.gov/thredds/dodsC/namanl/201802/20180219/namanl_218_20180219_0600_000.grb2
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





## Forecasts aka `nam218` come in two forms: *archived* and *current*

### Archived Forecasts

An [example](https://www.ncei.noaa.gov/thredds/catalog/nam218/201802/20180219/catalog.html?dataset=nam218/201802/20180219/nam_218_20180219_0000_000.grb2)

Note that these grids are projected Lamber Conformal Conic `lcc`.  Data covers
2016-06 through present - typically lagging 'current' by **2 days**.
Forecasts are available for forecast statement times `ftime = 0000, 0600, 1200, 1800`
with look-ahead times `ahead = (hourly) 000-036, (3 hourly) 039-084`

Contents accessed using the `NAM218` reference class and are arranged as shown
above for *Analyses*

### Current Forecasts

An [example](http://nomads.ncep.noaa.gov/dods/nam) and more specifically we
typically use the [NAM every 3 hours fcst staring from 00Z](http://nomads.ncep.noaa.gov/dods/nam/nam20180221/nam_00z.info)

Grid node locations are given by lat,lon coordinates. We use the `NAMcast`
reference object to handle this data source. Note that the variable names and
content organization are different than for **Analyses** ans **Archived Forecasts**


```
Z = NAMcast('http://nomads.ncep.noaa.gov/dods/nam/nam20180221/nam_00z')
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




