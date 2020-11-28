# oceancolouR

This is a collection of frequently used functions for Ocean Colour Remote Sensing data processing and analysis using R.
This package is in continuing development, but the following list is some (not all) of the functions included.

**Chlorophyll-a algorithms:**

* `gsm()`: R implementation of the GSM algorithm, with traditional coefficients or coefficients calculated with `get_gs()`
* `oci()`, `ocx()` : OCI and OCX algorithms for MODIS-Aqua, SNPP-VIIRS and SeaWiFS. OCX coefficients can be optimized with `optimize_ocx_coefs()`
* `qaa()`: QAA algorithm (v6) to calculate chlorophyll-a as well as phytoplankton absorption coefficients

**Other:** 

* `read_pixEx()`: load output file of SNAP pixEx pixel extraction tool as data.frame
* `read_h5_L3b()`: read the contents of a NASA Level-3 binned file in h5 format
* `haversine()`: calculate haversine distance between two points
* `geoMean()`: calculate geometric mean
* `week8()`, `week8_date`: convert to/from standard 8-day week number and date
* `days_vector()`: list julian days for a given year/month


## How to install

``` r
remotes::install_github("BIO-RSG/oceancolouR")
#devtools::install_github("BIO-RSG/oceancolouR")
```

## Datasets

There are numeric vectors included in the package, which give the bin numbers and their corresponding latitudes, longitudes, and bathymetry values for NASA binned images, for the Pan-Canadian grid, the Northwest Atlantic region, and the Northeast Pacific region (approximate boundaries defined below), at both 4km and 9km resolution.  

    * Pan-Canadian grid:  -147 to -41 W, 39 to 86 N  
    * Northwest Atlantic: -95 to -42 W, 39 to 82 N  
    * Northeast Pacific:  -140 to -122 W, 46 to 60 N  

Datasets can be loaded using the following command:  
__data("*region_variable_resolution*")__  
where:  
    *region* is either *pancan*, *nwa*, or *nep*  
    *variable* is either *bins*, *lats*, *lons*, or *bath*  
    *resolution* is either *4km* or *9km*  
