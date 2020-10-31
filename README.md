# oceancolouR

This is a collection of frequently used functions for remote sensing data analysis.


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

## Wishlist

* Commonly used matchup extraction methods (with Q/C options from standard papers?)
