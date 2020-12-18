# oceancolouR

This is a collection of frequently used functions for Ocean Colour Remote Sensing data processing and analysis using R.
This package is in continuing development, but the following list is some (not all) of the functions included.

**Chlorophyll-a algorithms:**

* `gsm()`: R implementation of the GSM algorithm, with traditional coefficients or coefficients calculated with `get_gs()`
* `oci()`, `ocx()` : OCI and OCX algorithms for MODIS-Aqua, SNPP-VIIRS and SeaWiFS. OCX coefficients can be optimized with `optimize_ocx_coefs()`
* `qaa()`: QAA algorithm (v6) to calculate chlorophyll-a as well as phytoplankton absorption coefficients
* `eof_chl()`: EOF (empirical orthogonal function) method to calculate chlorophyll-a  

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

## References and links

**OCx:**  
O'Reilly, John & Maritorena, S. & Mitchell, B.G. & Siegel, David & Carder, Kendall & Garver, S.A. & Kahru, Mati & Mcclain, Charles. (1998). Ocean color chlorophyll algorithms for SeaWiFS. Journal of Geophysical Research. 103. 937-953.  

**GSM:**  
Maritorena, Stephane & Siegel, David & Peterson, Alan. (2002). Optimization of a semianalytical ocean color model for global-scale application. Applied optics. 41. 2705-14. 10.1364/AO.41.002705.  

**QAA:**  
Lee, Zhongping & Carder, Kendall & Arnone, Robert. (2002). Deriving Inherent Optical Properties from Water Color: a Multiband Quasi-Analytical Algorithm for Optically Deep Waters. Applied optics. 41. 5755-72. 10.1364/AO.41.005755.  

**OCI:**  
Hu, Chuanmin & Lee, Zhongping & Franz, Bryan. (2012). Chlorophyll a algorithms for oligotrophic oceans: A novel approach based on three-band reflectance difference. Journal of Geophysical Research. 117. C01011. 10.1029/2011JC007395.  

**EOF:**  
Laliberté, Julien & Larouche, Pierre & Devred, Emmanuel & Craig, Susanne. (2018). Chlorophyll-a Concentration Retrieval in the Optically Complex Waters of the St. Lawrence Estuary and Gulf Using Principal Component Analysis. Remote Sensing. 10. 10.3390/rs10020265.  

**Regional tuning for OCx, GSM:**  
Clay, S.; Pena, A.; DeTracey, B.; Devred, E. Evaluation of Satellite-Based Algorithms to Retrieve Chlorophyll-a Concentration in the Canadian Atlantic and Pacific Oceans. Remote Sens. 2019, 11, 2609.  

**Integerized Sinusoidal Binning Scheme:**  
https://oceancolor.gsfc.nasa.gov/docs/format/l3bins/  



