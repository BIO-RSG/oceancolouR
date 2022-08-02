# oceancolouR <a href='https://github.com/BIO-RSG/oceancolouR/blob/master/README.md'><img src='man/figures/logoraw.png' align="right" height="139" /></a>


This is a collection of frequently used R functions for Ocean Colour Remote Sensing data processing and analysis.
This package is in continuing development, but the following list details some (not all) of the functions included.

**Chlorophyll-a algorithms:**

* `gsm()`: R implementation of the GSM algorithm, with traditional coefficients or coefficients calculated with `get_gs()`
* `ocx()` : OCX algorithm for MODIS-Aqua, SNPP-VIIRS and SeaWiFS. OCX coefficients can be optimized with `optimize_ocx_coefs()`
* `qaa()`: QAA algorithm (v6) to calculate chlorophyll-a as well as phytoplankton absorption coefficients
* `eof_chl()`: EOF (empirical orthogonal function) method to calculate chlorophyll-a  
* `hu()` : Hu algorithm for MODIS-Aqua, SNPP-VIIRS and SeaWiFS  
* `oci()` : OCI algorithm for MODIS-Aqua, SNPP-VIIRS and SeaWiFS. R implementation of NASA OBPG's chlor_a variable in OC (ocean colour) satellite images.  


**Other:** 

* `filtered_mean()`: calculate the filtered mean (as in Bailey and Werdell, 2006)
* `read_pixEx()`: load output file of SNAP pixEx pixel extraction tool as data.frame
* `read_h5_L3b()`: read the contents of a NASA Level-3 binned file in h5 format
* `haversine()`: calculate haversine distance between two points
* `geoMean()`, `geoSD()`: calculate geometric mean and geometric standard deviation factor
* `week8()`, `week8_date`: convert to/from standard 8-day week number and date
* `days_vector()`: list julian days for a given year/month

## How to install

``` r
remotes::install_github("BIO-RSG/oceancolouR", build_vignettes = TRUE)
#devtools::install_github("BIO-RSG/oceancolouR", build_vignettes = TRUE)
```

## Vignette

To access the vignette in RStudio, type the following in the console:  

``` r
vignette(topic="introduction", package="oceancolouR")
```


## Datasets

There are numeric vectors of the coordinates used in NASA binned images subset to the Pan-Canadian grid including the Northwest Atlantic, Northeast Pacific and Gulf of Saint Lawrence (approximate boundaries defined below in decimal degrees). The bin numbers and their corresponding latitudes, longitudes, and bathymetry values are included at both 4km and 9km resolution.

    * Pan-Canadian grid (pancan):      -147 to -41, 39 to 86  
    * Northwest Atlantic (nwa):       -95 to -42, 39 to 82  
    * Northeast Pacific (nep):        -140 to -122, 46 to 60  
    * Gulf of Saint Lawrence (gosl):  -75 to -49, 41 to 53  

Datasets can be loaded using the following command:  
__data("*region_variable_resolution*")__  
where:  
    **region** is either *pancan*, *nwa*, *nep*, or *gosl*  
    **variable** is either *bins*, *lats*, *lons*, or *bath*  
    **resolution** is either *4km* or *9km*  

## References and links

**OCx:**  
O'Reilly, John & Maritorena, S. & Mitchell, B.G. & Siegel, David & Carder, Kendall & Garver, S.A. & Kahru, Mati & Mcclain, Charles. (1998). [Ocean color chlorophyll algorithms for SeaWiFS.](https://doi.org/10.1029/98JC02160) Journal of Geophysical Research. 103. 937-953.  10.1029/98JC02160.

**GSM:**  
Maritorena, Stephane & Siegel, David & Peterson, Alan. (2002). [Optimization of a semianalytical ocean color model for global-scale application.](https://doi.org/10.1364/AO.41.002705) Applied optics. 41. 2705-14. 10.1364/AO.41.002705.  

**QAA:**  
Lee, Zhongping & Carder, Kendall & Arnone, Robert. (2002). [Deriving Inherent Optical Properties from Water Color: a Multiband Quasi-Analytical Algorithm for Optically Deep Waters.](https://doi.org/10.1364/AO.41.005755) Applied optics. 41. 5755-72. 10.1364/AO.41.005755.  

**OCI:**  
Hu, Chuanmin & Lee, Zhongping & Franz, Bryan. (2012). [Chlorophyll a algorithms for oligotrophic oceans: A novel approach based on three-band reflectance difference.](https://doi.org/10.1029/2011JC007395) Journal of Geophysical Research. 117. C01011. 10.1029/2011JC007395.  

**EOF:**  
Laliberté, Julien & Larouche, Pierre & Devred, Emmanuel & Craig, Susanne. (2018). [Chlorophyll-a Concentration Retrieval in the Optically Complex Waters of the St. Lawrence Estuary and Gulf Using Principal Component Analysis.](https://doi.org/10.3390/rs10020265) Remote Sensing. 10. 10.3390/rs10020265.  

**Regional tuning for OCx (POLY4), GSM (GSM_GC, GSM_GS):**  
Clay, S.; Pena, A.; DeTracey, B.; Devred, E. Evaluation of Satellite-Based Algorithms to Retrieve Chlorophyll-a Concentration in the Canadian Atlantic and Pacific Oceans. Remote Sens. 2019, 11, 2609.  

**Integerized Sinusoidal Binning Scheme:**  
https://oceancolor.gsfc.nasa.gov/docs/format/l3bins/  

**In situ matchups and filtered mean:**
Bailey, Sean & Werdell, Jeremy. (2006). A multi-sensor approach for the on-orbit validation of ocean color satellite data products. Remote Sensing of Environment. 102. 12-23. 10.1016/j.rse.2006.01.015.



