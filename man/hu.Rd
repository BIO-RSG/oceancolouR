% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CHLA_Hu.R
\name{hu}
\alias{hu}
\title{Hu chlorophyll-a algorithm}
\usage{
hu(rrs, wave, coefs = get_ci_coefs(2))
}
\arguments{
\item{rrs}{Either: Numeric matrix where rows = records, columns = Rrs wavebands, with named columns ("Rrs_XXX", where XXX is a wavelength in nanometres, ordered from blue to green to red), OR: RasterStack or RasterBrick of rrs layers with stack layers following the same naming convention.}

\item{wave}{Numeric vector of Rrs wavebands matching those used in the column name(s) of rrs, arranged from shortest waveband to longest (example: c(443, 555, 670)).}

\item{coefs}{Numeric vector of coefficients corresponding to terms in the polynomial (lowest degree to highest). See ?get_ci_coefs}
}
\value{
For matrix rrs: Numeric value (or vector) -- chlorophyll as computed by Hu for the given Rrs, wavebands, and coefficients. For RasterStack/Brick rrs: equivalent raster with Hu chlorophyll-a.
}
\description{
Given a set of Rrs, and corresponding wavebands and coefficients, calculate chlorophyll using the Hu algorithm:
}
\details{
10^(a + b*CI), where CI is the color index calculated by ci()

(similar to the OCx algorithm, but using CI instead of a band ratio, and a first degree polynomial instead of a 3rd or 4th degree.)

The coefficients are not sensor-specific, but there are two different versions (first calculated in Hu et al 2012, later optimized in 2019).

This uses the SeaWiFS bands centered at 443, 555, and 670nm. For sensors with other wavebands, it uses the band closest to those (note that for MODIS, the closest "ocean" band is 547nm, not 555nm, which is for land). The difference in Rrs in the "green" band can be noticeable with a shift in wavelength, so if the sensor does not use the 555nm band, conv_rrs_to_555() is used to convert the Rrs at the green wavelength to Rrs at 555nm. This is not done with the red band because the difference in Rrs in that area of the spectrum is not as noticeable with a small shift in wavelength.
}
\examples{
# Some in situ chl / MODIS Rrs data used in Clay et al (2019)
in_situ_chl <- c(0.085,0.09,0.1,0.106,0.107,0.122,0.122,0.126,0.128,0.133,0.134,0.143,0.153,0.166,0.166,0.173,0.175,0.182,0.189,0.194,0.197,0.198,0.199,0.208,0.225,0.236,0.249,0.254,0.254,0.258,0.259,0.262,0.266,0.274,0.276,0.28,0.283,0.305,0.311,0.314,0.314,0.318,0.326,0.327,0.34,0.355,0.404,0.405,0.413,0.423,0.435,0.46,0.473,0.493,0.53,0.66,0.66,0.672,0.691,0.726,0.752,0.803,0.803,0.806,0.817,0.829,0.889,0.895,0.897,0.976,1.004,1.006,1.036,1.161,1.219,1.252,1.253,1.835,2.075,2.551,4.117,5.297)
rrs <- matrix(c(0.01042,0.01143,0.00645,0.01166,0.00751,0.00482,0.01019,0.01377,0.00759,0.00724,0.00627,0.00987,0.0101,0.0076,0.00818,0.00637,0.0116,0.00718,0.0091,0.00814,0.0158,0.00721,0.0137,0.00832,0.00442,0.00858,0.01847,0.01217,0.01029,0.00905,0.006,0.01071,0.01411,0.00882,0.00682,0.0096,0.0103,0.00565,0.01648,0.01592,0.01125,0.00791,0.01568,0.01148,0.00789,0.01236,0.00632,0.01144,0.01533,0.0119,0.01201,0.02022,0.00814,0.00768,0.00603,0.00911,0.01096,0.01402,0.00946,0.01063,0.01438,0.01168,0.0117,0.01114,0.01317,0.0099,0.01077,0.00826,0.01352,0.01026,0.01035,0.01166,0.01033,0.01148,0.00933,0.01217,0.01078,0.01174,0.01237,0.00587,0.01591,0.01053,0.00343,0.00407,0.00237,0.00291,0.00323,0.00166,0.00346,0.00377,0.00321,0.00352,0.00281,0.00339,0.0039,0.00357,0.00262,0.00297,0.00358,0.00341,0.00365,0.00331,0.00498,0.00351,0.00499,0.00346,0.00195,0.00361,0.00623,0.00458,0.00348,0.00388,0.00272,0.00458,0.00485,0.00317,0.00313,0.00457,0.004,0.00244,0.00487,0.00597,0.00487,0.00375,0.00578,0.0047,0.00358,0.0035,0.00305,0.00475,0.00558,0.00373,0.00454,0.00624,0.00366,0.00374,0.00268,0.00441,0.00417,0.00431,0.00339,0.00388,0.00476,0.00494,0.00434,0.00476,0.0052,0.00352,0.00461,0.00329,0.00507,0.00431,0.00384,0.00494,0.00441,0.0034,0.0041,0.00477,0.0034,0.00524,0.00518,0.00242,0.00628,0.00384,0.00038,0.00051,0.00023,0.00012,0.00039,2e-05,0.00028,0.00027,0.00028,0.00048,0.00033,0.00029,0.00014,0.00039,9e-05,0.00035,0.00024,0.00031,0.00039,2e-04,0.00054,0.00037,0.00044,0.00033,1e-04,0.00048,0.00065,0.00033,0.00029,0.00053,2e-04,0.00065,0.00051,0.00033,0.00025,0.00067,0.00055,0.00015,0.00034,0.00066,0.00047,0.00038,6e-04,0.00029,0.00044,0.00019,0.00038,5e-04,0.00082,0.00019,0.00058,0.00042,0.00041,4e-04,0.00022,0.00068,0.00032,0.00045,0.00025,0.00015,0.00031,0.00034,0.00065,5e-04,0.00074,0.00048,0.00061,0.00051,0.00037,0.00048,0.00023,0.00032,0.00049,0.00049,0.00052,0.00034,0.00051,0.00031,0.00119,0.00039,0.00061,0.00022), ncol=3)
colnames(rrs) <- c("Rrs_443", "Rrs_547", "Rrs_667")
wave <- get_ci_bands("modisaqua")
coefs <- get_ci_coefs(2)
chl_hu <- hu(rrs, wave, coefs)
plot(in_situ_chl, chl_hu, xlim=c(0.01,0.5), ylim=c(0.01,0.5))

# transform rrs matrix into RasterStack, and test it
library(raster)
library(magrittr)
rrs_stack <- stack(lapply(1:3, function(i) raster(matrix(rrs[1:9,i], nrow=3))))
names(rrs_stack) <- colnames(rrs)
chl_raster <- hu(rrs_stack, wave, coefs)
plot(chl_raster)

}
\references{
Original paper:

Hu, Chuanmin & Lee, Zhongping & Franz, Bryan. (2012). Chlorophyll a algorithms for oligotrophic oceans: A novel approach based on three-band reflectance difference. Journal of Geophysical Research. 117. C01011. 10.1029/2011JC007395.

Updates:

Hu, C., Feng, L., Lee, Z., Franz, B. A., Bailey, S. W., Werdell, P. J., & Proctor, C. W. (2019). Improving satellite global chlorophyll a data products through algorithm refinement and data recovery. Journal of Geophysical Research: Oceans, 124, 1524– 1543. https://doi.org/10.1029/2019JC014941

Link to NASA chlorophyll-a description:

https://oceancolor.gsfc.nasa.gov/atbd/chlor_a/
}
