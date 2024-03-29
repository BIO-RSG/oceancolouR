% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CHLA_QAA.R
\name{qaa}
\alias{qaa}
\title{QAA algorithm, version 6}
\usage{
qaa(
  rrs,
  lambda,
  c1 = -1.146,
  c2 = -1.366,
  c3 = -0.469,
  aw_all = list(`410` = 0.00473, `412` = 0.00455056, `443` = 0.00706914, `469` =
    0.0104326, `486` = 0.0139217, `488` = 0.0145167, `490` = 0.015, `510` = 0.0325, `531`
    = 0.0439153, `547` = 0.0531686, `551` = 0.0577925, `555` = 0.0596, `645` = 0.325,
    `667` = 0.434888, `670` = 0.439, `671` = 0.442831, `678` = 0.462323),
  bbw_all = list(`410` = 0.00339515, `412` = 0.003325, `443` = 0.002436175, `469` =
    0.001908315, `486` = 0.0016387, `488` = 0.001610175, `490` = 0.001582255, `510` =
    0.001333585, `531` = 0.001122495, `547` = 0.000988925, `551` = 0.000958665, `555` =
    0.000929535, `645` = 0.00049015, `667` = 0.000425025, `670` = 0.000416998, `671` =
    0.000414364, `678` = 0.000396492),
  aphstar_all = list(`410` = 0.054343207, `412` = 0.055765253, `443` = 0.063251586, `469`
    = 0.051276462, `486` = 0.041649554, `488` = 0.040647623, `490` = 0.039546143, `510` =
    0.025104817, `531` = 0.015745358, `547` = 0.011477324, `551` = 0.010425453, `555` =
    0.009381989, `645` = 0.008966522, `667` = 0.019877564, `670` = 0.022861409, `671` =
    0.023645549, `678` = 0.024389358)
)
}
\arguments{
\item{rrs}{Remote sensing reflectances above sea level, numeric vector with names matching the wavebands in lambda with "Rrs_" prefixes (e.g. if lambda=c(443,490) then rrs names are c("Rrs_443","Rrs_490"))}

\item{lambda}{Wavelengths corresponding to rrs, numeric vector}

\item{c1}{First coefficient used in the model, default are those used for SeaWiFS}

\item{c2}{Second coefficient}

\item{c3}{Third coefficient}

\item{aw_all}{Named list of water absorption coefficients (names must be same as lambda)}

\item{bbw_all}{Named list of water backscattering coefficients (names must be same as lambda)}

\item{aphstar_all}{Named list of specific absorption coefficients (i.e. absorption per unit chlorophyll-a, names must be same as lambda)}
}
\value{
Named list containing wavelengths used in the fit (numeric vector), phytoplankton absorption coefficients (numeric vector, one for each wavelength), and chlorophyll-a (single numeric value, mg m^-3, computed as the median of aph/aphstar).
}
\description{
Compute phytoplankton absorption coefficients for each wavelength, and a single chlorophyll-a value, using QAA (the Quasi-Analytical Algorithm).
}
\details{
This code was originally written for SeaWiFS using wavelengths 412, 443, 490, 555, and 670nm. If other sensors/wavelengths are used, the function will choose the wavelengths closest to the wavelengths listed above.

Wavelengths/lambda typically used for each sensor: 412,443,469,488,531,547,555,645,667,678 (MODIS), 412,443,490,510,555,670 (SeaWiFS), 410,443,486,551,671 (VIIRS).

Sources for default aw_all, bbw_all, and aphstar_all, respectively: Pope and Fry 1997 (https://oceancolor.gsfc.nasa.gov/docs/rsr/water_coef.txt), Smith and Baker 1981 (https://oceancolor.gsfc.nasa.gov/docs/rsr/water_coef.txt, this does not account for salinity effects on backscattering like the values in Zhang 2009), and DFO cruise records containing aph and chlorophyll-a values which were converted to aphstar by mean(aph/chl).
}
\examples{
# create a matrix of MODIS-Aqua rrs values for testing
rrs <- matrix(c(0.001974, 0.002002, 0.002044, 0.001932, 0.002296, 0.001708, 0.002570,
                0.002280, 0.002582, 0.002558, 0.002746, 0.001990, 0.003086, 0.002964,
                0.002986, 0.003030, 0.003100, 0.002572, 0.002974, 0.002748, 0.002914,
                0.002784, 0.002954, 0.002564, 0.002174, 0.002086, 0.002194, 0.002054,
                0.002496, 0.002342, 0.001862, 0.001784, 0.001850, 0.001764, 0.002220,
                0.002096, 0.001670, 0.001512, 0.001780, 0.001666, 0.001992, 0.001834,
                0.000324, 0.000256, 0.000216, 0.000344, 0.000494, 0.000440, 0.000256,
                0.000214, 0.000216, 0.000242, 0.000330, 0.000352, 0.000250, 0.000244,
                0.000270, 0.000294, 0.000382, 0.000402), nrow=6, ncol=10)
# select wavelengths (these are the defaults for MODIS-Aqua)
lambda <- c(412, 443, 469, 488, 531, 547, 555, 645, 667, 678)
# run qaa to process one Rrs record
test_qaa <- qaa(rrs=rrs[1,], lambda=lambda)
# print results
cat("\n\nSingle record:\n\n")
print(test_qaa)
# run qaa to process multiple records stored in an rrs matrix, where rows=records and columns=wavelengths
test_qaa <- apply(X=rrs, MARGIN=1, FUN=qaa, lambda=lambda)
# convert to matrix/vector format for easier printing
aph <- matrix(as.numeric(sapply(1:length(test_qaa), function(i) {test_qaa[[i]]$aph})),nrow=length(test_qaa),byrow=TRUE)
chl <- as.numeric(sapply(1:length(test_qaa), function(i) {test_qaa[[i]]$chl}))
# print results
cat("\n\n\nSet of records:\n\n")
cat("wavelengths:\n")
print(test_qaa[[1]]$wvs_used)
cat("\naph:\n")
print(aph)
cat("\nchl:\n")
print(chl)
}
\references{
Original paper:

Lee, Zhongping & Carder, Kendall & Arnone, Robert. (2002).
Deriving Inherent Optical Properties from Water Color:
a Multiband Quasi-Analytical Algorithm for Optically Deep Waters.
Applied optics. 41. 5755-72. 10.1364/AO.41.005755.
https://www.researchgate.net/publication/11140186_Deriving_Inherent_Optical_Properties_from_Water_Color_a_Multiband_Quasi-Analytical_Algorithm_for_Optically_Deep_Waters

Version 6 updates:

http://www.ioccg.org/groups/Software_OCA/QAA_v6_2014209.pdf
}
