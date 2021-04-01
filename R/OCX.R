#' Color Index Algorithm
#'
#' Given blue, green, and red Rrs (remote sensing reflectances), calculate the color index used in Hu et al (2012).
#'
#' The blue, green, and red wavebands are 443, 555, and 670, respectively. See ?hu or the example below for details about using sensors with other wavebands.
#'
#' @param rrs_blue Numeric vector of remote sensing reflectances (Rrs) from the "blue" waveband
#' @param rrs_green Numeric vector of remote sensing reflectances (Rrs) from the "green" waveband
#' @param rrs_red Numeric vector of remote sensing reflectances (Rrs) from the "red" waveband
#' @references
#' Hu, Chuanmin & Lee, Zhongping & Franz, Bryan. (2012). Chlorophyll a algorithms for oligotrophic oceans: A novel approach based on three-band reflectance difference. Journal of Geophysical Research. 117. C01011. 10.1029/2011JC007395.
#'
#' Link to NASA chlorophyll-a description:
#'
#' https://oceancolor.gsfc.nasa.gov/atbd/chlor_a/
#'
#' @return Numeric vector of color index values
#' @examples
#' # Some in situ chl / MODIS Rrs data used in Clay et al (2019)
#' in_situ_chl <- c(0.085,0.09,0.1,0.106,0.107,0.122,0.122,0.126,0.128,0.133,0.134,0.143,0.153,0.166,0.166,0.173,0.175,0.182,0.189,0.194,0.197,0.198,0.199,0.208,0.225,0.236,0.249,0.254,0.254,0.258,0.259,0.262,0.266,0.274,0.276,0.28,0.283,0.305,0.311,0.314,0.314,0.318,0.326,0.327,0.34,0.355,0.404,0.405,0.413,0.423,0.435,0.46,0.473,0.493,0.53,0.66,0.66,0.672,0.691,0.726,0.752,0.803,0.803,0.806,0.817,0.829,0.889,0.895,0.897,0.976,1.004,1.006,1.036,1.161,1.219,1.252,1.253,1.835,2.075,2.551,4.117,5.297)
#' rrs <- matrix(c(0.01042,0.01143,0.00645,0.01166,0.00751,0.00482,0.01019,0.01377,0.00759,0.00724,0.00627,0.00987,0.0101,0.0076,0.00818,0.00637,0.0116,0.00718,0.0091,0.00814,0.0158,0.00721,0.0137,0.00832,0.00442,0.00858,0.01847,0.01217,0.01029,0.00905,0.006,0.01071,0.01411,0.00882,0.00682,0.0096,0.0103,0.00565,0.01648,0.01592,0.01125,0.00791,0.01568,0.01148,0.00789,0.01236,0.00632,0.01144,0.01533,0.0119,0.01201,0.02022,0.00814,0.00768,0.00603,0.00911,0.01096,0.01402,0.00946,0.01063,0.01438,0.01168,0.0117,0.01114,0.01317,0.0099,0.01077,0.00826,0.01352,0.01026,0.01035,0.01166,0.01033,0.01148,0.00933,0.01217,0.01078,0.01174,0.01237,0.00587,0.01591,0.01053,0.00343,0.00407,0.00237,0.00291,0.00323,0.00166,0.00346,0.00377,0.00321,0.00352,0.00281,0.00339,0.0039,0.00357,0.00262,0.00297,0.00358,0.00341,0.00365,0.00331,0.00498,0.00351,0.00499,0.00346,0.00195,0.00361,0.00623,0.00458,0.00348,0.00388,0.00272,0.00458,0.00485,0.00317,0.00313,0.00457,0.004,0.00244,0.00487,0.00597,0.00487,0.00375,0.00578,0.0047,0.00358,0.0035,0.00305,0.00475,0.00558,0.00373,0.00454,0.00624,0.00366,0.00374,0.00268,0.00441,0.00417,0.00431,0.00339,0.00388,0.00476,0.00494,0.00434,0.00476,0.0052,0.00352,0.00461,0.00329,0.00507,0.00431,0.00384,0.00494,0.00441,0.0034,0.0041,0.00477,0.0034,0.00524,0.00518,0.00242,0.00628,0.00384,0.00038,0.00051,0.00023,0.00012,0.00039,2e-05,0.00028,0.00027,0.00028,0.00048,0.00033,0.00029,0.00014,0.00039,9e-05,0.00035,0.00024,0.00031,0.00039,2e-04,0.00054,0.00037,0.00044,0.00033,1e-04,0.00048,0.00065,0.00033,0.00029,0.00053,2e-04,0.00065,0.00051,0.00033,0.00025,0.00067,0.00055,0.00015,0.00034,0.00066,0.00047,0.00038,6e-04,0.00029,0.00044,0.00019,0.00038,5e-04,0.00082,0.00019,0.00058,0.00042,0.00041,4e-04,0.00022,0.00068,0.00032,0.00045,0.00025,0.00015,0.00031,0.00034,0.00065,5e-04,0.00074,0.00048,0.00061,0.00051,0.00037,0.00048,0.00023,0.00032,0.00049,0.00049,0.00052,0.00034,0.00051,0.00031,0.00119,0.00039,0.00061,0.00022), ncol=3)
#' colnames(rrs) <- c("Rrs_443", "Rrs_547", "Rrs_667")
#' rrs_blue <- rrs[,1]
#' rrs_green <- rrs[,2]
#' rrs_red <- rrs[,3]
#' # get wavebands of selected sensor, and coefficients from the 2019 Hu paper
#' wave <- get_ci_bands("modis")
#' coefs <- get_ci_coefs(2)
#' # if green waveband is not 555nm, convert green Rrs to Rrs at 555nm
#' if (wave[2] != 555) {
#'     rrs_green <- conv_rrs_to_555(rrs_green, wave[2])
#' }
#' ci_result <- ci(rrs_blue, rrs_green, rrs_red)
#'
#' @export
ci <- function(rrs_blue, rrs_green, rrs_red) {
    return(rrs_green - (rrs_blue + (rrs_red - rrs_blue)*(555-443)/(670-443)))
}


#' Convert green Rrs to Rrs at 555nm
#'
#' Given a numeric vector of Rrs at a green waveband not centered at 555nm, convert the Rrs to Rrs at 555nm.
#'
#' @param rrs Numeric vector of remote sensing reflectances (Rrs) from the "green" waveband
#' @param wave Numeric value, "green" waveband associated with these rrs values (example: 547)
#' @references
#' Adapted from function coded at NASA OBPG. Source:
#'
#' https://oceancolor.sci.gsfc.nasa.gov/docs/ocssw/convert__band_8c_source.html
#'
#' @return Numeric vector of Rrs converted to 555nm
#' @export
conv_rrs_to_555 <- function(rrs, wave) {
     if (abs(wave - 555) > 2) {
         if (abs(wave - 547) <= 2) {
             sw = 0.001723
             a1 = 0.986
             b1 = 0.081495
             a2 = 1.031
             b2 = 0.000216
         } else if (abs(wave - 550) <= 2) {
             sw = 0.001597
             a1 = 0.988
             b1 = 0.062195
             a2 = 1.014
             b2 = 0.000128
         } else if (abs(wave - 560) <= 2) {
             sw = 0.001148
             a1 = 1.023
             b1 = -0.103624
             a2 = 0.979
             b2 = -0.000121
         } else if (abs(wave - 565) <= 2) {
             sw = 0.000891
             a1 = 1.039
             b1 = -0.183044
             a2 = 0.971
             b2 = -0.000170
         } else {
             print(paste0("Unable to convert Rrs at ", wave, "nm to 555nm for use in OCI algorithm."))
             stop()
         }
         rrs = a2 * rrs - b2
         rrs[rrs < sw] = 10.0^ (a1 * log10(rrs[rrs < sw]) - b1)

     }
     return (rrs)
}


#' Get coefficients to use in the Hu chlorophyll-a algorithm.
#'
#' Given a version number (either 1 or 2), get the Hu coefficients. Version 1 coefficients are from the 2012 paper, version 2 are from the 2019 paper.
#'
#' @param version Single numeric value, either 1 or 2
#' @references
#' #' Original paper:
#'
#' Hu, Chuanmin & Lee, Zhongping & Franz, Bryan. (2012). Chlorophyll a algorithms for oligotrophic oceans: A novel approach based on three-band reflectance difference. Journal of Geophysical Research. 117. C01011. 10.1029/2011JC007395.
#'
#' Updates:
#'
#' Hu, C., Feng, L., Lee, Z., Franz, B. A., Bailey, S. W., Werdell, P. J., & Proctor, C. W. (2019). Improving satellite global chlorophyll a data products through algorithm refinement and data recovery. Journal of Geophysical Research: Oceans, 124, 1524– 1543. https://doi.org/10.1029/2019JC014941
#'
#' Link to NASA chlorophyll-a description:
#'
#' https://oceancolor.gsfc.nasa.gov/atbd/chlor_a/
#'
#' @return Numeric vector of coefficients, from lowest degree to highest.
#' @export
get_ci_coefs <- function(version) {
    version <- as.numeric(version)
    if (!(version %in% 1:2)) {stop("Version number must be 1 or 2.")}
    if (version==1) {
        return(c(-0.4909, 191.6590))
    } else if (version==2) {
        return(c(-0.4287, 230.47))
    }
}


#' Get wavebands to use in the color index algorithm
#'
#' Given a sensor name, get the appropriate wavebands closest to 443, 555, and 670nm. Note that for MODIS, the 547nm band should be used for the green band (547nm is the "ocean" band, 555nm is the "land" band).
#'
#' @param sensor String, either "modis" (for MODIS-Aqua), "seawifs", or "viirs" (for VIIRS-SNPP)
#' @references
#' Link to NASA chlorophyll-a description:
#'
#' https://oceancolor.gsfc.nasa.gov/atbd/chlor_a/
#'
#' @return Numeric vector of wavebands for the selected sensor, to use in the color index and Hu algorithm.
#' @export
get_ci_bands <- function(sensor) {
    stopifnot(sensor %in% c("modis", "seawifs", "viirs"))
    if (sensor=="modis") {
        return(c(443,547,667))
    } else if (sensor=="seawifs") {
        return(c(443,555,670))
    } else if (sensor=="viirs") {
        return(c(443,551,671))
    }
}


#' Hu chlorophyll-a algorithm
#'
#' Given a set of Rrs, and corresponding wavebands and coefficients, calculate chlorophyll using the Hu algorithm:
#'
#' 10^(a + b*CI), where CI is the color index calculated by ci()
#'
#' (similar to the OCx algorithm, but using CI instead of a band ratio, and a first degree polynomial instead of a 3rd or 4th degree.)
#'
#' The coefficients are not sensor-specific, but there are two different versions (first calculated in Hu et al 2012, later optimized in 2019).
#'
#' This uses the SeaWiFS bands centered at 443, 555, and 670nm. For sensors with other wavebands, it uses the band closest to those (note that for MODIS, the closest "ocean" band is 547nm, not 555nm, which is for land). The difference in Rrs in the "green" band can be noticeable with a shift in wavelength, so if the sensor does not use the 555nm band, conv_rrs_to_555() is used to convert the Rrs at the green wavelength to Rrs at 555nm. This is not done with the red band because the difference in Rrs in that area of the spectrum is not as noticeable with a small shift in wavelength.
#'
#' @param rrs Numeric matrix where rows = records, columns = Rrs wavebands, with named columns ("Rrs_XXX", where XXX is a wavelength in nanometres, ordered from blue to green to red).
#' @param wave Numeric vector of Rrs wavebands matching those used in the column name(s) of rrs, arranged from shortest waveband to longest (example: c(443, 555, 670)).
#' @param coefs Numeric vector of coefficients corresponding to terms in the polynomial (lowest degree to highest). See ?get_ci_coefs
#' @references
#' Original paper:
#'
#' Hu, Chuanmin & Lee, Zhongping & Franz, Bryan. (2012). Chlorophyll a algorithms for oligotrophic oceans: A novel approach based on three-band reflectance difference. Journal of Geophysical Research. 117. C01011. 10.1029/2011JC007395.
#'
#' Updates:
#'
#' Hu, C., Feng, L., Lee, Z., Franz, B. A., Bailey, S. W., Werdell, P. J., & Proctor, C. W. (2019). Improving satellite global chlorophyll a data products through algorithm refinement and data recovery. Journal of Geophysical Research: Oceans, 124, 1524– 1543. https://doi.org/10.1029/2019JC014941
#'
#' Link to NASA chlorophyll-a description:
#'
#' https://oceancolor.gsfc.nasa.gov/atbd/chlor_a/
#'
#' @return Numeric value (or vector) -- chlorophyll as computed by hu for the given Rrs, wavebands, and coefficients
#' @examples
#' # Some in situ chl / MODIS Rrs data used in Clay et al (2019)
#' in_situ_chl <- c(0.085,0.09,0.1,0.106,0.107,0.122,0.122,0.126,0.128,0.133,0.134,0.143,0.153,0.166,0.166,0.173,0.175,0.182,0.189,0.194,0.197,0.198,0.199,0.208,0.225,0.236,0.249,0.254,0.254,0.258,0.259,0.262,0.266,0.274,0.276,0.28,0.283,0.305,0.311,0.314,0.314,0.318,0.326,0.327,0.34,0.355,0.404,0.405,0.413,0.423,0.435,0.46,0.473,0.493,0.53,0.66,0.66,0.672,0.691,0.726,0.752,0.803,0.803,0.806,0.817,0.829,0.889,0.895,0.897,0.976,1.004,1.006,1.036,1.161,1.219,1.252,1.253,1.835,2.075,2.551,4.117,5.297)
#' rrs <- matrix(c(0.01042,0.01143,0.00645,0.01166,0.00751,0.00482,0.01019,0.01377,0.00759,0.00724,0.00627,0.00987,0.0101,0.0076,0.00818,0.00637,0.0116,0.00718,0.0091,0.00814,0.0158,0.00721,0.0137,0.00832,0.00442,0.00858,0.01847,0.01217,0.01029,0.00905,0.006,0.01071,0.01411,0.00882,0.00682,0.0096,0.0103,0.00565,0.01648,0.01592,0.01125,0.00791,0.01568,0.01148,0.00789,0.01236,0.00632,0.01144,0.01533,0.0119,0.01201,0.02022,0.00814,0.00768,0.00603,0.00911,0.01096,0.01402,0.00946,0.01063,0.01438,0.01168,0.0117,0.01114,0.01317,0.0099,0.01077,0.00826,0.01352,0.01026,0.01035,0.01166,0.01033,0.01148,0.00933,0.01217,0.01078,0.01174,0.01237,0.00587,0.01591,0.01053,0.00343,0.00407,0.00237,0.00291,0.00323,0.00166,0.00346,0.00377,0.00321,0.00352,0.00281,0.00339,0.0039,0.00357,0.00262,0.00297,0.00358,0.00341,0.00365,0.00331,0.00498,0.00351,0.00499,0.00346,0.00195,0.00361,0.00623,0.00458,0.00348,0.00388,0.00272,0.00458,0.00485,0.00317,0.00313,0.00457,0.004,0.00244,0.00487,0.00597,0.00487,0.00375,0.00578,0.0047,0.00358,0.0035,0.00305,0.00475,0.00558,0.00373,0.00454,0.00624,0.00366,0.00374,0.00268,0.00441,0.00417,0.00431,0.00339,0.00388,0.00476,0.00494,0.00434,0.00476,0.0052,0.00352,0.00461,0.00329,0.00507,0.00431,0.00384,0.00494,0.00441,0.0034,0.0041,0.00477,0.0034,0.00524,0.00518,0.00242,0.00628,0.00384,0.00038,0.00051,0.00023,0.00012,0.00039,2e-05,0.00028,0.00027,0.00028,0.00048,0.00033,0.00029,0.00014,0.00039,9e-05,0.00035,0.00024,0.00031,0.00039,2e-04,0.00054,0.00037,0.00044,0.00033,1e-04,0.00048,0.00065,0.00033,0.00029,0.00053,2e-04,0.00065,0.00051,0.00033,0.00025,0.00067,0.00055,0.00015,0.00034,0.00066,0.00047,0.00038,6e-04,0.00029,0.00044,0.00019,0.00038,5e-04,0.00082,0.00019,0.00058,0.00042,0.00041,4e-04,0.00022,0.00068,0.00032,0.00045,0.00025,0.00015,0.00031,0.00034,0.00065,5e-04,0.00074,0.00048,0.00061,0.00051,0.00037,0.00048,0.00023,0.00032,0.00049,0.00049,0.00052,0.00034,0.00051,0.00031,0.00119,0.00039,0.00061,0.00022), ncol=3)
#' colnames(rrs) <- c("Rrs_443", "Rrs_547", "Rrs_667")
#' wave <- get_ci_bands("modis")
#' coefs <- get_ci_coefs(2)
#' chl_hu <- hu(rrs, wave, coefs)
#' plot(in_situ_chl, chl_hu, xlim=c(0.01,0.5), ylim=c(0.01,0.5))
#'
#' @export
hu <- function(rrs, wave, coefs) {

    if (!all(colnames(rrs)==paste0("Rrs_", wave))) {
        stop("rrs column names must be in the form Rrs_XXX, where XXX is the waveband (nm) matching those in the wave variable, in the same order, from blue to green to red")
    }

    wblue <- wave[1]
    wgreen <- wave[2]
    wred <- wave[3]

    # subset to rrs at proper wavelengths, matching "wave"
    rrs_blue <- rrs[,1]
    rrs_green <- rrs[,2]
    rrs_red <- rrs[,3]

    # check if data is missing, and print error with the wavelength missing data
    if (length(rrs_blue)==0) {
        stop(paste0("Data missing for Rrs_", wblue))
    } else if (length(rrs_green)==0) {
        stop(paste0("Data missing for Rrs_", wgreen))
    } else if (length(rrs_red)==0) {
        stop(paste0("Data missing for Rrs_", wred))
    }

    if (wgreen != 555) {
        # for modis and viirs, shift green band to 555nm
        rrs_green <- conv_rrs_to_555(rrs_green, wgreen)
    }

    ci_res <- ci(rrs_blue, rrs_green, rrs_red)
    ci_res[ci_res > 0] <- 0

    return(10^(coefs[1] + coefs[2] * ci_res))

}


#' Calculate OCI, the chlorophyll-a algorithm that blends OCx and Hu.
#'
#' This is the algorithm currently used by NASA for chlor_a (as of April 2021).
#'
#' If Hu <= CI_bound1, the Hu is used. If it is between the two bounds, Hu and OCx are blended as described in Hu et al (2012).
#'
#' @param chl_hu Numeric vector of chlorophyll-a calculated using the Hu algorithm.
#' @param chl_ocx Numeric vector of chlorophyll-a calculated using the OCx algorithm.
#' @param CI_bound1 Single numeric value, the lower bound of the blending interval.
#' @param CI_bound2 Single numeric value, the upper bound of the blending interval.
#' @references
#' #' Original paper:
#'
#' Hu, Chuanmin & Lee, Zhongping & Franz, Bryan. (2012). Chlorophyll a algorithms for oligotrophic oceans: A novel approach based on three-band reflectance difference. Journal of Geophysical Research. 117. C01011. 10.1029/2011JC007395.
#'
#' Updates:
#'
#' Hu, C., Feng, L., Lee, Z., Franz, B. A., Bailey, S. W., Werdell, P. J., & Proctor, C. W. (2019). Improving satellite global chlorophyll a data products through algorithm refinement and data recovery. Journal of Geophysical Research: Oceans, 124, 1524– 1543. https://doi.org/10.1029/2019JC014941
#'
#' Link to NASA chlorophyll-a description:
#'
#' https://oceancolor.gsfc.nasa.gov/atbd/chlor_a/
#'
#' @return Numeric vector of OCI chlorophyll-a values.
#' @export
oci <- function(chl_hu, chl_ocx, CI_bound1, CI_bound2) {

    oci_chl <- chl_ocx

    # get indices to use OCI, and indices to blend OCI and OCX
    hu_ind <- chl_hu <= CI_bound1 & is.finite(chl_hu)
    blend_ind <- chl_hu > CI_bound1 & chl_hu < CI_bound2 & is.finite(chl_hu)

    # calculate coefficients for blending
    a <- (chl_hu[blend_ind] - CI_bound1)/(CI_bound2 - CI_bound1)
    b <- (CI_bound2 - chl_hu[blend_ind])/(CI_bound2 - CI_bound1)

    # change values to hu or a blend of hu/ocx at appropriate indices
    oci_chl[hu_ind] <- chl_hu[hu_ind]
    oci_chl[blend_ind] <- a*chl_ocx[blend_ind] + b*chl_hu[blend_ind]

    return(oci_chl)

}


#' Get predefined coefficients for OCx
#'
#' Library of existing optimized coefficients for the polynomial band ratio algorithm.
#' "ocx" returns the coefficients of the global ocean colour algorithm used by NASA (as of Nov 2020) for the selected sensor. "poly1" to "poly4" return the regionally-optimized coefficients for polynomial algorithms of degrees 1 to 4 for the selected sensor, for the two available regions: nwa (Northwest Atlantic) or nep (Northeast Pacific).
#'
#' @param sensor String, either "modis", "seawifs", or "viirs" (note: "modis" is MODIS-Aqua, and "viirs" is VIIRS-SNPP)
#' @param region String, either "nwa" or "nep"
#' @param alg String, either "poly1", "poly2", "poly3", "poly4", or "ocx"
#' @references
#' Clay, S.; Peña, A.; DeTracey, B.; Devred, E. Evaluation of Satellite-Based Algorithms to Retrieve Chlorophyll-a Concentration in the Canadian Atlantic and Pacific Oceans. Remote Sens. 2019, 11, 2609.
#' https://www.mdpi.com/2072-4292/11/22/2609
#' @return Numeric vector of polynomial coefficients, from the coefficient on the lowest degree term to highest degree.
#' @export
get_ocx_coefs <- function(sensor, region, alg) {

    stopifnot(sensor %in% c("modis", "seawifs", "viirs"),
              region %in% c("nwa", "nep"),
              alg %in% c("poly1", "poly2", "poly3", "poly4", "ocx"))

    coefs <- list("nwa"=list("modis"=list("poly1"=c(0.36695,-3.27757),
                                          "poly2"=c(0.37539,-3.12409,-0.75408),
                                          "poly3"=c(0.37657,-3.26173,-0.60435,1.1404),
                                          "poly4"=c(0.37925,-3.28487,-0.7583,1.49122,0.8002),
                                          "ocx"=c(0.2424,-2.7423,1.8017,0.0015,-1.228)),
                             "seawifs"=list("poly1"=c(0.51664,-3.84589),
                                            "poly2"=c(0.51424,-3.59265,-0.95058),
                                            "poly3"=c(0.52039,-3.75269,-0.92392,1.71524),
                                            "poly4"=c(0.51824,-3.68431,-0.97401,0.84875,0.77874),
                                            "ocx"=c(0.3272,-2.994,2.7218,-1.2259,-0.5683)),
                             "viirs"=list("poly1"=c(0.43399,-3.09652),
                                          "poly2"=c(0.41461,-2.54637,-1.47087),
                                          "poly3"=c(0.44156,-3.05795,-0.65894,1.21248),
                                          "poly4"=c(0.44786,-3.11091,-0.77987,1.425,0.90445),
                                          "ocx"=c(0.2228,-2.4683,1.5867,-0.4275,-0.7768))),
                  "nep"=list("modis"=list("poly1"=c(0.24947,-2.84152),
                                          "poly2"=c(0.28424,-2.66996,-1.09915),
                                          "poly3"=c(0.2805,-2.77728,-1.01747,0.92282),
                                          "poly4"=c(0.26575,-2.84142,-0.57938,0.74974,0.47743),
                                          "ocx"=c(0.2424,-2.7423,1.8017,0.0015,-1.228)),
                             "seawifs"=list("poly1"=c(0.41867,-3.14708),
                                            "poly2"=c(0.42171,-2.95509,-0.68104),
                                            "poly3"=c(0.42506,-2.74285,-1.48743,0.17624),
                                            "poly4"=c(0.42516,-3.14271,-0.70269,1.21802,1.59686),
                                            "ocx"=c(0.3272,-2.994,2.7218,-1.2259,-0.5683)),
                             "viirs"=list("poly1"=c(0.31886,-2.6501),
                                          "poly2"=c(0.33771,-2.56462,-0.5314),
                                          "poly3"=c(0.3303,-2.74252,-0.34545,1.35569),
                                          "poly4"=c(0.33055,-2.76455,-0.39595,1.52198,0.46509),
                                          "ocx"=c(0.2228,-2.4683,1.5867,-0.4275,-0.7768))))

    return(as.numeric(coefs[[region]][[sensor]][[alg]]))

}


#' Get OCX wavebands for a sensor
#'
#' Given a sensor name, get the wavebands typically used in the OCX algorithm.
#'
#' @param sensor String, either "modis", "seawifs", or "viirs" (note: "modis" is MODIS-Aqua, and "viirs" is VIIRS-SNPP)
#' @param use_443nm Logical value, TRUE to make the 443nm band an option in the band ratio
#' @return Named list of 2 character vectors, one for "green" waveband(s) and one for "blue"
#' @export
get_ocx_lambda <- function(sensor, use_443nm) {

    # Blue Rrs wavelengths used in band ratio algorithms
    all_blues <- list("modis"=c("Rrs_443","Rrs_488"),
                      "seawifs"=c("Rrs_443","Rrs_490","Rrs_510"),
                      "viirs"=c("Rrs_443","Rrs_486"))
    # Green Rrs wavelengths used in band ratio algorithms
    all_greens <- list("modis"="Rrs_547",
                       "seawifs"="Rrs_555",
                       "viirs"="Rrs_551") # algorithm for viirs uses 550, not 551

    blues <- all_blues[[sensor]]
    green <- all_greens[[sensor]]
    if (!use_443nm) {blues <- blues[blues != "Rrs_443"]}

    return(list(green=green, blues=blues))

}


#' Get band ratios
#'
#' Given a matrix of Rrs (remote sensing reflectances) with column names, and the blue and green wavebands to use, calculate the band ratios.
#'
#' @param rrs Numeric matrix where rows = records, columns = Rrs wavebands, with named columns ("Rrs_XXX", where XXX is a wavelength in nanometres). Names must match c(blues, green), i.e. same names, from shortest waveband to longest.
#' @param blues Character vector of Rrs wavebands in the blue range (e.g. c("Rrs_443", Rrs_488")), matching column name(s) in rrs, maximum 3 options, arranged from shortest waveband to longest. Note that if use_443nm=FALSE, the 443nm waveband will be removed and another must be used in its place.
#' @param green String, Rrs waveband in the green range (e.g. "Rrs_547"), matching a column name in rrs
#' @param use_443nm Logical value, TRUE to make the 443nm band an option in the band ratio
#' @return Named list of two vectors: rrs_ocx (the band ratio values), and ratio_used (strings indicating which "blue" waveband was used for each band ratio)
#' @export
get_br <- function(rrs, blues, green, use_443nm=FALSE) {

    full_rrs_ocx <- rep(NA, nrow(rrs))
    full_ratio_used <- rep(NA, nrow(rrs))

    # Get green Rrs.
    rrsg <- rrs[,colnames(rrs)==green]

    if (!use_443nm) {
        # Remove 443nm from the list of "blue" wavelength options
        blues <- blues[blues != "Rrs_443"]
    }
    # Get dataframe of blue Rrs.
    rrsb <- data.frame(rrs[,colnames(rrs) %in% blues],stringsAsFactors=F)
    colnames(rrsb) <- blues

    # Get first "blue" vector and start building valid index.
    ind <- is.finite(rrsb[,1]) & is.finite(rrsg)

    # # Exclude values with missing (negative) Rrs (bad atmospheric correction)
    # ind <- is.finite(rrs[,1])
    # for (j in 2:ncol(rrs)) {
    #     ind <- ind & is.finite(rrs[,j])
    # }

    # Update valid index
    if (length(blues) >= 2) {ind <- ind & is.finite(rrsb[,2])}
    if (length(blues) == 3) {ind <- ind & is.finite(rrsb[,3])}

    # Get band ratio
    r1 <- rrsb[ind,1]/rrsg[ind]
    rrs_ocx <- r1

    ratio_used <- rep(blues[1],length(r1))

    if (length(blues) >= 2) {
        r2 <- rrsb[ind,2]/rrsg[ind]
        rrs_ocx[r2 > r1] <- r2[r2 > r1]
        ratio_used[r2 > r1] <- blues[2]
    }

    if (length(blues) == 3) {
        r3 <- rrsb[ind,3]/rrsg[ind]
        rrs_ocx[r3 > r1 & r3 > r2] <- r3[r3 > r1 & r3 > r2]
        ratio_used[r3 > r1 & r3 > r2] <- blues[3]
    }

    full_rrs_ocx[ind] <- rrs_ocx
    full_ratio_used[ind] <- ratio_used

    return(list(rrs_ocx = full_rrs_ocx, ratio_used = full_ratio_used))

}


#' OCX algorithm
#'
#' Given a set of Rrs and coefficients, calculate chlorophyll using a polynomial band ratio algorithm. See ?optimize_ocx_coefs for example.
#'
#' This is the algorithm used by NASA (as of Nov 2020). It is replaced with the Hu algorithm (see ?hu) for Hu chla concentrations < 0.15 mg /m^3, and a blend of Hu and OCx for Hu concentrations between 0.15 and 0.2.
#'
#' @param rrs Either: Numeric matrix where rows = records, columns = Rrs wavebands, with named columns ("Rrs_XXX", where XXX is a wavelength in nanometres), OR: RasterStack of rrs layers with stack layers following the same naming convention. Names must match c(blues, green), i.e. same names, from shortest waveband to longest.
#' @param blues Character vector of Rrs wavebands in the blue range (e.g. c("Rrs_443", Rrs_488")), matching column name(s) in rrs, maximum 3 options, arranged from shortest waveband to longest. Note that if use_443nm=FALSE, the 443nm waveband will be removed and another must be used in its place.
#' @param green String, Rrs waveband in the green range (e.g. "Rrs_547"), matching a column name in rrs
#' @param coefs Numeric vector of coefficients corresponding to terms in the polynomial (lowest degree to highest)
#' @param use_443nm Logical value, TRUE to make the 443nm band an option in the band ratio
#' @param use_CI Logical value, use the color index algorithm from Hu et al (2012) and the blending boundaries used in the NASA OCx algorithm? (see details)
#' @param sensor String, either modis, seawifs, or viirs (only necessary if using CI so the wavebands can be determined, also note that "modis" is MODIS-Aqua, and "viirs" is VIIRS-SNPP)
#' @param CI_coef_version Numeric value, 1 or 2 only. Version of coefficients to use for the Hu algorithm (version 1 = 2012 paper, version 2 = 2019 paper). See ?ocx or ?hu for details.
#' @param CI_bound1 Numeric value, the lower value where blending between OCI and OCX should begin
#' @param CI_bound2 Numeric value, the higher value where blending between OCI and OCX should end
#' @references
#' Original paper:
#'
#' O'Reilly, John & Maritorena, S. & Mitchell, B.G. & Siegel, David & Carder, Kendall & Garver, S.A. & Kahru, Mati & Mcclain, Charles. (1998). Ocean color chlorophyll algorithms for SeaWiFS. Journal of Geophysical Research. 103. 937-953.
#'
#' Reference for regional OCX algorithms tuned to Atlantic and Pacific Canadian coasts:
#'
#' Clay, S.; Peña, A.; DeTracey, B.; Devred, E. Evaluation of Satellite-Based Algorithms to Retrieve Chlorophyll-a Concentration in the Canadian Atlantic and Pacific Oceans. Remote Sens. 2019, 11, 2609.
#' https://www.mdpi.com/2072-4292/11/22/2609
#'
#' References for Hu algorithm and updates:
#'
#' Hu, Chuanmin & Lee, Zhongping & Franz, Bryan. (2012). Chlorophyll a algorithms for oligotrophic oceans: A novel approach based on three-band reflectance difference. Journal of Geophysical Research. 117. C01011. 10.1029/2011JC007395.
#'
#' Hu, C., Feng, L., Lee, Z., Franz, B. A., Bailey, S. W., Werdell, P. J., & Proctor, C. W. (2019). Improving satellite global chlorophyll a data products through algorithm refinement and data recovery. Journal of Geophysical Research: Oceans, 124, 1524– 1543. https://doi.org/10.1029/2019JC014941
#'
#' Link to NASA chlorophyll-a description:
#'
#' https://oceancolor.gsfc.nasa.gov/atbd/chlor_a/
#'
#' @return For matrix rrs: Numeric value (or vector) -- chlorophyll as computed by OCX for the given Rrs, sensor, and coefficients. For RasterStack rrs: equivalent raster with OCx chlorophyll-a.
#' @export
ocx <- function(rrs, blues, green, coefs, use_443nm=FALSE, use_CI=FALSE, sensor="seawifs", CI_coef_version=2, CI_bound1=0.15, CI_bound2=0.2) {

    stopifnot(class(rrs)[1] %in% c("matrix", "RasterStack"))

    coefs <- as.numeric(coefs)
    if (length(coefs) < 5) {
        coefs <- c(coefs, rep(0, (5 - length(coefs))))
    }

    if (class(rrs)[1] == "RasterStack") {
        # reformat raster input
        rstack <- raster::subset(rrs, c(blues, green))
        rrs <- lapply(1:length(c(blues, green)), function(i) raster::getValues(rstack[[i]]))
        rrs <- do.call(cbind, rrs)
        colnames(rrs) <- c(blues, green)
    }

    # calculate band ratio and chlorophyll
    br <- log10(get_br(rrs = rrs, blues = blues, green = green, use_443nm = use_443nm)$rrs_ocx)
    chl_ocx <- 10^(coefs[1] + (coefs[2] * br) + (coefs[3] * br^2) + (coefs[4] * br^3) + (coefs[5] * br^4))

    # optionally use color index algorithm on chl < 0.2
    if (use_CI) {
        # calculate hu chlorophyll
        chl_hu <- hu(rrs=rrs, wave=get_ci_bands(sensor), coefs=get_ci_coefs(CI_coef_version))
        # blend hu and ocx
        chl_final <- oci(chl_hu, chl_ocx, CI_bound1, CI_bound2)
    } else {
        chl_final <- chl_ocx
    }

    if (class(rrs)[1] == "RasterStack") {
        chl_final <- raster::raster(crs=raster::crs(rstack[[1]]), ext=raster::extent(rstack[[1]]), resolution=raster::res(rstack[[1]]), vals=chl_final)
    }

    return(chl_final)

}


#' OCx SSE
#'
#' Calculate the sum of squared error between in situ chl and satellite OCx chl. Called from within optimize_ocx_coefs() with the optim() function to minimize the error and get the optimal set of coefficients for the set of in situ chl / band ratios.
#'
#' @param params Named numeric vector (names must be a0,a1,a2,a3,a4, or fewer if using a smaller alg_degree) - coefficients of the polynomial
#' @param insitu_chl Numeric vector of log10(in_situ_chla)
#' @param bandratio Numeric vector of log10(band_ratio), same length as insitu_chl
#' @param alg_degree Numeric value between 1 and 4 inclusive, polynomial degree of the algorithm (note that a 4th degree polynomial requires 5 coefficients, including the intercept term)
#' @param reg_method Number of the regression method used in lmodel2 (see ?lmodel2 for details, default = 3, Standard Major Axis)
#' @return Numeric value, sum of squared error between in situ chl and satellite chl generated using the user-specified polynomial degree and coefficients
#' @export
ocx_sse <- function(params, insitu_chl, bandratio, alg_degree=4, reg_method=3) {

    params <- as.list(params)
    if (alg_degree==1) {
        y <- with(params, (a0 +(a1 * bandratio)))
    } else if (alg_degree==2) {
        y <- with(params, (a0 +(a1 * bandratio) + (a2 * bandratio^2)))
    } else if (alg_degree==3) {
        y <- with(params, (a0 +(a1 * bandratio) + (a2 * bandratio^2) + (a3 * bandratio^3)))
    } else if (alg_degree==4) {
        y <- with(params, (a0 +(a1 * bandratio) + (a2 * bandratio^2) + (a3 * bandratio^3) + (a4 * bandratio^4)))
    }

    mod <- suppressMessages(lmodel2::lmodel2(y ~ insitu_chl))
    m <- mod$regression.results[reg_method,"Slope"]
    b <- mod$regression.results[reg_method,"Intercept"]
    yp <- m * insitu_chl + b

    return(sum((insitu_chl - yp)^2)) # fixes the tilt of the linear model when minimized by "optim" function

}


#' OCX coefficient optimization
#'
#' Given a dataframe with log10(in_situ_chla) values in the first column and corresponding log10(satellite band_ratio) values in the second column, and the degree of the polynomial algorithm to use, find the best set of coefficients to fit the band ratio to the chla.
#'
#' Note that this uses the full dataset for optimization (it's NOT separated into training/test/validation sets). See the example below for a method to use bootstrapping to get confidence intervals for the retrieved coefficients.
#'
#' @param data Dataframe containing 2 columns labelled x and y, where x = log10(in_situ_chla) and y = log10(band_ratio)
#' @param alg_degree Numeric value between 1 and 4 inclusive, polynomial degree of the algorithm (note that a 4th degree polynomial requires 5 coefficients, including the intercept term)
#' @param params_guessed Named numeric vector (names must be a0,a1,a2,a3,a4, or fewer if using a smaller alg_degree)
#' @param reg_method Number of the regression method used in lmodel2 (see ?lmodel2 for details, default = 3, Standard Major Axis)
#' @return Numeric vector of optimized coefficients, for the term with the smaller degree to highest
#' @examples
#' # Some in situ chl / MODIS Rrs data used in Clay et al (2019)
#' input <- matrix(c(0.118, 0.0072, 0.0064, 0.0035, 0.122, 0.0048, 0.005, 0.0017, 0.128, 0.0076, 0.007, 0.0032, 0.198, 0.0072, 0.007, 0.0035, 0.199, 0.0137, 0.0099, 0.005, 0.206, 0.0049, 0.005, 0.0027, 0.208, 0.0083, 0.0074, 0.0035, 0.213, 0.0035, 0.0036, 0.0023, 0.215, 0.0053, 0.0057, 0.0032, 0.217, 0.0031, 0.0041, 0.0026, 0.22, 0.0067, 0.0066, 0.0034, 0.223, 0.0032, 0.0035, 0.0023, 0.223, 0.0042, 0.0045, 0.0024, 0.249, 0.0185, 0.0125, 0.0062, 0.249, 0.0027, 0.0056, 0.005, 0.254, 0.0048, 0.0055, 0.0035, 0.403, 0.0052, 0.0055, 0.0026, 0.404, 0.0054, 0.0054, 0.0043, 0.404, 0.0026, 0.003, 0.0023, 0.418, 0.004, 0.0042, 0.0028, 0.438, 0.0053, 0.0054, 0.0032, 0.438, 0.0047, 0.0048, 0.0034, 0.5, 0.0045, 0.0048, 0.0038, 0.501, 0.0047, 0.0074, 0.0069, 0.508, 0.0138, 0.0114, 0.0075, 0.511, 0.0047, 0.0053, 0.0037, 0.958, 0.0023, 0.0034, 0.003, 0.971, 0.0072, 0.0054, 0.0038, 1.253, 0.0019, 0.003, 0.0028, 1.253, 0.0108, 0.0058, 0.0034, 1.259, 0.0017, 0.0026, 0.0026, 1.261, 0.0057, 0.0073, 0.0074, 1.264, 0.0031, 0.0032, 0.0027, 1.269, 0.0033, 0.0044, 0.0044, 1.273, 0.0047, 0.0045, 0.0036, 1.311, 0.0043, 0.0046, 0.0031, 1.975, 0.0066, 0.0051, 0.0038, 1.975, 0.0067, 0.0065, 0.0043, 1.994, 0.0016, 0.0026, 0.0029, 1.999, 0.0022, 0.0037, 0.0033, 2.019, 0.0024, 0.0032, 0.0035, 2.551, 0.0059, 0.0043, 0.0024, 3.01, 0.0037, 0.0044, 0.0036, 3.035, 8e-04, 0.0026, 0.0031, 3.064, 0.0043, 0.0042, 0.0034, 3.086, 0.0077, 0.0081, 0.0072, 3.148, 0.0061, 0.0045, 0.0034, 3.216, 0.0027, 0.0034, 0.0035, 3.222, 0.0059, 0.0046, 0.0035, 4.47, 0.0033, 0.0042, 0.0033, 4.558, 0.0052, 0.0053, 0.0037, 4.575, 0.0051, 0.0042, 0.004, 4.613, 0.0031, 0.0034, 0.0034, 4.653, 0.0014, 0.0023, 0.0033, 4.749, 6e-04, 0.0019, 0.0034, 6.644, 0.0046, 0.0039, 0.0037, 6.825, 0.0015, 0.0023, 0.0026, 6.832, 0.0042, 0.0047, 0.0045, 6.954, 0.0053, 0.0045, 0.0034, 7.049, 0.0036, 0.0034, 0.0039, 7.099, 3e-04, 0.0013, 0.0026, 7.162, 0.0027, 0.0027, 0.003, 7.407, 0.0025, 0.003, 0.0035, 7.462, 0.0056, 0.0052, 0.0049, 7.79, 0.0012, 0.0019, 0.0028, 7.89, 0.0013, 0.0022, 0.0028, 8.142, 0.0044, 0.0044, 0.0047, 8.162, 5e-04, 0.0014, 0.0024, 8.869, 0.0011, 0.0022, 0.0029, 9.274, 0.0018, 0.0022, 0.0026, 9.533, 0.0015, 0.0022, 0.003), ncol=4, byrow=TRUE)
#' colnames(input) <- c("in_situ_chl", "Rrs_443", "Rrs_488", "Rrs_547")
#'
#' rrs <- input[,2:4]
#' chl <- input[,1]
#'
#' lambdas <- get_ocx_lambda("modis", use_443nm = FALSE)
#' use_443nm <- FALSE
#' br <- get_br(rrs=rrs, blues=lambdas$blues, green=lambdas$green, use_443nm=use_443nm)$rrs_ocx
#' alg_degree <- 4
#' df <- data.frame(x=log10(chl), y=log10(br), stringsAsFactors = FALSE)
#'
#' # get the optimal coefficients
#' # Note that x = LOGGED in situ chla and y = LOGGED satellite Rrs band ratio.
#' # Also - this gets the best coefficients after fitting the in situ data to satellite data,
#' # but this does not bootstrap or split it into training/test sets so the coefficients
#' # derived from this function are not robust.
#' best_alg_coefs <- optimize_ocx_coefs(data = df, alg_degree = alg_degree)
#'
#' # calculate ocx chl for each data point using those coefficients
#' sat_ocx_chl <- ocx(rrs=rrs, blues=lambdas$blues, green=lambdas$green, coefs=best_alg_coefs)
#'
#' # plot in situ against satellite chl
#' library(ggplot2)
#' ggplot(data.frame(in_situ_chl=chl, sat_ocx_chl=sat_ocx_chl, stringsAsFactors=FALSE), aes(x=in_situ_chl, y=sat_ocx_chl)) +
#'     geom_point() +
#'     geom_abline(slope=1, intercept=0) +
#'     scale_x_continuous(limits=c(0.1, 15), trans="log10") +
#'     scale_y_continuous(limits=c(0.1, 15), trans="log10") +
#'     theme_bw()
#'
#'
#' # use bootstrapping to get confidence intervals for optimized coefficients
#' library(boot)
#'
#' # help understanding bootstrap results:
#' # https://www.datacamp.com/community/tutorials/bootstrap-r
#'
#' # write a function that takes the data and randomly subsets the records (rows) of the input data for each bootstrap iteration
#' boot_ocx <- function(data,inds,alg_degree) optimize_ocx_coefs(data=data[inds,], alg_degree=alg_degree)
#'
#' # bootstrap coefficients
#' boot_results <- boot(data=df, statistic=boot_ocx, R=100, alg_degree=alg_degree)
#'
#' # get coefficients from the whole dataset:
#'
#' # # use the coefficients retrieved from the entire dataset
#' # # (same as simply calling optimize_ocx_coefs(data=df, alg_degree=alg_degree))
#' # boot_coefs <- boot_results$t0
#' # use the mean value of coefficients retrieved from each bootstrap subset
#' boot_coefs <- colMeans(boot_results$t, na.rm=TRUE)
#'
#' # get the confidence intervals of the coefficients
#' boot_coefs_CI <- lapply(1:length(boot_coefs), function(i) {boot.ci(boot_results, index=i)})
#' # extract the BCA (bias-corrected, accelerated) confidence interval values
#' boot_coefs_BCA_CI1 <- sapply(1:length(boot_coefs_CI), function(i) {boot_coefs_CI[[i]]$bca[4]})
#' boot_coefs_BCA_CI2 <- sapply(1:length(boot_coefs_CI), function(i) {boot_coefs_CI[[i]]$bca[5]})
#'
#' # print results
#' coef_df <- data.frame("coefficients" = boot_coefs,
#'                       #"coefficients" = boot_results$t0,
#'                       "lower" = boot_coefs_BCA_CI1,
#'                       "upper" = boot_coefs_BCA_CI2,
#'                       stringsAsFactors = FALSE)
#' print(coef_df)
#'
#' # plot results
#' ggplot(data=coef_df, aes(x=1:nrow(coef_df), y=coefficients)) +
#'     geom_point() +
#'     geom_line() +
#'     labs(x="coefficient", y="value") +
#'     geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.1, color="red") +
#'     theme_bw()
#' @export
optimize_ocx_coefs <- function(data, alg_degree, params_guessed=c(a0 = 0.3, a1 = -3.8, a2 = -1, a3 = 1, a4 = 1), reg_method=3) {
    params_fitted <- optim(par=params_guessed,
                           fn=ocx_sse,
                           insitu_chl=data$x,
                           bandratio=data$y,
                           alg_degree=alg_degree,
                           reg_method=reg_method)
    return(as.numeric(params_fitted$par))
}
