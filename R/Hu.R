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
        rrs[rrs < sw] = 10.0^(a1 * log10(rrs[rrs < sw]) - b1)

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
#' @param rrs Either: Numeric matrix where rows = records, columns = Rrs wavebands, with named columns ("Rrs_XXX", where XXX is a wavelength in nanometres, ordered from blue to green to red), OR: RasterStack of rrs layers with stack layers following the same naming convention.
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
#' @return For matrix rrs: Numeric value (or vector) -- chlorophyll as computed by Hu for the given Rrs, wavebands, and coefficients. For RasterStack rrs: equivalent raster with Hu chlorophyll-a.
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
#' # transform rrs matrix into RasterStack, and test it
#' library(raster)
#' library(magrittr)
#' rrs_stack <- stack(lapply(1:3, function(i) raster(matrix(rrs[1:9,i], nrow=3))))
#' names(rrs_stack) <- colnames(rrs)
#' chl_raster <- hu(rrs_stack, wave, coefs)
#' plot(chl_raster)
#'
#' @export
hu <- function(rrs, wave, coefs) {

    input_class <- class(rrs)[1]

    stopifnot(input_class %in% c("matrix", "RasterStack"))

    if (!all(paste0("Rrs_", wave) %in% colnames(rrs))) {
        stop("rrs column names must be in the form Rrs_XXX, where XXX is the waveband (nm) matching those in the wave variable, in the same order, from blue to green to red")
    }

    chlmin <- 0.001
    chlmax <- 1000

    wave <- sort(wave)

    if (input_class == "RasterStack") {
        rast <- rrs[[1]] # for reformatting later
        rrs <- raster_to_matrix(r = rrs, rnames = paste0("Rrs_", wave))
    } else if (input_class == "matrix") {
        rrs <- rrs[,paste0("Rrs_", wave)]
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

    chl_final <- 10^(coefs[1] + coefs[2] * ci_res)

    chl_final[chl_final < chl_min] <- chl_min
    chl_final[chl_final > chl_max] <- chl_max

    if (input_class == "RasterStack") {
        chl_final <- raster::raster(crs=raster::crs(rast), ext=raster::extent(rast), resolution=raster::res(rast), vals=chl_final)
    }

    return(chl_final)

}
