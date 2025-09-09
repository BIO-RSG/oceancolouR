#' Get predefined coefficients for OCx
#'
#' Library of existing optimized coefficients for the polynomial band ratio algorithm.
#' "ocx" returns the coefficients of the global ocean colour algorithm used by NASA (as of Nov 2020) for the selected sensor. "poly1" to "poly4" return the regionally-optimized coefficients for polynomial algorithms of degrees 1 to 4 for the selected sensor, for the two available regions: nwa (Northwest Atlantic) or nep (Northeast Pacific).
#'
#' @param sensor String, either modisaqua, seawifs, viirssnpp, landsat8, sentinel2, or olci
#' @param region String, either "global" (for ocx algorithms), or "nwa" or "nep" (for ocx or poly1 to poly4)
#' @param alg String, either "poly1", "poly2", "poly3", "poly4", "poly4v2", "ocx", "oc2", "oc3", or "oc4"
#' @references
#' Standard algorithms from NASA: https://oceancolor.gsfc.nasa.gov/atbd/chlor_a/
#'
#' Clay, S.; Peña, A.; DeTracey, B.; Devred, E. Evaluation of Satellite-Based Algorithms to Retrieve Chlorophyll-a Concentration in the Canadian Atlantic and Pacific Oceans. Remote Sens. 2019, 11, 2609.
#' https://www.mdpi.com/2072-4292/11/22/2609
#'
#' poly4v2 is the same as poly4, but was tuned with more HPLC in situ data (2002-2021), and with adjusted matchup criteria (satellite pass within 12 hours of sampling, 5x5 matrix around matching pixel, at least 13 valid pixels required in 5x5 matrix).
#'
#' @return Numeric vector of polynomial coefficients, from the coefficient on the lowest degree term to highest degree.
#' @export
get_ocx_coefs <- function(sensor, region="global", alg="ocx") {

    stopifnot(sensor %in% c("modisaqua", "seawifs", "viirssnpp", "landsat8", "sentinel2", "olci", "occci"),
              ((region=="global" & alg %in% c("ocx","oc2","oc3","oc4")) | (region %in% c("nwa", "nep", "gosl", "bof") & alg %in% c("poly1", "poly2", "poly3", "poly4", "poly4v2", "ocx","oc2","oc3","oc4","ocxspmcor"))))

    # Standard algorithms from NASA: https://oceancolor.gsfc.nasa.gov/atbd/chlor_a/
    # "ocx" refers to those that are
    nasa_coefs <- list("modisaqua" = list("oc3" = c(0.26294, -2.64669, 1.28364, 1.08209, -1.76828),
                                      "oc2" = c(0.2500,-2.4752,1.4061,-2.8233,0.5405)),
                      "seawifs" = list("oc4" = c(0.32814, -3.20725, 3.22969, -1.36769, -0.81739),
                                       "oc3" = c(0.2515,-2.3798,1.5823,-0.6372,-0.5692),
                                       "oc2" = c(0.2511,-2.0853,1.5035,-3.1747,0.3383)),
                      "viirssnpp" = list("oc3" = c(0.23548, -2.63001, 1.65498, 0.16117, -1.37247)),
                      "landsat8" = list("oc3" = c(0.2412,-2.0546,1.1776,-0.5538,-0.4570),
                                        "oc2" = c(0.1977,-1.8117,1.9743,-2.5635,-0.7218)),
                      "olci" = list("oc4" = c(0.4254,-3.21679,2.86907,-0.62628,-1.09333)))
    # Subset of the NASA coefs that are default for "ocx"
    standard_coefs <- list("modisaqua" = list("ocx" = nasa_coefs$modisaqua$oc3),
                           "seawifs" = list("ocx" = nasa_coefs$seawifs$oc4),
                           "viirssnpp" = list("ocx" = nasa_coefs$viirssnpp$oc3),
                           "landsat8" = list("ocx" = nasa_coefs$landsat8$oc3),
                           # Assuming S2 is the same as L8 for now:
                           "sentinel2" = list("ocx" = nasa_coefs$landsat8$oc3),
                           "olci" = list("ocx" = nasa_coefs$olci$oc4))
    # Coefficients parameterized for some sensors in the Northwest Atlantic Ocean
    nwa_coefs <- list("modisaqua"=list("poly1" = c(0.36695,-3.27757),
                                  "poly2" = c(0.37539,-3.12409,-0.75408),
                                  "poly3" = c(0.37657,-3.26173,-0.60435,1.1404),
                                  "poly4" = c(0.37925,-3.28487,-0.7583,1.49122,0.8002),
                                  "poly4v2" = c(0.49318,-3.86911,-0.83267,1.19094,0.9436)),
                     "seawifs"=list("poly1" = c(0.51664,-3.84589),
                                    "poly2" = c(0.51424,-3.59265,-0.95058),
                                    "poly3" = c(0.52039,-3.75269,-0.92392,1.71524),
                                    "poly4" = c(0.51824,-3.68431,-0.97401,0.84875,0.77874),
                                    "poly4v2" = c(0.53490,-4.15635,-1.02904,1.57328,1.56441)),
                     "viirssnpp"=list("poly1" = c(0.43399,-3.09652),
                                  "poly2" = c(0.41461,-2.54637,-1.47087),
                                  "poly3" = c(0.44156,-3.05795,-0.65894,1.21248),
                                  "poly4" = c(0.44786,-3.11091,-0.77987,1.425,0.90445),
                                  "poly4v2" = c(0.41083,-3.7271,-0.85655,1.29441,0.89483)),
                     "olci"=list("poly4"=c(0.45626,-3.461,-0.74022,1.10433,1.29928)),
                     "occci"=list("poly4"=c(0.59779,-3.15824,-0.80348,0.93129,0.78322)))
    # Coefficients parameterized for some sensors in the Northeast Pacific Ocean
    nep_coefs <- list("modisaqua"=list("poly1" = c(0.24947,-2.84152),
                                   "poly2" = c(0.28424,-2.66996,-1.09915),
                                   "poly3" = c(0.2805,-2.77728,-1.01747,0.92282),
                                   "poly4" = c(0.26575,-2.84142,-0.57938,0.74974,0.47743)),
                      "seawifs"=list("poly1" = c(0.41867,-3.14708),
                                     "poly2" = c(0.42171,-2.95509,-0.68104),
                                     "poly3" = c(0.42506,-2.74285,-1.48743,0.17624),
                                     "poly4" = c(0.42516,-3.14271,-0.70269,1.21802,1.59686)),
                      "viirssnpp"=list("poly1" = c(0.31886,-2.6501),
                                   "poly2" = c(0.33771,-2.56462,-0.5314),
                                   "poly3" = c(0.3303,-2.74252,-0.34545,1.35569),
                                   "poly4" = c(0.33055,-2.76455,-0.39595,1.52198,0.46509)),
                      "olci" = list("poly4" = c(0.32001,-2.6277,-0.75687,0.71839,0.80784)),
                      "occci"=list("poly4"=c(0.46743,-2.88694,-0.70127,1.49738,1.07817)))
    gosl_coefs <- list("olci"=list("poly4"=c(0.14211,-2.62659,0.38526,1.33859,-1.0363)),
                       "occci"=list("poly4"=c(0.28271,-2.84730,-0.85693,1.06846,0.15699)))
    bof_coefs <- list("occci"=list("ocxspmcor"=c(-0.17617,-2.65457,-0.75323,1.91574,1.53627,-1.41368))) # a0-a4 and spm coef
    # Combine into master list:
    coefs <- list("global" = list("modisaqua" = c(nasa_coefs$modisaqua,
                                              standard_coefs$modisaqua),
                                  "viirssnpp" = c(nasa_coefs$viirssnpp,
                                              standard_coefs$viirssnpp),
                                  "seawifs" = c(nasa_coefs$seawifs,
                                                standard_coefs$seawifs),
                                  "landsat8" = c(nasa_coefs$landsat8,
                                                 standard_coefs$landsat8),
                                  "sentinel2" = c(standard_coefs$sentinel2),
                                  "olci" = c(standard_coefs$olci)),
                  "nwa" = list("modisaqua" = c(nasa_coefs$modisaqua,
                                           standard_coefs$modisaqua,
                                           nwa_coefs$modisaqua),
                               "viirssnpp" = c(nasa_coefs$viirssnpp,
                                           standard_coefs$viirssnpp,
                                           nwa_coefs$viirssnpp),
                               "seawifs" = c(nasa_coefs$seawifs,
                                             standard_coefs$seawifs,
                                             nwa_coefs$seawifs),
                               "landsat8" = c(nasa_coefs$landsat8,
                                              standard_coefs$landsat8),
                               "sentinel2" = standard_coefs$sentinel2,
                               "olci" = c(nasa_coefs$olci,
                                          standard_coefs$olci,
                                          nwa_coefs$olci),
                               "occci" = c(nwa_coefs$occci)),
                  "nep" = list("modisaqua" = c(nasa_coefs$modisaqua,
                                           standard_coefs$modisaqua,
                                           nep_coefs$modisaqua),
                               "viirssnpp" = c(nasa_coefs$viirssnpp,
                                           standard_coefs$viirssnpp,
                                           nep_coefs$viirssnpp),
                               "seawifs" = c(nasa_coefs$seawifs,
                                             standard_coefs$seawifs,
                                             nep_coefs$seawifs),
                               "landsat8" = c(nasa_coefs$landsat8,
                                              standard_coefs$landsat8),
                               "sentinel2" = standard_coefs$sentinel2,
                               "olci" = c(nasa_coefs$olci,
                                          standard_coefs$olci,
                                          nep_coefs$olci),
                               "occci" = c(nep_coefs$occci)),
                  "gosl" = list("olci" = c(gosl_coefs$olci),
                                "occci" = c(gosl_coefs$occci)),
                  "bof" = list("occci" = c(bof_coefs$occci)))
    # Return coeffs, if available
    coef_vals = try({
        coefs[[region]][[sensor]][[alg]]
        })
    if (length(coef_vals)==0) {
        message("Not parameterized for that region / sensor / algorithm")
    } else {
        return(coef_vals)
    }
}


#' Get OCX wavebands for a sensor
#'
#' Given a sensor name, get the wavebands typically used in the OCX algorithm.
#'
#' @param sensor String, either modisaqua, seawifs, viirssnpp, landsat8, or olci
#' @param use_443nm Logical value, TRUE to make the 443nm band an option in the band ratio
#' @return Named list of 2 character vectors, one for "green" waveband(s) and one for "blue"
#' @export
get_ocx_bands <- function(sensor, use_443nm) {

    # Blue Rrs wavelengths used in band ratio algorithms
    all_blues <- list("modisaqua"=c("Rrs_443","Rrs_488"),
                      "seawifs"=c("Rrs_443","Rrs_490","Rrs_510"),
                      "viirssnpp"=c("Rrs_443","Rrs_486"),
                      "landsat8"=c("Rrs_443","Rrs_482"),
                      "olci"=c("Rrs_443","Rrs_490","Rrs_510"),
                      "occci"=c("Rrs_443","Rrs_490","Rrs_510"),
                      "globcolour"=c("Rrs_443","Rrs_490"))
    # Green Rrs wavelengths used in band ratio algorithms
    # algorithm for viirssnpp uses 550, not 551
    all_greens <- list("modisaqua"="Rrs_547",
                       "seawifs"="Rrs_555",
                       "viirssnpp"="Rrs_551",
                       "landsat8"="Rrs_561",
                       "olci"="Rrs_560",
                       "occci"="Rrs_560",
                       "globcolour"="Rrs_555")

    blues <- all_blues[[sensor]]
    green <- all_greens[[sensor]]
    if (!use_443nm) {blues <- blues[blues != "Rrs_443"]}

    return(list(green=green, blues=blues))

}


#' Get band ratios
#'
#' Given a matrix of Rrs (remote sensing reflectances) with column names, and the blue and green wavebands to use, calculate the band ratios.
#'
#' Slightly negative Rrs are allowed according to the rules used by NASA OBPG: The shortest blue band used in the model must be > -0.001, and if there are 2 possible blue bands used in the model, the one with the lowest value must be > -0.001.
#'
#' @param rrs Numeric matrix where rows = records, columns = Rrs wavebands, with named columns ("Rrs_XXX", where XXX is a wavelength in nanometres). Names must match c(blues, green), i.e. same names, from shortest waveband to longest.
#' @param blues Character vector of Rrs wavebands in the blue range (e.g. c("Rrs_443", Rrs_488")), matching column name(s) in rrs, maximum 3 options, arranged from shortest waveband to longest. Note that if use_443nm=FALSE, the 443nm waveband will be removed and another must be used in its place.
#' @param green String, Rrs waveband in the green range (e.g. "Rrs_547"), matching a column name in rrs
#' @param use_443nm Logical value, TRUE to make the 443nm band an option in the band ratio
#' @return Named list of two vectors: rrs_ocx (the band ratio values), and ratio_used (strings indicating which "blue" waveband was used for each band ratio)
#' @export
get_br <- function(rrs, blues, green, use_443nm=FALSE) {

    # some boundaries defined here:
    # https://oceancolor.sci.gsfc.nasa.gov/docs/ocssw/get__chl_8c_source.html#l00140
    # https://oceancolor.gsfc.nasa.gov/docs/ocssw/chl_8h_source.html

    blues <- sort(blues)

    rrs[!is.finite(rrs)] <- NA

    # Get green Rrs
    rrsg <- rrs[,colnames(rrs)==green]

    if (!use_443nm) {
        blues <- blues[blues != "Rrs_443"]
        if (nrow(rrs)==1) {
            rrsb <- data.frame(matrix(rrs[,colnames(rrs) %in% blues],nrow=1))
        } else {
            rrsb <- data.frame(rrs[,colnames(rrs) %in% blues])
        }
        colnames(rrsb) <- blues
        if (length(blues)==1) {
            valid_ind <- rrsg > 0 & rrsb[,1] > 0
        } else if (length(blues)==2) {
            valid_ind <- rrsg > 0 & rrsb[,2] > 0 & rrsb[,1] > -0.001
        }
        valid_ind <- valid_ind & is.finite(valid_ind)
        rrsb[!valid_ind,] <- NA
    } else {
        # Get dataframe of blue Rrs
        if (nrow(rrs)==1) {
            rrsb <- data.frame(matrix(rrs[,colnames(rrs) %in% blues],nrow=1))
        } else {
            rrsb <- data.frame(rrs[,colnames(rrs) %in% blues])
        }
        colnames(rrsb) <- blues
        if (length(blues)==2) {
            valid_ind <- rrsg > 0 & rrsb[,2] > 0 & rrsb[,1] > -0.001
        } else if (length(blues)==3) {
            valid_ind <- rrsg > 0 & rrsb[,3] > 0 & (rrsb[,2] > 0 | rrsb[,1]*rrsb[,2] > 0) & apply(rrsb[,1:2],MARGIN=1,FUN=min) > -0.001
        }
        valid_ind <- valid_ind & is.finite(valid_ind)
        rrsb[!valid_ind,] <- NA
    }

    rrsg[!valid_ind] <- NA

    # Calculate blue/green ratios
    all_ratios <- rrsb/rrsg

    # Pick the max ratio and note which column it's in (i.e. which "blue" band it uses),
    # and if one is NA, ignore it and choose the other
    good_ind <- apply(is.na(all_ratios), MARGIN=1, sum) < ncol(all_ratios)
    rrs_ocx <- ratio_used <- rep(NA, nrow(all_ratios))
    if (dim(rrsb)[2]==1) {
        rrs_ocx[good_ind] <- all_ratios[good_ind,]
        ratio_used[good_ind] <- blues
    } else {
        rrs_ocx[good_ind] <- apply(all_ratios[good_ind,], MARGIN=1, max, na.rm = TRUE)
        ratio_used[good_ind] <- blues[as.numeric(unlist(apply(all_ratios[good_ind,], MARGIN=1, which.max)))]
    }
    bad_rrs_ocx <- !is.finite(rrs_ocx) | rrs_ocx <= 0.21 | rrs_ocx >= 30
    rrs_ocx[bad_rrs_ocx] <- NA
    ratio_used[bad_rrs_ocx] <- NA

    return(list(rrs_ocx = rrs_ocx, ratio_used = ratio_used))

}


#' OCX algorithm
#'
#' Given a set of Rrs and coefficients, calculate chlorophyll using a polynomial band ratio algorithm. See ?optimize_ocx_coefs for example.
#'
#' @param rrs Either: Numeric matrix where rows = records, columns = Rrs wavebands, with named columns ("Rrs_XXX", where XXX is a wavelength in nanometres), OR: RasterStack or RasterBrick or SpatRaster of rrs layers with stack layers following the same naming convention. Names must match c(blues, green), i.e. same names, from shortest waveband to longest.
#' @param blues Character vector of Rrs wavebands in the blue range (e.g. c("Rrs_443", Rrs_488")), matching column name(s) in rrs, maximum 3 options, arranged from shortest waveband to longest. Note that if use_443nm=FALSE, the 443nm waveband will be removed and another must be used in its place.
#' @param green String, Rrs waveband in the green range (e.g. "Rrs_547"), matching a column name in rrs
#' @param coefs Numeric vector of coefficients corresponding to terms in the polynomial (lowest degree to highest)
#' @param use_443nm Logical value, TRUE to make the 443nm band an option in the band ratio
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
#' Link to NASA chlorophyll-a description:
#'
#' https://oceancolor.gsfc.nasa.gov/atbd/chlor_a/
#'
#' @return For matrix rrs: Numeric value (or vector) -- chlorophyll as computed by OCX for the given Rrs, sensor, and coefficients. For RasterStack/Brick/SpatRaster rrs: equivalent raster with OCx chlorophyll-a.
#' @examples
#' # Some in situ chl / modisaqua Rrs data used in Clay et al (2019)
#' input <- matrix(c(0.118, 0.0072, 0.0064, 0.0035, 0.122, 0.0048, 0.005, 0.0017, 0.128, 0.0076, 0.007, 0.0032, 0.198, 0.0072, 0.007, 0.0035, 0.199, 0.0137, 0.0099, 0.005, 0.206, 0.0049, 0.005, 0.0027, 0.208, 0.0083, 0.0074, 0.0035, 0.213, 0.0035, 0.0036, 0.0023, 0.215, 0.0053, 0.0057, 0.0032, 0.217, 0.0031, 0.0041, 0.0026, 0.22, 0.0067, 0.0066, 0.0034, 0.223, 0.0032, 0.0035, 0.0023, 0.223, 0.0042, 0.0045, 0.0024, 0.249, 0.0185, 0.0125, 0.0062, 0.249, 0.0027, 0.0056, 0.005, 0.254, 0.0048, 0.0055, 0.0035, 0.403, 0.0052, 0.0055, 0.0026, 0.404, 0.0054, 0.0054, 0.0043, 0.404, 0.0026, 0.003, 0.0023, 0.418, 0.004, 0.0042, 0.0028, 0.438, 0.0053, 0.0054, 0.0032, 0.438, 0.0047, 0.0048, 0.0034, 0.5, 0.0045, 0.0048, 0.0038, 0.501, 0.0047, 0.0074, 0.0069, 0.508, 0.0138, 0.0114, 0.0075, 0.511, 0.0047, 0.0053, 0.0037, 0.958, 0.0023, 0.0034, 0.003, 0.971, 0.0072, 0.0054, 0.0038, 1.253, 0.0019, 0.003, 0.0028, 1.253, 0.0108, 0.0058, 0.0034, 1.259, 0.0017, 0.0026, 0.0026, 1.261, 0.0057, 0.0073, 0.0074, 1.264, 0.0031, 0.0032, 0.0027, 1.269, 0.0033, 0.0044, 0.0044, 1.273, 0.0047, 0.0045, 0.0036, 1.311, 0.0043, 0.0046, 0.0031, 1.975, 0.0066, 0.0051, 0.0038, 1.975, 0.0067, 0.0065, 0.0043, 1.994, 0.0016, 0.0026, 0.0029, 1.999, 0.0022, 0.0037, 0.0033, 2.019, 0.0024, 0.0032, 0.0035, 2.551, 0.0059, 0.0043, 0.0024, 3.01, 0.0037, 0.0044, 0.0036, 3.035, 8e-04, 0.0026, 0.0031, 3.064, 0.0043, 0.0042, 0.0034, 3.086, 0.0077, 0.0081, 0.0072, 3.148, 0.0061, 0.0045, 0.0034, 3.216, 0.0027, 0.0034, 0.0035, 3.222, 0.0059, 0.0046, 0.0035, 4.47, 0.0033, 0.0042, 0.0033, 4.558, 0.0052, 0.0053, 0.0037, 4.575, 0.0051, 0.0042, 0.004, 4.613, 0.0031, 0.0034, 0.0034, 4.653, 0.0014, 0.0023, 0.0033, 4.749, 6e-04, 0.0019, 0.0034, 6.644, 0.0046, 0.0039, 0.0037, 6.825, 0.0015, 0.0023, 0.0026, 6.832, 0.0042, 0.0047, 0.0045, 6.954, 0.0053, 0.0045, 0.0034, 7.049, 0.0036, 0.0034, 0.0039, 7.099, 3e-04, 0.0013, 0.0026, 7.162, 0.0027, 0.0027, 0.003, 7.407, 0.0025, 0.003, 0.0035, 7.462, 0.0056, 0.0052, 0.0049, 7.79, 0.0012, 0.0019, 0.0028, 7.89, 0.0013, 0.0022, 0.0028, 8.142, 0.0044, 0.0044, 0.0047, 8.162, 5e-04, 0.0014, 0.0024, 8.869, 0.0011, 0.0022, 0.0029, 9.274, 0.0018, 0.0022, 0.0026, 9.533, 0.0015, 0.0022, 0.003), ncol=4, byrow=TRUE)
#' colnames(input) <- c("in_situ_chl", "Rrs_443", "Rrs_488", "Rrs_547")
#' rrs <- input[,2:4]
#' chl <- input[,1]
#'
#' # get the default globally-tuned ocx coefficients for MODIS-Aqua
#' best_alg_coefs <- get_ocx_coefs("modisaqua", region="global", alg="ocx")
#' # get the wavebands used in the ocx algorithm for modisaqua, and include the 443nm band as an option
#' use_443nm <- TRUE
#' lambdas <- get_ocx_bands("modisaqua", use_443nm = use_443nm)
#' # calculate the band ratio
#' br <- get_br(rrs=rrs, blues=lambdas$blues, green=lambdas$green, use_443nm=use_443nm)$rrs_ocx
#' # calculate ocx chl for each data point using those coefficients
#' sat_ocx_chl <- ocx(rrs=rrs, blues=lambdas$blues, green=lambdas$green, coefs=best_alg_coefs)
#'
#' # plot in situ against satellite chl
#' library(ggplot2)
#' ggplot(data.frame(in_situ_chl=chl, sat_ocx_chl=sat_ocx_chl, stringsAsFactors=FALSE),
#'        aes(x=in_situ_chl, y=sat_ocx_chl)) +
#'     geom_point() +
#'     geom_abline(slope=1, intercept=0) +
#'     scale_x_log10(limits=c(0.1, 15)) +
#'     scale_y_log10(limits=c(0.1, 15)) +
#'     theme_bw()
#'
#' @export
ocx <- function(rrs, blues, green, coefs, use_443nm=FALSE) {

    input_class <- class(rrs)[1]

    stopifnot(input_class %in% c("matrix", "RasterStack", "RasterBrick", "SpatRaster"))

    chl_min <- 0.001
    chl_max <- 1000

    coefs <- as.numeric(coefs)
    if (length(coefs) < 5) {
        coefs <- c(coefs, rep(0, (5 - length(coefs))))
    }

    if (input_class %in% c("RasterStack", "RasterBrick")) {
        if (input_class == "RasterBrick"){
            rrs <- raster::stack(rrs)
        }
        rast <- rrs[[1]] # for reformatting later
        rrs <- raster_to_matrix(r = rrs, rnames = c(blues, green))
    } else if (input_class=="SpatRaster") {
        rastrrs <- rrs[[1]] # for reformatting later
        rrs <- terra::as.matrix(rrs, wide=FALSE)
    } else if (input_class == "matrix") {
        if (nrow(rrs)==1) {
            rrs <- matrix(rrs[, sort(c(blues, green))], nrow=1)
            colnames(rrs) <- sort(c(blues, green))
        } else {
            rrs <- rrs[, sort(c(blues, green))]
        }
    }

    # calculate band ratio and chlorophyll
    br <- log10(get_br(rrs = rrs, blues = blues, green = green, use_443nm = use_443nm)$rrs_ocx)
    chl_final <- 10^(coefs[1] + (coefs[2] * br) + (coefs[3] * br^2) + (coefs[4] * br^3) + (coefs[5] * br^4))

    chl_final[chl_final < chl_min] <- chl_min
    chl_final[chl_final > chl_max] <- chl_max

    if (input_class %in% c("RasterStack", "RasterBrick")) {
        chl_final <- raster::raster(crs=raster::crs(rast), ext=raster::extent(rast), resolution=raster::res(rast), vals=chl_final)
    } else if (input_class=="SpatRaster") {
        extr <- ext(rastrrs)
        chl_final <- terra::rast(ncols=ncol(rastrrs), nrows=nrow(rastrrs), xmin=extr[1], xmax=extr[2], ymin=extr[3], ymax=extr[4], vals=chl_final)
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
#' lambdas <- get_ocx_bands("modisaqua", use_443nm = FALSE)
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





#' OCxSPM-Cor algorithm
#'
#' Modified version of ocx() for use in waters with high SPM, tested in the Bay of Fundy. Input is the same as ocx(), with the requirement of an extra vector (or raster) of spm values.
#'
#' @param rrs Either: Numeric matrix where rows = records, columns = Rrs wavebands, with named columns ("Rrs_XXX", where XXX is a wavelength in nanometres), OR: RasterStack or RasterBrick or SpatRaster of rrs layers with stack layers following the same naming convention. Names must match c(blues, green), i.e. same names, from shortest waveband to longest.
#' @param spm Numeric vector of spm values, the same number of records as in rrs, OR: RasterLayer or SpatRaster of spm
#' @param blues Character vector of Rrs wavebands in the blue range (e.g. c("Rrs_443", Rrs_488")), matching column name(s) in rrs, maximum 3 options, arranged from shortest waveband to longest. Note that if use_443nm=FALSE, the 443nm waveband will be removed and another must be used in its place.
#' @param green String, Rrs waveband in the green range (e.g. "Rrs_547"), matching a column name in rrs
#' @param coefs Numeric vector of coefficients corresponding to terms in the polynomial (lowest degree to highest)
#' @param use_443nm Logical value, TRUE to make the 443nm band an option in the band ratio
#' @references
#' doi:10.1007/s12237-024-01334-x
#' @return For matrix rrs: Numeric value (or vector) -- chlorophyll as computed by OCxSPM-Cor for the given Rrs and SPM, sensor, and coefficients. For RasterStack/Brick rrs: equivalent raster with OCxSPM-Cor chlorophyll-a.
#' @examples
#'
#' # example using a spatraster
#' library(terra)
#' data("example04_OCCCI.20240703.L3b.DAY.RRS.PANCAN")
#' # convert rrs to terra spatrasters
#' rrs <- lapply(example04_OCCCI.20240703.L3b.DAY.RRS.PANCAN, FUN=terra::rast) %>% unname() %>% do.call(what=c)
#' names(rrs) <- paste0("Rrs_",all_lambda$occci)
#' # calculate spm
#' spm_band <- get_spm_band(sensor="occci")
#' spm <- get_spm_nechad(rrs[[names(rrs)==spm_band]], sensor="occci")
#' # get the coefficients for ocxspmcor
#' coefs <- get_ocx_coefs("occci", region="bof", alg="ocxspmcor")
#' # get the wavebands used in the ocxspmcor algorithm for occci
#' lambdas <- get_ocx_bands("occci", use_443nm = FALSE)
#' # calculate ocxspmcor chl for each data point using those coefficients
#' sat_ocxspmcor_chl <- ocxspmcor(rrs=rrs, spm=spm, blues=lambdas$blues, green=lambdas$green, coefs=coefs)
#' plot(sat_ocxspmcor_chl)
#'
#' @export
ocxspmcor <- function(rrs, spm, blues, green, coefs, use_443nm=FALSE) {

    input_class_rrs <- class(rrs)[1]
    input_class_spm <- class(spm)[1]

    stopifnot(input_class_rrs %in% c("matrix", "RasterStack", "RasterBrick", "SpatRaster"))
    stopifnot(input_class_spm %in% c("numeric", "RasterLayer", "SpatRaster"))
    stopifnot((input_class_rrs=="matrix" & input_class_spm=="numeric") |
                  (input_class_rrs %in% c("RasterStack","RasterBrick") & input_class_spm=="RasterLayer") |
                  (input_class_rrs=="SpatRaster" & input_class_spm=="SpatRaster"))
    stopifnot(length(coefs)==6)

    input_class <- input_class_spm

    chl_min <- 0.001
    chl_max <- 1000

    coefs <- as.numeric(coefs)

    if (input_class=="RasterLayer") {
        if (input_class_rrs == "RasterBrick"){
            rrs <- raster::stack(rrs)
        }
        rastrrs <- rrs[[1]] # for reformatting later
        rrs <- raster_to_matrix(r = rrs, rnames = c(blues, green))
        spm <- raster_to_matrix(spm, rnames = names(spm))
    } else if (input_class=="SpatRaster") {
        rastrrs <- rrs[[1]] # for reformatting later
        rrs <- terra::as.matrix(rrs, wide=FALSE)
        spm <- terra::as.matrix(spm, wide=FALSE)
    } else if (input_class == "matrix") {
        if (nrow(rrs)==1) {
            rrs <- matrix(rrs[, sort(c(blues, green))], nrow=1)
            colnames(rrs) <- sort(c(blues, green))
        } else {
            rrs <- rrs[, sort(c(blues, green))]
        }
    }

    if (nrow(rrs)!=nrow(spm)) stop("rrs and spm must have the same number of records/pixels")

    # calculate band ratio and chlorophyll
    br <- log10(get_br(rrs = rrs, blues = blues, green = green, use_443nm = use_443nm)$rrs_ocx)
    spm <- log10(spm)
    spm[!is.finite(spm)] <- NA
    chl_final <- 10^(coefs[1] + (coefs[2] * br) + (coefs[3] * br^2) + (coefs[4] * br^3) + (coefs[5] * br^4) + (coefs[6]*spm))

    chl_final[chl_final < chl_min] <- chl_min
    chl_final[chl_final > chl_max] <- chl_max

    if (input_class=="RasterLayer") {
        chl_final <- raster::raster(crs=raster::crs(rastrrs), ext=raster::extent(rastrrs), resolution=raster::res(rastrrs), vals=chl_final)
    } else if (input_class=="SpatRaster") {
        extr <- ext(rastrrs)
        chl_final <- terra::rast(ncols=ncol(rastrrs), nrows=nrow(rastrrs), xmin=extr[1], xmax=extr[2], ymin=extr[3], ymax=extr[4], vals=chl_final)
    }

    return(chl_final)

}
