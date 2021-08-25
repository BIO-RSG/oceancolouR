#' Get predefined coefficients for OCx
#'
#' Library of existing optimized coefficients for the polynomial band ratio algorithm.
#' "ocx" returns the coefficients of the global ocean colour algorithm used by NASA (as of Nov 2020) for the selected sensor. "poly1" to "poly4" return the regionally-optimized coefficients for polynomial algorithms of degrees 1 to 4 for the selected sensor, for the two available regions: nwa (Northwest Atlantic) or nep (Northeast Pacific).
#'
#' @param sensor String, either "modis", "seawifs", or "viirs" (note: "modis" is MODIS-Aqua, and "viirs" is VIIRS-SNPP)
#' @param region String, either "global" (for ocx), or "nwa" or "nep" (for ocx or poly1 to poly4)
#' @param alg String, either "poly1", "poly2", "poly3", "poly4", or "ocx"
#' @references
#' Clay, S.; Peña, A.; DeTracey, B.; Devred, E. Evaluation of Satellite-Based Algorithms to Retrieve Chlorophyll-a Concentration in the Canadian Atlantic and Pacific Oceans. Remote Sens. 2019, 11, 2609.
#' https://www.mdpi.com/2072-4292/11/22/2609
#' @return Numeric vector of polynomial coefficients, from the coefficient on the lowest degree term to highest degree.
#' @export
get_ocx_coefs <- function(sensor, region="global", alg="ocx") {

    stopifnot(sensor %in% c("modis", "seawifs", "viirs"),
              ((region=="global" & alg=="ocx") | (region %in% c("nwa", "nep") & alg %in% c("poly1", "poly2", "poly3", "poly4", "ocx"))))

    coefs <- list("global"=list("modis"=list("ocx"=c(0.2424,-2.7423,1.8017,0.0015,-1.228)),
                                "seawifs"=list("ocx"=c(0.3272,-2.994,2.7218,-1.2259,-0.5683)),
                                "viirs"=list("ocx"=c(0.2228,-2.4683,1.5867,-0.4275,-0.7768))),
                  "nwa"=list("modis"=list("poly1"=c(0.36695,-3.27757),
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
#' @param rrs Either: Numeric matrix where rows = records, columns = Rrs wavebands, with named columns ("Rrs_XXX", where XXX is a wavelength in nanometres), OR: RasterStack of rrs layers with stack layers following the same naming convention. Names must match c(blues, green), i.e. same names, from shortest waveband to longest.
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
#' @return For matrix rrs: Numeric value (or vector) -- chlorophyll as computed by OCX for the given Rrs, sensor, and coefficients. For RasterStack rrs: equivalent raster with OCx chlorophyll-a.
#' @examples
#' # Some in situ chl / MODIS Rrs data used in Clay et al (2019)
#' input <- matrix(c(0.118, 0.0072, 0.0064, 0.0035, 0.122, 0.0048, 0.005, 0.0017, 0.128, 0.0076, 0.007, 0.0032, 0.198, 0.0072, 0.007, 0.0035, 0.199, 0.0137, 0.0099, 0.005, 0.206, 0.0049, 0.005, 0.0027, 0.208, 0.0083, 0.0074, 0.0035, 0.213, 0.0035, 0.0036, 0.0023, 0.215, 0.0053, 0.0057, 0.0032, 0.217, 0.0031, 0.0041, 0.0026, 0.22, 0.0067, 0.0066, 0.0034, 0.223, 0.0032, 0.0035, 0.0023, 0.223, 0.0042, 0.0045, 0.0024, 0.249, 0.0185, 0.0125, 0.0062, 0.249, 0.0027, 0.0056, 0.005, 0.254, 0.0048, 0.0055, 0.0035, 0.403, 0.0052, 0.0055, 0.0026, 0.404, 0.0054, 0.0054, 0.0043, 0.404, 0.0026, 0.003, 0.0023, 0.418, 0.004, 0.0042, 0.0028, 0.438, 0.0053, 0.0054, 0.0032, 0.438, 0.0047, 0.0048, 0.0034, 0.5, 0.0045, 0.0048, 0.0038, 0.501, 0.0047, 0.0074, 0.0069, 0.508, 0.0138, 0.0114, 0.0075, 0.511, 0.0047, 0.0053, 0.0037, 0.958, 0.0023, 0.0034, 0.003, 0.971, 0.0072, 0.0054, 0.0038, 1.253, 0.0019, 0.003, 0.0028, 1.253, 0.0108, 0.0058, 0.0034, 1.259, 0.0017, 0.0026, 0.0026, 1.261, 0.0057, 0.0073, 0.0074, 1.264, 0.0031, 0.0032, 0.0027, 1.269, 0.0033, 0.0044, 0.0044, 1.273, 0.0047, 0.0045, 0.0036, 1.311, 0.0043, 0.0046, 0.0031, 1.975, 0.0066, 0.0051, 0.0038, 1.975, 0.0067, 0.0065, 0.0043, 1.994, 0.0016, 0.0026, 0.0029, 1.999, 0.0022, 0.0037, 0.0033, 2.019, 0.0024, 0.0032, 0.0035, 2.551, 0.0059, 0.0043, 0.0024, 3.01, 0.0037, 0.0044, 0.0036, 3.035, 8e-04, 0.0026, 0.0031, 3.064, 0.0043, 0.0042, 0.0034, 3.086, 0.0077, 0.0081, 0.0072, 3.148, 0.0061, 0.0045, 0.0034, 3.216, 0.0027, 0.0034, 0.0035, 3.222, 0.0059, 0.0046, 0.0035, 4.47, 0.0033, 0.0042, 0.0033, 4.558, 0.0052, 0.0053, 0.0037, 4.575, 0.0051, 0.0042, 0.004, 4.613, 0.0031, 0.0034, 0.0034, 4.653, 0.0014, 0.0023, 0.0033, 4.749, 6e-04, 0.0019, 0.0034, 6.644, 0.0046, 0.0039, 0.0037, 6.825, 0.0015, 0.0023, 0.0026, 6.832, 0.0042, 0.0047, 0.0045, 6.954, 0.0053, 0.0045, 0.0034, 7.049, 0.0036, 0.0034, 0.0039, 7.099, 3e-04, 0.0013, 0.0026, 7.162, 0.0027, 0.0027, 0.003, 7.407, 0.0025, 0.003, 0.0035, 7.462, 0.0056, 0.0052, 0.0049, 7.79, 0.0012, 0.0019, 0.0028, 7.89, 0.0013, 0.0022, 0.0028, 8.142, 0.0044, 0.0044, 0.0047, 8.162, 5e-04, 0.0014, 0.0024, 8.869, 0.0011, 0.0022, 0.0029, 9.274, 0.0018, 0.0022, 0.0026, 9.533, 0.0015, 0.0022, 0.003), ncol=4, byrow=TRUE)
#' colnames(input) <- c("in_situ_chl", "Rrs_443", "Rrs_488", "Rrs_547")
#' rrs <- input[,2:4]
#' chl <- input[,1]
#'
#' # get the default globally-tuned ocx coefficients for MODIS-Aqua
#' best_alg_coefs <- get_ocx_coefs("modis", region="global", alg="ocx")
#' # get the wavebands used in the ocx algorithm for modis, and include the 443nm band as an option
#' use_443nm <- TRUE
#' lambdas <- get_ocx_lambda("modis", use_443nm = use_443nm)
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

    stopifnot(input_class %in% c("matrix", "RasterStack"))

    chl_min <- 0.001
    chl_max <- 1000

    coefs <- as.numeric(coefs)
    if (length(coefs) < 5) {
        coefs <- c(coefs, rep(0, (5 - length(coefs))))
    }

    if (input_class == "RasterStack") {
        rast <- rrs[[1]] # for reformatting later
        rrs <- raster_to_matrix(r = rrs, rnames = c(blues, green))
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

    if (input_class == "RasterStack") {
        chl_final <- raster::raster(crs=raster::crs(rast), ext=raster::extent(rast), resolution=raster::res(rast), vals=chl_final)
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
