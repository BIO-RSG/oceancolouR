#' GSM gs coefficients
#'
#' Given a numeric vector of wavelengths, calculate the corresponding g1, g2, and g3 coefficients to use in the GSM model.
#'
#' @param lambda Numeric vector of wavelengths in nanometres
#' @return Named list containing the numeric vectors g1, g2, g3 corresponding to input lambda
#' @export
get_gs <- function(lambda) {

    # Spectrally-dependent g coefficients for 400-700nm at 10nm intervals.
    data("spectralg_coefs")
    g_lambda <- spectralg_coefs[,1]

    # Interpolate to find values for the necessary wavelengths for each of the 3 g coefficients.
    g <- data.frame(matrix(nrow=length(lambda),ncol=3),stringsAsFactors=F)

    for (i in 1:3) {
        ag <- spectralg_coefs[,i+1]
        x <- c(g_lambda,lambda)
        y <- c(ag,approx(g_lambda,ag,lambda,rule=2)$y)
        xy <- as.data.frame(t(rbind(x,y)))
        xy <- unique(xy[order(as.numeric(xy[['x']])),])
        g[,i] <- as.numeric(xy[xy[,'x'] %in% lambda,]$y)
    }

    return(list(g1=g[,1], g2=g[,2], g3=g[,3]))

}



#' Get predefined IOP exponents for GSM in specific regions (NWA or NEP only)
#'
#' Library of existing optimized exponents for the Garver-Siegel-Maritorena algorithm, used on the IOPs chl, adg, and bbp within the absorption and backscattering terms of the algorithm.
#'
#' region="global" for the standard exponents, "nwa" (Northwest Atlantic) or "nep" (Northeast Pacific) for the regionally-tuned exponents used in Clay et al 2019.
#'
#' gtype="gc" for the exponents that were tuned using the g coefficients that are constant across wavebands, "gs" for those tuned using the spectrally-dependent g coefficients described in Clay et al 2019. "gsv2" refers to the re-optimized coefficients using the R2022.0 reprocessing.
#'
#' @param sensor String, either "modisaqua", "seawifs", "viirssnpp", or "olci"
#' @param region String, either "nwa", or "nep"
#' @param gtype String, either "gc", "gs", or "gsv2" (see description below)
#' @references
#' Clay, S.; Peña, A.; DeTracey, B.; Devred, E. Evaluation of Satellite-Based Algorithms to Retrieve Chlorophyll-a Concentration in the Canadian Atlantic and Pacific Oceans. Remote Sens. 2019, 11, 2609.
#' https://www.mdpi.com/2072-4292/11/22/2609
#' @return Numeric vector of exponents used on the IOPs in the algorithm, in this order: chl, adg, bbp
#' @export
get_gsm_IOPexps <- function(sensor, region, gtype) {

    stopifnot(sensor %in% c("modisaqua", "seawifs", "viirssnpp", "olci"),
              region %in% c("nwa", "nep"),
              gtype %in% c("gc", "gs", "gsv2"))

    exps <- list("nwa"=list("modisaqua"=list("gc"=c(0.5,0.038,0.8),
                                         "gs"=c(0.5,0.036,0.75),
                                         "gsv2"=c(0.447424475,0.029787807,0.813871834)),
                            "seawifs"=list("gc"=c(0.5,0.035,0.6),
                                           "gs"=c(0.5,0.034,0.525),
                                           "gsv2"=c(0.487986154,0.034473545,0.571016089)),
                            "viirssnpp"=list("gc"=c(0.6,0.026,1.4),
                                         "gs"=c(0.5,0.026,1.75),
                                         "gsv2"=c(0.483961381,0.023038133,1.821948237)),
                            "olci"=list("gs"=c(0.711749398,0.026033124,0.744068039))),
                 "nep"=list("modisaqua"=list("gc"=c(0.6,0.038,0.9),
                                         "gs"=c(0.6,0.036,0.75)),
                            "seawifs"=list("gc"=c(0.7,0.028,0.75),
                                           "gs"=c(0.65,0.026,0.65)),
                            "viirssnpp"=list("gc"=c(0.6,0.034,0.8),
                                         "gs"=c(0.6,0.03,0.75))))

    return(as.numeric(exps[[region]][[sensor]][[gtype]]))

}



#' Get water absorption coefficients (aw) for selected wavebands
#'
#' If lambda contains wavebands that are not integer values, the value will be interpolated. Bands must be within 400-700nm (inclusive).
#'
#' @param lambda Numeric vector of wavebands (nanometers)
#' @references
#' Sources for default aw, bbw, and aphstar: Pope and Fry 1997, and Smith and Baker 1981 (https://oceancolor.gsfc.nasa.gov/docs/rsr/water_coef.txt, this does not account for salinity effects on backscattering like the values in Zhang 2009), and DFO cruise records containing aph and chlorophyll-a values which were converted to aphstar by mean(aph/chl).
#' @return Numeric vector of aw coefficients corresponding to lambda
#' @export
get_aw <- function(lambda) {
    lambda <- sort(lambda)
    data("aw_coefs")
    # subset aw vector to the values corresponding to the wavelengths you selected, interpolating if necessary
    selected_coefs <- approx(x=aw_coefs[,1],y=aw_coefs[,2],xout=lambda)$y
    return(selected_coefs)
}

#' Get water backscattering coefficients (bbw) for selected wavebands
#'
#' If lambda contains wavebands that are not integer values, the value will be interpolated. Bands must be within 400-700nm (inclusive).
#'
#' @param lambda Numeric vector of wavebands (nanometers)
#' @references
#' Sources for default aw, bbw, and aphstar: Pope and Fry 1997, and Smith and Baker 1981 (https://oceancolor.gsfc.nasa.gov/docs/rsr/water_coef.txt, this does not account for salinity effects on backscattering like the values in Zhang 2009), and DFO cruise records containing aph and chlorophyll-a values which were converted to aphstar by mean(aph/chl).
#' @return Numeric vector of bbw coefficients corresponding to lambda
#' @export
get_bbw <- function(lambda) {
    lambda <- sort(lambda)
    data("bbw_coefs")
    # subset bbw vector to the values corresponding to the wavelengths you selected, interpolating if necessary
    selected_coefs <- approx(x=bbw_coefs[,1],y=bbw_coefs[,2],xout=lambda)$y
    return(selected_coefs)
}

#' Get phytoplankton absorption coefficients (aphstar) for selected wavebands
#'
#' If lambda contains wavebands that are not integer values, the value will be interpolated. Bands must be within 400-700nm (inclusive).
#'
#' @param lambda Numeric vector of wavebands (nanometers)
#' @references
#' Sources for default aw, bbw, and aphstar: Pope and Fry 1997, and Smith and Baker 1981 (https://oceancolor.gsfc.nasa.gov/docs/rsr/water_coef.txt, this does not account for salinity effects on backscattering like the values in Zhang 2009), and DFO cruise records containing aph and chlorophyll-a values which were converted to aphstar by mean(aph/chl).
#' @return Numeric vector of aphstar coefficients corresponding to lambda
#' @export
get_aphstar <- function(lambda) {
    lambda <- sort(lambda)
    data("aphstar_coefs")
    # subset aphstar vector to the values corresponding to the wavelengths you selected, interpolating if necessary
    selected_coefs <- approx(x=aphstar_coefs[,1],y=aphstar_coefs[,2],xout=lambda)$y
    return(selected_coefs)
}


#' Convert Rrs below sea level
#'
#' Given a set of remote-sensing reflectances (Rrs) above sea level, convert them to equivalent values just below the surface.
#'
#' @param rrs Numeric value, vector, or matrix of Rrs
#' @return Numeric value, vector, or matrix of corresponding Rrs below sea level
#' @export
rrs_above_to_below_sea_level <- function(rrs) {
    return(rrs/(0.52 + 1.7*rrs))
}


# Algorithm for GSM model. This function is called by the "gsm" function below.
# A = c(Chl,  adg(ref_value),  bbp(ref_value))
# ref_value is usually 443nm, so these are often written adg443, bbp443
#' @export
gsm_model <- function(A, g1, g2, g3, aw, bbw, chl_exp, aphstar, adgstar, bbpstar) {

    abs <- aw + (A[1] ^ chl_exp) * aphstar + A[2] * adgstar
    #abs <- aw + A[1] * (aphstar ^ chl_exp) + A[2] * adgstar
    #abs <- aw + A[1] * aphstar + A[2] * adgstar
    bb <- bbw + A[3] * bbpstar
    # Gordon et al., 1988
    x <- bb/(abs + bb)

    res <- g1*x + g2*x^g3
    fact <- (g1 + g3*g2*x^(g3 - 1))*(x^2)

    # Each column contains the partial derivative of the formula with respect to
    # the column's parameter (chl, adg, and bbp), evaluated at each wavelength.
    # This allows the algorithm to find the direction of downward or upward
    # slope at each wavelength based on the combination of parameters, in order
    # to help it move it in the direction of the real rrs (i.e. minimize the error
    # between actual and estimated rrs).
    pder <- matrix(c(-fact * aphstar * (chl_exp) * (A[1]^(chl_exp - 1))/bb,
                     -fact * adgstar/bb,
                     fact * abs * bbpstar/(bb^2)),
                   length(aw), # number of wavelengths
                   length(A)) # number of optimized parameters

    attr(res, "gradient") <- pder

    return (res)

}


#' GSM algorithm for MODIS-Aqua, SeaWiFS, or VIIRS-SNPP
#'
#' Compute the inherent optical properties (IOPs) of the water (adg443, bbp443, chla) using the GSM (Garver-Siegel-Maritorena) semi-analytical algorithm. Adg443 = absorption of colored detrital and dissolved organic materials at 443nm, bbp443 = backscattering of particulate matter at 443nm, chla = chlorophyll-a.
#'
#' This code was originally written for SeaWiFS using wavelengths 412, 443, 490, 555, and 670nm. If other sensors/wavelengths are used, the function will choose the wavelengths closest to the wavelengths listed above.
#'
#' Wavelengths/lambda typically used for each sensor: 412,443,469,488,531,547,555,645,667,678 (MODIS), 412,443,490,510,555,670 (SeaWiFS), 410,443,486,551,671 (VIIRS).
#'
#' Options for g coefficients include "gs" (spectrally-dependent) or "gc" (constant). For gc, the model is quadratic and uses the coefficients in eq. 2 in Gordon et al 1988 (g1=0.0949, g2=0.0794). For gs, the coefficients vary spectrally, and the exponent is also allowed to vary spectrally so the model is no longer perfectly quadratic.
#'
#' Acceptable range of IOPs defined as: 0 <= chla <= 64, 0.0001 <= adg443 <= 2, 0.0001 <= bbp443 <= 0.1
#'
#' @param rrs Remote sensing reflectances below sea level, numeric vector, MUST be ordered from shortest wavelength to longest
#' @param lambda Wavelengths corresponding to rrs, numeric vector, MUST be in same order as rrs
#' @param iop3 Numeric vector of starting guesses for nls (nonlinear least squares) parameters, in this order: chl, adg443, bbp443
#' @param adg_exp Numeric value, exponent on the adg term (default = globally-tuned exponent)
#' @param bbp_exp Numeric value, exponent on the bbp term (default = globally-tuned exponent)
#' @param chl_exp Numeric value, exponent on the chl term (default = globally-tuned exponent)
#' @param gtype String, either "gs" or "gc" to indicate the type of g coefficients to use (see details).
#' @param aw Numeric vector of water absorption coefficients corresponding to lambda
#' @param bbw Numeric vector of water backscattering coefficients corresponding to lambda
#' @param aphstar Numeric vector of specific absorption coefficients corresponding to lambda (i.e. absorption per unit chlorophyll-a)
#' @references
#' Maritorena, Stéphane & Siegel, David & Peterson, Alan. (2002). Optimization of a semianalytical ocean color model for global-scale application. Applied optics. 41. 2705-14. 10.1364/AO.41.002705.
#' https://www.researchgate.net/publication/11345370_Optimization_of_a_semianalytical_ocean_color_model_for_global-scale_application
#'
#' Reference for regional GSM algorithms tuned to Atlantic and Pacific Canadian coasts:
#'
#' Clay, S.; Peña, A.; DeTracey, B.; Devred, E. Evaluation of Satellite-Based Algorithms to Retrieve Chlorophyll-a Concentration in the Canadian Atlantic and Pacific Oceans. Remote Sens. 2019, 11, 2609.
#' https://www.mdpi.com/2072-4292/11/22/2609
#'
#' Code was converted from IDL to R by George White, later edited by Stephanie Clay, 2017/2018.
#'
#' @return Named vector containing IOPs (chl, adg443, bbp443, in that order), and a value named "invalid" that is either 0 or 1 (1 indicates that at least one of the IOPs is outside the acceptable range, defined in the details section).
#' @examples
#' # create a matrix of MODIS-Aqua rrs values for testing
#' # NOTE: these example values are already below sea level (conversion from above to below: rrs <- rrs/(0.52 + 1.7*rrs))
#' rrs <- matrix(c(0.001974, 0.002002, 0.002044, 0.001932, 0.002296, 0.001708, 0.002570,
#'                 0.002280, 0.002582, 0.002558, 0.002746, 0.001990, 0.003086, 0.002964,
#'                 0.002986, 0.003030, 0.003100, 0.002572, 0.002974, 0.002748, 0.002914,
#'                 0.002784, 0.002954, 0.002564, 0.002174, 0.002086, 0.002194, 0.002054,
#'                 0.002496, 0.002342, 0.001862, 0.001784, 0.001850, 0.001764, 0.002220,
#'                 0.002096, 0.001670, 0.001512, 0.001780, 0.001666, 0.001992, 0.001834,
#'                 0.000324, 0.000256, 0.000216, 0.000344, 0.000494, 0.000440, 0.000256,
#'                 0.000214, 0.000216, 0.000242, 0.000330, 0.000352, 0.000250, 0.000244,
#'                 0.000270, 0.000294, 0.000382, 0.000402), nrow=6, ncol=10)
#'
#'
#' # select wavelengths (these are the defaults for MODIS-Aqua)
#' lambda <- c(412, 443, 469, 488, 531, 547, 555, 645, 667, 678)
#'
#' # tuned exponents for atlantic region, modisaqua, GSM_GS (see Clay et al 2019 reference)
#' tuned_exps <- get_gsm_IOPexps("modisaqua", "nwa", "gs")
#' chl_exp <- tuned_exps[1]
#' adg_exp <- tuned_exps[2]
#' bbp_exp <- tuned_exps[3]
#'
#' # run gsm to process one Rrs record
#' test_gsm <- gsm(rrs=rrs[1,], lambda=lambda, adg_exp=adg_exp, bbp_exp=bbp_exp, chl_exp=chl_exp)
#' # print results
#' cat("\n\nSingle record:\n\n")
#' print(test_gsm)
#' # run gsm to process multiple records stored in an rrs matrix, where rows=records and columns=wavelengths
#' test_gsm <- t(apply(X=rrs, MARGIN=1, FUN=gsm, lambda=lambda, adg_exp=adg_exp, bbp_exp=bbp_exp, chl_exp=chl_exp))
#' # print results
#' cat("\n\n\nSet of records:\n\n")
#' print(test_gsm)
#' @export
gsm <- function(rrs, lambda, iop3=c(0.01, 0.03, 0.019),
                adg_exp=0.02061, bbp_exp=1.03373, chl_exp=1, gtype="gs",
                aw=get_aw(lambda), bbw=get_bbw(lambda), aphstar=get_aphstar(lambda)) {

    # If the Rrs for any wavelengths are NA, skip this match.
    if (any(is.na(rrs))) {return(c(NA,NA,NA,T))}

    adgstar <- exp( - adg_exp * (lambda - 443))
    bbpstar <- (443/lambda) ^ bbp_exp

    if (gtype=="gc") {
        # Constants in eq. 2 Gordon et al., 1988
        g1 <- 0.0949
        g2 <- 0.0794
        g3 <- 2
    } else if (gtype=="gs") {
        # get the g coefficients based on lambda
        gs <- get_gs(lambda)
        g1 <- gs$g1
        g2 <- gs$g2
        g3 <- gs$g3
    }

    weights <- rep(1.0, length(rrs)) # this can be changed later if necessary
    model <- rrs ~ gsm_model(iop3, g1, g2, g3, aw, bbw, chl_exp, aphstar, adgstar, bbpstar)
    data.list <- list(rrs=rrs,
                      g1=g1, g2=g2, g3=g3,
                      aw=aw, bbw=bbw,
                      chl_exp=chl_exp,
                      aphstar=aphstar,
                      adgstar=adgstar,
                      bbpstar=bbpstar)

    # Fit the rrs values to the model, given the data in data.list and starting estimates given by iop3.
    # Tips for error catching with nls: https://stackoverflow.com/questions/2963729/r-catching-errors-in-nls
    gsm.nls <- NULL
    while (is.null(gsm.nls)) {
        try({gsm.nls <- nls(model,data=data.list,trace=F,start=list(iop3=iop3),
                            weights=weights,algorithm='default',na.action=na.omit,
                            control=nls.control(tol=1e-05),max_iters=30)},silent=T)
        if (sum(iop3)==0) {break}
        iop3 <- mapply(function(x,y) {sample(seq(x,y,by=0.00001),1)}, x=(iop3 - 0.001), y=iop3)
        iop3[iop3 < 0] <- 0 # IOPs must be positive
    }

    # Retrieve the results from the fitted model, if it exists
    if (is.null(gsm.nls)) {
        iop3 <- rep(NA, 3)
        invalid <- TRUE
    } else {
        # Get the coefficients from the fit
        iop3 <- as.numeric(coef(gsm.nls))
        # # Maritorena et al (2002) correction factor for bias - NO LONGER IN USE (used in 2019 paper).
        # iop3[2] <- iop3[2] * 0.754188
        # If they're outside the generally accepted range, set invalid = TRUE
        invalid <- FALSE
        if (iop3[1] < 0.01 | iop3[1] > 64.0 |
            iop3[2] < 0.0001 | iop3[2] > 2.0 |
            iop3[3] < 0.0001 | iop3[3] > 0.1) {
            invalid <- TRUE
        }
    }

    results <- c(iop3,invalid)
    names(results) <- c("chl", "adg443", "bbp443", "invalid")

    return(results)

}
