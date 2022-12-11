#' QAA algorithm, version 6
#'
#' Compute phytoplankton absorption coefficients for each wavelength, and a single chlorophyll-a value, using QAA (the Quasi-Analytical Algorithm).
#'
#' This code was originally written for SeaWiFS using wavelengths 412, 443, 490, 555, and 670nm. If other sensors/wavelengths are used, the function will choose the wavelengths closest to the wavelengths listed above.
#'
#' Wavelengths/lambda typically used for each sensor: 412,443,469,488,531,547,555,645,667,678 (MODIS), 412,443,490,510,555,670 (SeaWiFS), 410,443,486,551,671 (VIIRS).
#'
#' Sources for default aw_all, bbw_all, and aphstar_all, respectively: Pope and Fry 1997 (https://oceancolor.gsfc.nasa.gov/docs/rsr/water_coef.txt), Smith and Baker 1981 (https://oceancolor.gsfc.nasa.gov/docs/rsr/water_coef.txt, this does not account for salinity effects on backscattering like the values in Zhang 2009), and DFO cruise records containing aph and chlorophyll-a values which were converted to aphstar by mean(aph/chl).
#' @param rrs Remote sensing reflectances above sea level, numeric matrix with column names matching the wavebands in lambda with "Rrs_" prefixes (e.g. if lambda=c(443,490) then rrs column names are c("Rrs_443","Rrs_490"))
#' @param lambda Wavelengths corresponding to rrs, numeric vector
#' @param c1 First coefficient used in the model, default are those used for SeaWiFS
#' @param c2 Second coefficient
#' @param c3 Third coefficient
#' @param aw_all Named list of water absorption coefficients (names must be same as lambda)
#' @param bbw_all Named list of water backscattering coefficients (names must be same as lambda)
#' @param aphstar_all Named list of specific absorption coefficients (i.e. absorption per unit chlorophyll-a, names must be same as lambda)
#' @references
#' Original paper:
#'
#' Lee, Zhongping & Carder, Kendall & Arnone, Robert. (2002).
#' Deriving Inherent Optical Properties from Water Color:
#' a Multiband Quasi-Analytical Algorithm for Optically Deep Waters.
#' Applied optics. 41. 5755-72. 10.1364/AO.41.005755.
#' https://www.researchgate.net/publication/11140186_Deriving_Inherent_Optical_Properties_from_Water_Color_a_Multiband_Quasi-Analytical_Algorithm_for_Optically_Deep_Waters
#'
#' Version 6 updates:
#'
#' http://www.ioccg.org/groups/Software_OCA/QAA_v6_2014209.pdf
#' @return Named list containing wavelengths used in the fit (numeric vector), phytoplankton absorption coefficients (numeric vector, one for each wavelength), and chlorophyll-a (single numeric value, mg m^-3, computed as the median of aph/aphstar).
#' @examples
#' # create a matrix of MODIS-Aqua rrs values for testing
#' rrs <- matrix(c(0.001974, 0.002002, 0.002044, 0.001932, 0.002296, 0.001708, 0.002570,
#'                 0.002280, 0.002582, 0.002558, 0.002746, 0.001990, 0.003086, 0.002964,
#'                 0.002986, 0.003030, 0.003100, 0.002572, 0.002974, 0.002748, 0.002914,
#'                 0.002784, 0.002954, 0.002564, 0.002174, 0.002086, 0.002194, 0.002054,
#'                 0.002496, 0.002342, 0.001862, 0.001784, 0.001850, 0.001764, 0.002220,
#'                 0.002096, 0.001670, 0.001512, 0.001780, 0.001666, 0.001992, 0.001834,
#'                 0.000324, 0.000256, 0.000216, 0.000344, 0.000494, 0.000440, 0.000256,
#'                 0.000214, 0.000216, 0.000242, 0.000330, 0.000352, 0.000250, 0.000244,
#'                 0.000270, 0.000294, 0.000382, 0.000402), nrow=6, ncol=10)
#' # select wavelengths (these are the defaults for MODIS-Aqua)
#' lambda <- c(412, 443, 469, 488, 531, 547, 555, 645, 667, 678)
#' # run qaa to process one Rrs record
#' test_qaa <- qaa(rrs=rrs[1,], lambda=lambda)
#' # print results
#' cat("\n\nSingle record:\n\n")
#' print(test_qaa)
#' # run qaa to process multiple records stored in an rrs matrix, where rows=records and columns=wavelengths
#' test_qaa <- apply(X=rrs, MARGIN=1, FUN=qaa, lambda=lambda)
#' # convert to matrix/vector format for easier printing
#' aph <- matrix(as.numeric(sapply(1:length(test_qaa), function(i) {test_qaa[[i]]$aph})),nrow=length(test_qaa),byrow=TRUE)
#' chl <- as.numeric(sapply(1:length(test_qaa), function(i) {test_qaa[[i]]$chl}))
#' # print results
#' cat("\n\n\nSet of records:\n\n")
#' cat("wavelengths:\n")
#' print(test_qaa[[1]]$wvs_used)
#' cat("\naph:\n")
#' print(aph)
#' cat("\nchl:\n")
#' print(chl)
#' @export
qaa <- function(rrs, lambda, c1=-1.146, c2=-1.366, c3=-0.469,
                aw_all = list('410'=0.00473000,'412'=0.00455056,'443'=0.00706914,
                              '469'=0.01043260,'486'=0.01392170,'488'=0.01451670,
                              '490'=0.01500000,'510'=0.03250000,'531'=0.04391530,
                              '547'=0.05316860,'551'=0.05779250,'555'=0.05960000,
                              '645'=0.32500000,'667'=0.43488800,'670'=0.43900000,
                              '671'=0.44283100,'678'=0.46232300),
                bbw_all = list('410'=0.003395150,'412'=0.003325000,'443'=0.002436175,
                               '469'=0.001908315,'486'=0.001638700,'488'=0.001610175,
                               '490'=0.001582255,'510'=0.001333585,'531'=0.001122495,
                               '547'=0.000988925,'551'=0.000958665,'555'=0.000929535,
                               '645'=0.000490150,'667'=0.000425025,'670'=0.000416998,
                               '671'=0.000414364,'678'=0.000396492),
                aphstar_all = list('410'=0.054343207,'412'=0.055765253,'443'=0.063251586,
                                   '469'=0.051276462,'486'=0.041649554,'488'=0.040647623,
                                   '490'=0.039546143,'510'=0.025104817,'531'=0.015745358,
                                   '547'=0.011477324,'551'=0.010425453,'555'=0.009381989,
                                   '645'=0.008966522,'667'=0.019877564,'670'=0.022861409,
                                   '671'=0.023645549,'678'=0.024389358)) {

    # SeaWiFS wavelengths used in the original code (nm)
    original_wvs <- c(412,443,490,555,670)
    # From lambda, extract wavelengths closest to the original wavelengths
    lambda <- lambda[sapply(original_wvs, function(x) which.min(abs(x-lambda)))]

    # subset aw, bbw, and aphstar vectors to the values corresponding to the
    # wavelengths you selected.
    aw <- as.numeric(aw_all[names(aw_all) %in% as.character(lambda)])
    bbw <- as.numeric(bbw_all[names(bbw_all) %in% as.character(lambda)])
    aphstar <- as.numeric(aphstar_all[names(aphstar_all) %in% lambda])

    # More coefficients for the model
    g0 <- 0.089 # 0.0949
    g1 <- 0.1245 # 0.0794
    S <- 0.015 # 0.018

    # Subset Rrs and other variables based on selected wavelengths.
    rrs <- rrs[,paste0("Rrs_",lambda)]
    # Only use records where rrs for all wavelengths is finite
    if (sum(!is.finite(rrs)) > 0) {
        cat("Error: missing Rrs values\n")
        return(list(wvs_used=NA,aph=NA,chl=NA))
    }

    # Convert rrs to below sea level.
    rrs <- rrs / (0.52 + 1.7*rrs)


    # QAA STEPS
    #*******************************************************************

    # STEP 1
    u <- (sqrt(g0*g0 + 4 * g1 * rrs) - g0)/(2*g1)

    # STEP 2
    # If the 5th wavelength is >= 0.0015, use 670nm; if not, use 555nm and
    # change calculations slightly because of high absorption coefficients
    if (rrs[5] >= 0.0015) {

        wvlref <- lambda[5]
        aref <- aw[5] + 0.39*((rrs[5]/(rrs[3] + rrs[2]))^1.14)

        # STEP 3
        bbpref <- u[5] * aref / (1 - u[5]) - bbw[5]

    } else {

        wvlref <- lambda[4]
        numer <- rrs[2] + rrs[3]
        denom <- rrs[4] + 5 * rrs[5]*rrs[5]/rrs[3]
        aux <- log10(numer/denom)
        rho <- c1 + c2*aux + c3 * aux^2
        aref <- aw[4] + 10^rho

        # STEP 3
        bbpref <- u[4] * aref / (1 - u[4]) - bbw[4]

    }

    # STEP 4
    rat <- rrs[2]/rrs[4]
    Y <- 2.0*(1 - 1.2 * exp(-0.9 * rat))

    # STEP 5
    temp_lambda_ref <- rep(wvlref,length(lambda))
    bb <- bbpref * (temp_lambda_ref/lambda)^Y + bbw

    # STEP 6
    a <- ( (1 - u) * bb) / u

    # Decomposition of absorption into aph, adg

    # STEP 7
    rat <- rrs[2]/rrs[4]
    symbol <- 0.74 + 0.2 / (0.8 + rat)

    # STEP 8
    Sr <- S + 0.002 / (0.6 + rat)
    zeta <- exp( Sr * (442.5 - 415.5) )

    # STEP 9
    denom <- zeta - symbol
    dif1 <- a[1] - symbol * a[2]
    dif2 <- aw[1] - symbol * aw[2]
    ag440 <- (dif1 - dif2) / denom
    adg <- ag440 * exp( Sr * (lambda[2] - lambda))
    aph <- a - adg - aw


    # CHECK APH (make sure it's within acceptable range)
    #*******************************************************************

    # proportion of absorption by chla at 443nm
    x1 <- aph[2] / a[2]

    # aph proportion of total absorption should be between 0.15 and 0.6
    if ((x1 < 0.15 | x1 > 0.6) & is.finite(x1)) {
        x1 <- -0.8 + 1.4 * (a[2] - aw[2])/(a[1] - aw[1])
    }
    # if it's still outside the range, set it to the boundary
    if (x1 < 0.15) {x1 <- 0.15
    } else if (x1 > 0.6) {x1 <- 0.6}

    # then work backwards to correct the aph of this wavelength (443nm)
    aph[2] <- a[2] * x1

    # get the corrected reference value
    ag440 <- a[2] - aph[2] - aw[2]

    # use it to correct the rest of adg
    adg <- ag440 * exp( Sr * (lambda[2] - lambda))

    # use that to correct the rest of aph
    aph <- a - adg - aw


    # COMPUTE CHL BASED ON APH, EXCLUDING NA
    #*******************************************************************

    # Choose which of the original wavelengths to use when calculating chl
    aph_wvs <- c(1:5)
    # Calculate chl using the median of aph/aphstar for these wavelengths
    chl <- median(aph[aph_wvs]/aphstar[aph_wvs], na.rm=TRUE)

    return(list(wvs_used = lambda,
                aph = aph,
                chl = chl))

}

