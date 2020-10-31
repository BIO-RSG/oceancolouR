#' GSM gs coefficients
#'
#' Given a numeric vector of wavelengths, calculate the corresponding g1, g2, and g3 coefficients to use in the GSM model.
#'
#' @param lambda Numeric vector of wavelengths in nanometres
#' @return Named list containing the numeric vectors g1, g2, g3 corresponding to input lambda
#' @export
get_gs <- function(lambda) {

    # Spectrally-dependent g coefficients for 400-700nm at 10nm intervals.
    g_lambda <- seq(400,700,by=10)
    gs_all <- list(g1=c(0.0741506663108934,0.0715677856118096,0.0696710302801319,0.0685287201876029,
                        0.0696508174466669,0.0772854546146616,0.0801088917456024,0.0831607019724111,
                        0.0869404404234872,0.0877889627116253,0.0875313655055949,0.0861334256256866,
                        0.0843746243091012,0.0820683879426047,0.0800267140772176,0.0780733121656009,
                        0.0763222547148317,0.0753585594018177,0.0756826630809369,0.0767541054732122,
                        0.078084966262557,0.0784328573114403,0.0783437259230638,0.0782152125331966,
                        0.0780006950155139,0.0781912606804756,0.0789255444927423,0.0797867813587184,
                        0.0794801629308588,0.0788705433193678,0.0790601284091157),
                   g2=c(0.0804812448582737,0.0820469203229378,0.0841063688845819,0.086165505980941,
                        0.0889947957043606,0.10088816688655,0.114241894530481,0.139435280475089,
                        0.209499418648362,0.262082478195844,0.281994529041355,0.25676244345273,
                        0.223288045458878,0.19673313531248,0.181062431366567,0.171677942779835,
                        0.16512773255519,0.162372957625953,0.163967506976479,0.171164949950375,
                        0.186392243529789,0.193935755286762,0.19556263935158,0.196925147566756,
                        0.19734446888493,0.200897987836546,0.222669878787164,0.251330000765241,
                        0.246450854098513,0.226979734831274,0.232272804510047),
                   g3=c(1.48390768955065,1.45204753288004,1.43531916589843,1.43002586240363,
                        1.45952758059577,1.63872754787611,1.74887145006781,1.90553035892429,
                        2.18904761776977,2.30910695833387,2.3211931604049,2.22151493678141,
                        2.10581050188318,1.9920372042663,1.90967638988007,1.84637406204401,
                        1.79678848121282,1.77220096917138,1.78158307535396,1.82113328655168,
                        1.8878820585764,1.91432622218574,1.91717818862859,1.91858953410232,
                        1.91603793331892,1.92832816707188,1.99229167234146,2.06628017243855,
                        2.05098056665546,2.00000340834201,2.01371743612098))

    # Interpolate to find values for the necessary wavelengths for each of the 3 g coefficients.
    g <- data.frame(matrix(nrow=length(lambda),ncol=3),stringsAsFactors=F)

    for (i in 1:3) {
        ag <- gs_all[[i]]
        x <- c(g_lambda,lambda)
        y <- c(ag,approx(g_lambda,ag,lambda,rule=2)$y)
        xy <- as.data.frame(t(rbind(x,y)))
        xy <- unique(xy[order(as.numeric(xy[['x']])),])
        g[,i] <- as.numeric(xy[xy[,'x'] %in% lambda,]$y)
    }

    return(list(g1=g[,1], g2=g[,2], g3=g[,3]))

}


# Algorithm for GSM model. This function is called by the "gsm" function below.
# A = c(Chl,  adg(ref_value),  bbp(ref_value))
# ref_value is usually 443nm, so these are often written adg443, bbp443
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





#' GSM algorithm
#'
#' Compute the inherent optical properties (IOPs) of the water (adg443, bbp443, chla) using the GSM (Garver-Siegel-Maritorena) semi-analytical algorithm. Adg443 = absorption of colored detrital and dissolved organic materials at 443nm, bbp443 = backscattering of particulate matter at 443nm, chla = chlorophyll-a.
#'
#' This code was originally written for SeaWiFS using wavelengths 412, 443, 490, 555, and 670nm. If other sensors/wavelengths are used, the function will choose the wavelengths closest to the wavelengths listed above.
#'
#' Wavelengths/lambda typically used for each sensor: 412,443,469,488,531,547,555,645,667,678 (MODIS), 412,443,490,510,555,670 (SeaWiFS), 410,443,486,551,671 (VIIRS).
#'
#' Sources for default aw_all, bbw_all, and aphstar_all, respectively: Pope and Fry 1997 (https://oceancolor.gsfc.nasa.gov/docs/rsr/water_coef.txt), Smith and Baker 1981 (https://oceancolor.gsfc.nasa.gov/docs/rsr/water_coef.txt, this does not account for salinity effects on backscattering like the values in Zhang 2009), and DFO cruise records containing aph and chlorophyll-a values which were converted to aphstar by mean(aph/chl).
#'
#' Options for g coefficients include "gs" (spectrally-dependent) or "gc" (constant). For gc, the model is quadratic and uses the coefficients in eq. 2 in Gordon et al 1988 (g1=0.0949, g2=0.0794). For gs, the coefficients vary spectrally, and the exponent is also allowed to vary spectrally so the model is no longer perfectly quadratic.
#'
#' Acceptable range of IOPs defined as:
#'          0 <= chla <= 64
#'          0.0001 <= adg443 <= 2
#'          0.0001 <= bbp443 <= 0.1
#'
#' @param rrs Remote sensing reflectances below sea level, numeric vector
#' @param lambda Wavelengths corresponding to rrs, numeric vector
#' @param iop3 Numeric vector of starting guesses for nls (nonlinear least squares) parameters, in this order: chl, adg443, bbp443
#' @param adg_exp Numeric value, exponent on the adg term
#' @param bbp_exp Numeric value, exponent on the bbp term
#' @param chl_exp Numeric value, exponent on the chl term
#' @param gtype String, either "gs" or "gc" to indicate the type of g coefficients to use (see details).
#' @param aw_all Named list of water absorption coefficients (names must be same as lambda)
#' @param bbw_all Named list of water backscattering coefficients (names must be same as lambda)
#' @param aphstar_all Named list of specific absorption coefficients (i.e. absorption per unit chlorophyll-a, names must be same as lambda)
#' @references
#' Maritorena, Stéphane & Siegel, David & Peterson, Alan. (2002). Optimization of a semianalytical ocean color model for global-scale application. Applied optics. 41. 2705-14. 10.1364/AO.41.002705.
#' https://www.researchgate.net/publication/11345370_Optimization_of_a_semianalytical_ocean_color_model_for_global-scale_application
#'
#' Reference for regional GSM algorithms tuned to Atlantic and Pacific Canadian coasts:
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
#' # tuned exponents for atlantic region, modis, GSM_GS (see Clay et al 2019 reference)
#' chl_exp <- 0.5
#' adg_exp <- 0.036
#' bbp_exp <- 0.75
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
gsm <- function(rrs, lambda, iop3=c(0.01, 0.03, 0.019), adg_exp, bbp_exp, chl_exp, gtype="gs",
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

    # If the Rrs for any wavelengths are NA, skip this match.
    if (any(is.na(rrs))) {return(c(NA,NA,NA,T))}

    # subset aw, bbw, and aphstar vectors to the values corresponding to the wavelengths you selected.
    aw <- as.numeric(aw_all[names(aw_all) %in% as.character(lambda)])
    bbw <- as.numeric(bbw_all[names(bbw_all) %in% as.character(lambda)])
    aphstar <- as.numeric(aphstar_all[names(aphstar_all) %in% lambda])

    adgstar <- exp( - adg_exp * (lambda - 443))
    bbpstar <- (443/lambda) ^ bbp_exp

    if (gtype=="gc") {
        # Constants in eq. 2 Gordon et al., 1988: g1=0.0949, g2=0.0794
        g1 <- rep(0.0949,length(lambda))
        g2 <- rep(0.0794,length(lambda))
        g3 <- rep(2,length(lambda))
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
