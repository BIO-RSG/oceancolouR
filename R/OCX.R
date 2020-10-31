#' QAA algorithm, version 6
#'
#' Compute phytoplankton absorption coefficients for each wavelength, and a single chlorophyll-a value, using QAA (the Quasi-Analytical Algorithm).
#'
#' This code was originally written for SeaWiFS using wavelengths 412, 443, 490, 555, and 670nm. If other sensors/wavelengths are used, the function will choose the wavelengths closest to the wavelengths listed above.
#'
#' Wavelengths/lambda typically used for each sensor: 412,443,469,488,531,547,555,645,667,678 (MODIS), 412,443,490,510,555,670 (SeaWiFS), 410,443,486,551,671 (VIIRS).
#'
#' Sources for default aw_all, bbw_all, and aphstar_all, respectively: Pope and Fry 1997 (https://oceancolor.gsfc.nasa.gov/docs/rsr/water_coef.txt), Smith and Baker 1981 (https://oceancolor.gsfc.nasa.gov/docs/rsr/water_coef.txt, this does not account for salinity effects on backscattering like the values in Zhang 2009), and DFO cruise records containing aph and chlorophyll-a values which were converted to aphstar by mean(aph/chl).
#' @param rrs Remote sensing reflectances above sea level, numeric vector
#' @param lambda Wavelengths corresponding to rrs, numeric vector
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





# Constants used in the algorithm.
get_lambda <- function(sensor, use_443nm) {

    if (use_443nm) {
        # Blue Rrs wavelengths used in band ratio algorithms
        all_blues <- list("modis"=c("Rrs_443","Rrs_488"),
                          "seawifs"=c("Rrs_443","Rrs_490","Rrs_510"),
                          "viirs"=c("Rrs_443","Rrs_486"))
    } else {
        # Green Rrs wavelengths used in band ratio algorithms
        all_greens <- list("modis"="Rrs_547",
                           "seawifs"="Rrs_555",
                           "viirs"="Rrs_551") # algorithm for viirs uses 550, not 551
    }

    return(list(greens=all_greens[[sensor]], blues=all_blues[[sensor]]))

}



# OCx function from NASA.
ocx <- function(coefs, log_br) {

    # coefs = vector of coefficients you want to use, in order (from terms with
    #         the smallest powers to the largest)
    # log_br = vector of logged satellite band ratios

    # If polynomial is < degree 4, pad coefs vector with 0s
    if (length(coefs) < 5) {
        coefs <- c(coefs,rep(0,(5-length(coefs))))
    }

    chl <- 10^(coefs[1]
               + (coefs[2] * log_br)
               + (coefs[3] * log_br^2)
               + (coefs[4] * log_br^3)
               + (coefs[5] * log_br^4))

    return(chl)

}

#-------------------------------------------------------------------------------

# Use this function within the "optim" function of R to find the optimal coefficients
# of the band ratio model for polynomials of different degrees (1 - 4).
br <- function(params, log_insitu_chl, log_bandratio, deg, regr) {

    # params = vector of starting guesses for coefficients of polynomials (necessary for nonlinear optimization)
    # log_insitu_chl = vector of logged in situ chlorophyll-a
    # log_bandratio = corresponding vector of the logged satellite band ratio
    # deg = degree of the polynomial to use (1 - 4)
    # regr = method used by lmodel2 for regression (default = 3, Standard Major Axis method, SMA)

    params <- as.list(params)
    if (deg==1) {
        y <- with(params, (a0 +(a1 * log_bandratio)))
    } else if (deg==2) {
        y <- with(params, (a0 +(a1 * log_bandratio) + (a2 * log_bandratio^2)))
    } else if (deg==3) {
        y <- with(params, (a0 +(a1 * log_bandratio) + (a2 * log_bandratio^2) + (a3 * log_bandratio^3)))
    } else if (deg==4) {
        y <- with(params, (a0 +(a1 * log_bandratio) + (a2 * log_bandratio^2) + (a3 * log_bandratio^3) + (a4 * log_bandratio^4)))
    }

    mod <- lmodel2(y ~ log_insitu_chl)
    m <- mod$regression.results[regr,"Slope"]
    b <- mod$regression.results[regr,"Intercept"]
    yp <- m * log_insitu_chl + b

    return(sum((log_insitu_chl - yp)^2)) # fixes the tilt of the linear model when minimized by "optim" function

}

#-------------------------------------------------------------------------------

# Use this to bootstrap the optimization of the band ratio function.
br_boot <- function(data, ind, alg, reg_method) {

    # data = dataframe of x, y (and, originally, pixel weight values)
    # ind = indices selected by the boot function for the current iteration
    #       (default = 2000 iterations of bootstrapping; ind will be a different set for each iteration)
    # alg = degree of the polynomial to use (1 - 4)
    # reg_method = method used by lmodel2 for regression (default = 3, Standard Major Axis method, SMA)

    data <- data[ind,]

    x <- data$x
    ys <- data$ys

    params_guessed <- c(a0 = 0.3, a1 = -3.8, a2 = -1, a3 = 1, a4 = 1)
    params_fitted <- optim(par=params_guessed[1:(alg+1)], fn=br,log_insitu_chl=x,
                           log_bandratio=ys,deg=alg,regr=reg_method)
    coef <- as.numeric(params_fitted$par)

    return(coef)

}

#-------------------------------------------------------------------------------

# Get band ratio to use in OCX algorithm (MODIS, SeaWiFS, or VIIRS)
get_br <- function(rrs, blues, green, use_443nm=FALSE) {

    # rrs is a matrix (rows = records, columns = rrs wavelength)
    # Columns must be named (each column is "Rrs_XXX", where XXX is a wavelength
    # in nanometres)

    # "blues" is a vector of remote sensing reflectance names at different wavelengths
    # in the "blue" range of the visible spectrum, where the names match some
    # column names of the rrs matrix (max number of "blues" = 3).
    # "green" is a single remote sensing reflectance name in the "green" range
    # Example: Rrs_412

    # use_443nm=T to make Rrs 443nm a potential "blue" wavelength in the band
    # ratio (this has caused problems in the past, so it's optional here)

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

    return(list(rrs_ocx=rrs_ocx,ind=ind,ratio_used=ratio_used))

}

#-------------------------------------------------------------------------------

# Plot color-coded band ratios for 3 possible ratios, depending on the waveband
# used for each sensor.
br_plot <- function(ys,x,alg_chl,lambda,blue,green,blue_used,blues,use_443nm) {

    all_cols <-  c("#f8766d","#00b0f6","#00bf7d")
    if (!use_443nm) {all_cols <- all_cols[2:3]}

    # length(blues) includes 443, so it's either 2 or 3
    if (length(blues)==2) {
        br_cols <- all_cols[-length(all_cols)] # remove the last color
    } else {
        br_cols <- all_cols
    }

    br_df <- data.frame(band_ratio=10^ys,insitu_chl=10^x,blue_used=blue_used,
                        poly=alg_chl,stringsAsFactors=F)
    #br_df <- br_df[alg_chl < 0.2,]

    br <- ggplot(br_df) +
        geom_point(aes(x=band_ratio,y=insitu_chl,group=blue_used,colour=blue_used),size=0.4) +
        geom_point(aes(x=band_ratio,y=poly),size=0.4) +
        #geom_rect(aes(xmin=1.8,xmax=3.9,ymin=0.06,ymax=8),size=1,color="#0066dd",alpha=0) +
        scale_color_manual(values=br_cols) +
        scale_x_continuous(limits=c(0.4,4),breaks=c(0.4,1,4),labels=c('0.4','1','4'),trans='log10') +
        scale_y_continuous(limits=c(0.05,20),breaks=c(0.1,1,10),labels=c('0.1','1','10'),trans='log10') +
        #scale_x_continuous(limits=c(min(rrs_ocx),max(rrs_ocx)),breaks=pretty_breaks(),trans='log10') +
        #scale_y_continuous(limits=c(min(hplchla),max(hplchla)),breaks=pretty_breaks(),trans='log10') +
        theme_classic() + # white background with simple black axes
        labs(x=expression(paste(italic('R'[rs]),'(',lambda[blue],') / ',italic('R'[rs]),'(',lambda[green],')'))) +
        theme(legend.position='none',
              axis.title.x=element_text(size=16,colour='black'),
              axis.text.x=element_text(size=10,colour='black'),
              axis.ticks.length=unit(0.1,'cm'),
              axis.title.y=element_blank(),
              axis.text.y=element_text(size=10,colour='black'),
              panel.border = element_rect(colour='black', fill=NA, size=0.4)) # box around plot

    return(br)

}




# Hu Color Index algorithm, used by NASA for chlor-a concentrations < 0.15mg/m^3.
oci <- function(w,rrs) {

    wblue <- w[[1]]
    wgreen <- w[[2]]
    wred <- w[[3]]

    rrs_blue <- rrs[,1]
    rrs_green <- rrs[,2]
    rrs_red <- rrs[,3]

    CI <- rrs_green - (rrs_blue + ((wgreen-wblue)/(wred-wblue)) * (rrs_red - rrs_blue))
    return(CI)

}






#-------------------------------------------------------------------------------

# EXAMPLE:



# params_fitted <- optim(par=params_guessed[1:(alg_num+1)], fn=br,
#                        log_insitu_chl=x,log_bandratio=ys,deg=alg_num,
#                        regr=reg_method)










