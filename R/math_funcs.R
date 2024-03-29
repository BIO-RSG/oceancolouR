#' Shifted Gaussian Curve
#'
#' Given a vector tv, and parameters of a Gaussian curve, calculate a shifted Gaussian.
#'
#' @param tv Numeric vector
#' @param B0 B0
#' @param beta beta
#' @param h h
#' @param sigma sigma
#' @param tmax tmax
#' @return Numeric vector containing the points along the Gaussian.
#' @export
shifted_gaussian <- function(tv, B0, beta=0, h, sigma, tmax) {
    return(B0 + beta*tv + h/(sqrt(2*pi)*sigma) * exp(-(tv - tmax)^2 / (2*sigma^2)))
}


#' Compute line parameters
#'
#' Find slope and intercept of a line based on the coordinates of two points.
#'
#' @param x1 x coordinate of point 1
#' @param y1 y coordinate of point 1
#' @param x2 x coordinate of point 2
#' @param y2 y coordinate of point 2
#' @return Named list containing slope and intercept.
#' @export
find_line <- function(x1, y1, x2, y2) {
    m <- (y2-y1)/(x2-x1)
    b <- y1 - m*x1
    return(list(slope=m, intercept=b))
}


#' Shift line
#'
#' Given an x vector of at least 2 points, a corresponding y vector of equal length, and a distance to shift the line, shift a SLANTED line "up" or "down", in the direction perpendicular to the line.
#'
#' @param x Numeric vector, length >=2.
#' @param y Numeric vector, same length as x.
#' @param dist Numeric value, how far to shift the line.
#' @param dir String, either "up" or "down".
#' @return Named list containing the new x and y vectors.
#' @export
shift_line <- function(x, y, dist=1, dir="up") {

    if (length(x) < 2 | length(y) < 2 | length(x) != length(y)) {
        stop("Error: x and y must be same length, > 1")
    }

    last <- length(x)

    # get vector representing the line
    v1 <- c(x[last]-x[1], y[last]-y[1])

    # get vector representing the normal to that line (i.e. perpendicular to it)
    # set x coordinate = 1 for reference point
    v1norm_x <- 1
    # if v1 and v1norm perpendicular, their dot product = 0
    # dot product =  <v1, v1norm> = (v1_x * v1norm_x) + (v1_y * v1norm_y)
    v1norm_y <- (-v1[1] * v1norm_x) / v1[2]
    v1norm <- c(v1norm_x, v1norm_y)
    # normalize the perpendicular vector (make unit length)
    unit_v1norm <- v1norm / sqrt(sum(v1norm * v1norm))

    # check the direction of the input vector so you know which way is "up" or "down"
    negative_quad <- FALSE
    if (xor((x[2]-x[1] < 0), (y[2]-y[1] < 0))) {
        # vectors point toward quadrants 2 or 4, operations must be flipped below
        negative_quad <- TRUE
    }

    # get new (x,y) coordinates of the ends of the line
    scaled_unit_x <- unit_v1norm[1] * dist
    scaled_unit_y <- unit_v1norm[2] * dist
    if ((dir=="up" & !negative_quad) | (dir=="down" & negative_quad)) {
        new_x <- c(x[1] - scaled_unit_x, x[last] - scaled_unit_x)
        new_y <- c(y[1] - scaled_unit_y, y[last] - scaled_unit_y)
    } else if ((dir=="down" & !negative_quad) | (dir=="up" & negative_quad)) {
        new_x <- c(x[1] + scaled_unit_x, x[last] + scaled_unit_x)
        new_y <- c(y[1] + scaled_unit_y, y[last] + scaled_unit_y)
    }

    # # test
    # plot(x=c(x[1], x[last]),
    #      y=c(y[1], y[last]),
    #      type="l", xlab="x", ylab="y")
    # lines(x=c(new_x[1], new_x[2]),
    #       y=c(new_y[1], new_y[2]),
    #       col="red")

    return(list(x=new_x, y=new_y))

}
# # test lines (pointing toward quadrants 1-4):
# dist=0.2
# x=c(-3,-2); y=c(-2,4)    # quad1
# shift_line(x,y,dist=dist,dir="up")
# shift_line(x,y,dist=dist,dir="down")
# x=c(1,-1);  y=c(-1,1)    # quad2
# shift_line(x,y,dist=dist,dir="up")
# shift_line(x,y,dist=dist,dir="down")
# x=c(4,2);   y=c(-1.5,-2) # quad3
# shift_line(x,y,dist=dist,dir="up")
# shift_line(x,y,dist=dist,dir="down")
# x=c(0,2.5); y=c(3,1)     # quad4
# shift_line(x,y,dist=dist,dir="up")
# shift_line(x,y,dist=dist,dir="down")


#' Geometric mean
#'
#' This calculates the geometric mean, AKA mean of the log transformed data converted back to given units. Use with lognormal distributions like chlorophyll-a
#'
#' @param x Numeric vector
#' @return Geometric mean value of the input data
#' @export
geoMean <- function(x, ...){
    xlog <- log(x)
    exp(mean(xlog[is.finite(xlog)]))
}

#' Geometric standard deviation
#'
#' @description
#' This calculates the geometric tandard deviation, a dimensionless multiplicative factor to use with the geometric mean.
#' When used with the geometric mean, the range is described as from the (geometric mean / geometric SD) to (geometric mean * geometric SD)
#'
#' @examples
#' x <- rlnorm(100)
#' gm <- geoMean(x)
#' gsd <- geoSD(x)
#'
#' print(paste("Geometric mean:",gm))
#' print(paste("Lower bound:", gm/gsd))
#' print(paste("Upper bound:", gm*gsd))
#'
#' @references
#' Kirkwood, T.B.L. (1979). "Geometric means and measures of dispersion". Biometrics. 35: 908-9. JSTOR 2530139.
#'
#' @param x Numeric vector
#' @return Geometric SD factor of the input data
#' @export
geoSD <- function(x, ...){
    # WRONG WAY:
    #exp(sd(log(x)))

    # Right way:
    # Can also write as:
    #exp(sqrt((length(x) - 1) / length(x)) * sd(log(x)))
    idx_z <- which(x<=0)
    if(length(idx_z)>0) {
        x <- x[-idx_z]
        message("x <= 0 removed")
    }
    x <- x[is.finite(x)]
    xlog <- log(x)
    mu_g <- exp(mean(xlog))
    sigma_g <- exp(sqrt(sum((log(x / mu_g) ^ 2)) / length(x)))
    return(sigma_g)
}


#' Matrix addition
#'
#' Add matrices together, specifying na.rm=TRUE or FALSE for each cell that has at least one finite value across corresponding cells in all matrices. Cells that have no finite values for any matrix will be set to empty_val (default NaN).
#'
#' @param ... Numeric matrices to add together (they must be the same size)
#' @param na.rm Logical value, remove NA before calculating?
#' @param empty_val Numeric value to use in cells that have no finite data across corresponding cells in all matrices
#' @return The numeric matrix that is the sum of the input matrices (same shape)
#' @examples
#' A <- matrix(c(1:11, NA), nrow=4)
#' B <- matrix(c(1:4, NA, 6:11, NA), nrow=4)
#' C <- matrix(c(NA, NA, 3:11, NA), nrow=4)
#' add_matrices(A, B, C)
#' @export
add_matrices <- function(..., na.rm=TRUE, empty_val=NaN) {
    mats <- list(...)
    vecs <- lapply(mats, as.numeric)
    mat <- do.call(cbind, vecs)
    finite_ind <- rowSums(is.finite(mat)) > 0
    mat_sum <- rep(empty_val, length(mats[[1]]))
    mat_sum[finite_ind] <- rowSums(mat[finite_ind,], na.rm=na.rm)
    return(matrix(mat_sum, ncol=ncol(mats[[1]])))
}


#' Get vector surrounding number
#'
#' Get a vector of numbers from (x-n) to (x+n).
#'
#' @param x Integer, the number at the center of the vector
#' @param n Integer, the number of places to expand the vector on either side of x
#' @return Numeric vector
#' @examples
#' plus_minus(4, 2)
#' @export
plus_minus <- function(x, n) {
    return((x-n):(x+n))
}


#' Convert an angle to its equivalent value in the range 0 to 2*pi
#'
#' Given an angle in radians, shift it to a value between 0 and 2*pi.
#'
#' This function was originally included in the code for the ULaval primary production model.
#'
#' @param x Angle (IN RADIANS)
#' @return Corresponding angle between 0 and 2*pi (IN RADIANS)
#' @examples
#' angle_rad <- -4.3*pi
#' pos_angle(angle_rad)
#'
#' # starting with an angle in degrees: convert to radians, calculate positive angle, convert back
#' angle_deg <- -365
#' pos_angle(angle_deg*pi/180) * 180/pi
#'
#' # multiple angles
#' pos_angle(c(-3, -2*pi, 4*pi, 0, 5*pi, pi/2))
#'
#' @export
pos_angle <- function(x) {
    x <- as.numeric(x)
    b <- x / (2*pi)
    a <- (2*pi) * (b - as.integer(b))
    a[a < 0] <- (2*pi) + a[a < 0]
    return (a)
}


#' Calculate filtered mean, stdev, and CV
#'
#' Given a numeric vector, calculate the filtered mean, filtered standard deviation, and coefficient of variation (filtered standard deviation over filtered mean). NA values are removed before performing the calculations.
#'
#' @param var Numeric vector or matrix
#' @return Dataframe containing the filtered mean, filtered standard deviation, and coefficient of variation
#' @examples
#' filtered_mean(rnorm(50))
#' filtered_mean(matrix(rnorm(60),nrow=12,ncol=5))
#' @references
#' Bailey, Sean & Werdell, Jeremy. (2006). A multi-sensor approach for the on-orbit validation of ocean color satellite data products. Remote Sensing of Environment. 102. 12-23. 10.1016/j.rse.2006.01.015.
#'
#' @export
filtered_mean <- function(var) {
    var <- var[is.finite(var)]
    vmean <- mean(var)
    vsd <- sd(var)
    valid_ind <- var > vmean-1.5*vsd & var < vmean+1.5*vsd
    filtered_mean <- mean(var[valid_ind])
    filtered_sd <- sd(var[valid_ind])
    coef_of_variation <- filtered_sd/filtered_mean
    numgood = length(!is.na(var[valid_ind]))
    return(data.frame(filtered_mean=filtered_mean,
                filtered_sd=filtered_sd,
                coef_of_variation=coef_of_variation,
                num_pix=numgood))
}


#' Set values outside range to upper/lower limits
#'
#' Given a numeric vector and lower and upper limits of allowed values, set the values outside the accepted range to the nearest limit.
#'
#' Other methods to get the same result:
#'
#' 1. `ifelse(v>upper,upper,ifelse(v<lower,lower,v))`
#'
#' 2. `v[v<upper] <- upper;`
#'    `v[v>lower] <- lower`
#'
#' (1) is slowest, (2) is sometimes faster than set_limits
#' (note: this has not been tested extensively)
#'
#' @param v Numeric vector
#' @param lower Lower limit, single numeric value
#' @param upper Upper limit, single numeric value
#' @param na.rm How to treat NA values
#' @return Numeric vector where values beyond the accepted range have been converted to the lower/upper limits
#' @examples
#' v <- 1:10
#' l <- 3
#' v <- set_limits(v,l)
#'
#' v <- rnorm(1e5)
#' l <- -2
#' u <- 2
#'
#' ptm <- Sys.time()
#' v <- set_limits(v,l,u)
#' print(Sys.time()-ptm)
#'
#' ptm <- Sys.time()
#' v[v<l] <- l
#' v[v>u] <- u
#' print(Sys.time()-ptm)
#'
#' @export
set_limits <- function(v, lower=-Inf, upper=Inf, na.rm=TRUE) {
    pmax(pmin(v,upper,na.rm=na.rm),lower,na.rm=na.rm)
}


#' Check if a vector is monotonically increasing/decreasing
#'
#' @param v Numeric vector
#' @param direction String, either "increasing" or "decreasing"
#' @return TRUE/FALSE
#' @examples
#' v1 <- sample(1:100,size=10)
#' v2 <- 1:10
#' monotonic_check(v1)
#' monotonic_check(v2)
#' monotonic_check(v2, direction="decreasing")
#'
#' @export
monotonic_check <- function(v, direction="increasing") {
    if (direction=="increasing") {
        return(all(v == cummax(v)))
    } else if (direction=="decreasing") {
        return(all(v == cummin(v)))
    }
}


#' Find all the factors of a positive integer
#'
#' @param x Positive integer
#' @return Vector of integers containing all the positive factors of x (from 1 to x inclusive)
#' @examples
#' find_factors(112749)
#'
#' @export
find_factors <- function(x) {
    x <- as.integer(abs(x))
    factors <- 1:x
    factors <- factors[x %% factors == 0L]
    return(factors)
}

#' Convert a latitude or longitude to decimal degrees
#'
#' @param deg degrees
#' @param min minutes
#' @param sec seconds
#' @return Input the degrees, minutes and seconds to return the decimal degrees. Works for degrees-minutes notation as well (leave seconds blank)
#' @examples
#' dms_to_deg(deg = -52, min = 48, sec = 30)
#' dms_to_deg(-52, 48.5)
#'
#' @export
dms_to_deg <- function(deg, min, sec) {
    if(missing(sec)) {sec = 0}
    dir = ifelse(deg<0, -1, 1)
    decdeg = (deg)*dir + ((min + (sec / 60))/60)
    return(decdeg*dir)
}

#' Convert a latitude or longitude from decimal degrees to degrees, minutes, seconds
#'
#' @param decdeg number in decimal degrees
#' @return Returns a data.frame() with columns indicating degrees, minutes, seconds. For degrees and decimal minutes use `deg_to_dm()`
#' @examples
#' deg_to_dms(-52.80833)
#'
#' @export
deg_to_dms <- function(decdeg) {
    dir = ifelse(decdeg<0, -1, 1)
    decdeg = decdeg*dir
    min = (decdeg - floor(decdeg))*60
    sec = (min - floor(min)) * 60
    decdeg = floor(decdeg)
    min = floor(min)
    dms = data.frame(degrees = decdeg*dir,
                     minutes = min,
                     seconds = sec)
    # print(paste(decdeg,"degrees",min,"minutes",sec,"seconds"))
    return(dms)
}

#' Convert a latitude or longitude from decimal degrees to degrees, decimal minutes
#'
#' @param decdeg number in decimal degrees
#' @return Returns a data.frame() with columns indicating degrees and decimal minutes. For degrees, minutes, seconds use `deg_to_dms()`
#' @examples
#' deg_to_dm(-52.80833)
#'
#' @export
deg_to_dm <- function(decdeg) {
    dir = ifelse(decdeg<0, -1, 1)
    decdeg = decdeg*dir
    min = (decdeg - floor(decdeg))*60
    decdeg = floor(decdeg)
    dm = data.frame(degrees = decdeg,
                    minutes = min)
    return(dm*dir)
}

