#' Shifted Gaussian Curve
#'
#' Given a vector tv, and parameters of a Gaussian curve, calculate a shifted Gaussian.
#'
#' @param tv Numeric vector
#' @param B0
#' @param beta
#' @param h
#' @param sigma
#' @param tmax
#' @return Numeric vector containing the points along the Gaussian.
#' @export
shifted_gaussian <- function(tv, B0, beta=0, h, sigma, tmax) {
    return(B0 + beta*tv + h/(sqrt(2*pi)*sigma) * exp(-(tv - tmax)^2 / (2*sigma^2)))
}


#' Compute line parameters
#'
#' Find slope and intercept of a line based on the coordinates of two points.
#'
#' @param x1
#' @param y1
#' @param x2
#' @param y2
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
