#' Extract closest points from regular 2d grid
#'
#' Given a 2d grid with corresponding x/y coordinates, and another x/y pair of vectors, find the data value closest to the second x/y vector pair.
#' This is similar to pracma::interp2(), but chooses the nearest point instead of interpolating (see below for example).
#'
#' @param grid_x Numeric vector of x-coordinates for the 2d grid (must match the number of columns in grid_values)
#' @param grid_y Numeric vector of y-coordinates for the 2d grid (must match the number of rows in grid_values)
#' @param grid_values Numeric matrix with number of columns/rows matchup grid_x and grid_y, respectively
#' @param x Numeric vector of x-coordinates where you want to extract the nearest grid_values point (must be same length as y)
#' @param y Numeric vector of y-coordinates where you want to extract the nearest grid_values point (must be same length as x)
#' @return Numeric vector of grid values, same length as x and y
#' @examples
#' # This example is from the help file of pracma::interp2() for 2d interpolation.
#' x <- linspace(-1, 1, 11)
#' y <- linspace(-1, 1, 11)
#' mgrid <- meshgrid(x, y)
#' Z <- mgrid$X^2 + mgrid$Y^2
#' xp <- yp <- linspace(-1, 1, 101)
#' zp <- interp2(x, y, Z, xp, yp, "linear")
#'
#' # Using get_match:
#' zp2 <- get_match(x, y, Z, xp, yp)
#'
#' plot(zp, zp2)
#' @export
get_match <- function(grid_x, grid_y, grid_values, x, y) {
    match_x <- lapply(x, function (i) {which.min(abs(i - grid_x))})
    match_y <- lapply(y, function (i) {which.min(abs(i - grid_y))})
    return(as.numeric(mapply(function (i,j) {grid_values[i,j]}, i=match_x, j=match_y)))
}
