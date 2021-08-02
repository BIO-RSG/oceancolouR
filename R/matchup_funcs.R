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


#' Given lat/lon, find closest bin
#'
#' If you have a dataframe of longitudes and latitudes, find the closest bin numbers from NASA's 4km or 9km-resolution L3b (level-3 binned) files.
#'
#' Warning: As of 30 Jul 2021, if there is more than one closest bin, this will crash.
#'
#' Bin/lat/lon vectors used for the bin_df variable can be loaded using the command data("region_variable_resolution"), where:
#'
#'    region is either pancan, nwa, nep, or gosl
#'    variable is either bins, lats, lons, or bath
#'    resolution is either 4km or 9km
#'
#' (see examples)
#'
#' @param geo_df Dataframe with numeric columns longitude, latitude
#' @param bin_df Dataframe with numeric columns bin, longitude, latitude (created using the bin, lon, lat vectors in this package, see details)
#' @param measure Algorithm to use for distance calculation, see "measure" in ?geodist::geodist
#' @return geo_df dataframe with new columns "dist" (distance to closest bin, in metres) and "bin" (closest bin number)
#' @examples
#' data("nwa_bins_4km")
#' data("nwa_lons_4km")
#' data("nwa_lats_4km")
#'
#' geo_df <- data.frame(latitude=c(43.76299,43.6579,43.47346,51.83278,52.19771,60.32528,60.19208,52.28504,52.06484,44.6917,47.46267),
#'                      longitude=c(-62.7525,-62.6464,-62.45467,-46.45183,-45.65968,-48.6459,-48.68755,-53.53753,-54.30495,-63.6417,-59.95133),
#'                      stringsAsFactors = FALSE)
#' bin_df <- data.frame(bin=nwa_bins_4km,
#'                      latitude=nwa_lats_4km,
#'                      longitude=nwa_lons_4km,
#'                      stringsAsFactors = FALSE)
#'
#' closest_bins <- get_closest_bin(geo_df=geo_df, bin_df=bin_df)
#'
#' @importFrom magrittr "%>%"
#' @export
get_closest_bins <- function(geo_df, bin_df, measure="geodesic") {
    dplyr::bind_cols(
        geo_df,
        lapply(1:nrow(geo_df),
               function(i) {
                   geo_lon <- geo_df$longitude[i]
                   geo_lat <- geo_df$latitude[i]
                   tmp_bin_df <- bin_df %>% dplyr::filter(abs(latitude - geo_lat) < 1 & abs(longitude - geo_lon) < 1)
                   tmp_bin_df$dist <- as.numeric(geodist::geodist(x=data.frame(longitude=geo_lon, latitude=geo_lat),
                                                                  y=tmp_bin_df %>% dplyr::select(longitude, latitude),
                                                                  measure=measure))
                   tmp_bin_df[which.min(tmp_bin_df$dist),c("dist","bin")]
               }) %>%
            do.call(rbind, .)
    )
}

