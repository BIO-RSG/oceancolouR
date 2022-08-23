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


#' Given lat/lon, find closest bin(s)
#'
#' If you have a dataframe of longitudes and latitudes, find the closest bin numbers from NASA's 4km or 9km-resolution L3b (level-3 binned) files.
#'
#' Bin/lat/lon data can be retrieved using the get_bins() function (see examples).
#'
#' @param geo_df Dataframe with numeric columns longitude, latitude
#' @param bin_df Dataframe with numeric columns bin, longitude, latitude (created using the bin, lon, lat vectors in this package, see details)
#' @param measure Algorithm to use for distance calculation, see "measure" in ?geodist::geodist
#' @param max_bins Maximum number of closest bins to return (<= 100)
#' @param radius Maximum distance between lat/lon pair and bin lat/lon (in metres)
#' @return geo_df dataframe with new columns "dist" (distance to closest bin(s), in metres), "bin" (closest bin number(s)), and bin_latitude/longitude
#' @examples
#' geo_df <- data.frame(latitude=c(43.76299,43.6579,43.47346,51.83278,52.19771,60.32528,60.19208,52.28504,52.06484,44.6917,47.46267),
#'                      longitude=c(-62.7525,-62.6464,-62.45467,-46.45183,-45.65968,-48.6459,-48.68755,-53.53753,-54.30495,-63.6417,-59.95133),
#'                      stringsAsFactors = FALSE)
#' # note that this example is using a bin grid restricted to the Northwest Atlantic (NWA),
#' # so if any points in geo_df are near the edge of the grid, they might have fewer matching bins
#' bin_df <- get_bins(region="nwa", resolution="4km", variables=c("bin","latitude","longitude"))
#'
#' # get closest bins within 10km, limited to the 50 closest bins
#' closest_bins <- get_closest_bins(geo_df=geo_df, bin_df=bin_df, max_bins=50, radius=10000)
#' head(closest_bins)
#'
#' @importFrom magrittr "%>%"
#' @export
get_closest_bins <- function(geo_df, bin_df, measure="geodesic", max_bins=100, radius=Inf) {
    if (max_bins > 100) {
        max_bins <- 100
        print("Warning: max_bins has been set to 100")
    }
    geo_df$id <- 1:nrow(geo_df)
    matched_bins <- lapply(1:nrow(geo_df),
                           function(i) {
                               geo_lon <- geo_df$longitude[i]
                               geo_lat <- geo_df$latitude[i]
                               tmp_bin_df <- bin_df %>% dplyr::filter(abs(latitude - geo_lat) < 1 & abs(longitude - geo_lon) < 1)
                               tmp_bin_df$dist <- as.numeric(geodist::geodist(x=data.frame(longitude=geo_lon, latitude=geo_lat),
                                                                              y=tmp_bin_df %>% dplyr::select(longitude, latitude),
                                                                              measure=measure))
                               tmp_bin_df <- tmp_bin_df %>%
                                   dplyr::arrange(dist) %>%
                                   dplyr::slice_head(n=max_bins) %>%
                                   dplyr::filter(dist<=radius) %>%
                                   dplyr::rename(bin_latitude=latitude,
                                                 bin_longitude=longitude) %>%
                                   dplyr::mutate(id=geo_df$id[i])
                               return(tmp_bin_df)
                           }) %>%
        do.call(rbind, .)
    matched_bins[,c("latitude","longitude")] <- geo_df[matched_bins$id,c("latitude","longitude")]
    return(matched_bins[,c(6,7,1:4)])
}

#' Extract a matchup box from a raster
#'
#' Give a row and column number, then get all pixels surrounding it in a box (and planning to add ability to do a radius)
#' @param r Raster layer
#' @param boxsize Size of box, must be an odd number in row, column order (i.e., 3 OR c(3,3) for a box 3 rows by 3 columns, c(3, 5) for box of 3 rows by 5 columns). '1' indicates just getting the matchup pixel
#' @param rowcol A data frame with row and col of point. See example below
#' @return Numeric vector of cell values and IDs
#' @examples
#' Get row and col of your point
#'
#' library(raster)
#' pt_x = -127.5
#' pt_y = 49.5
#' r = yourraster
#' xy = extract(r, y = cbind(pt_x, pt_y), cellnumbers=T, df=T)
#' xy <- as.data.frame(rowColFromCell(s, cell = xy$cells))
#' box_fun(r, 3, xy)
#' Get a 3x5 box (3 rows by 5 columns) from the same location
#' box_run(r, c(3,5, xy))
#'
#' @export
box_fun <- function(r, boxsize, rowcol) {
    require(raster)
    if (length(boxsize) == 1) {
        boxsize = c(boxsize, boxsize)
    }
    if ((boxsize[1] %% 2 == 1) && (boxsize[2] %% 2 == 1)) {
        rowcol$rmax <- rowcol[,1] + ((boxsize[1]-1)/2)
        rowcol$rmin <- rowcol[,1] - ((boxsize[1]-1)/2)
        rowcol$cmax <- rowcol[,2] + ((boxsize[2]-1)/2)
        rowcol$cmin <- rowcol[,2] - ((boxsize[2]-1)/2)
        boxvals <- r[rowcol$rmin:rowcol$rmax, rowcol$cmin:rowcol$cmax]
        } else {
            message("Please enter odd-numbered box dimensions")
            break
        }
}
