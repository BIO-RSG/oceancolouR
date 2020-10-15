# From George White's primary production scripts.
# This generates a vector of bin numbers at the start of each row, given the number of rows.
# Example: For 4km resolution, 4320 rows; for 9km resolution, 2160 rows
#' @export
gen_start_bin = function(nrows=4320) {
    latbins = (seq(1:nrows)-0.5)*180/nrows - 90
    numbins = floor(2*nrows*cos(latbins*pi/180.0) + 0.5)
    return(cumsum(c(1,numbins[1:nrows-1])))
}


# generate matrix/RasterObject with bin numbers.
# note that the latitudes in the array go from +90 down to -90
# while the start_num vector uses -90,...,90
#' @export
gen_bin_grid = function(start_num, xmn=-180, xmx=180, ymn=-90, ymx=90) {
    nrow = length(start_num)
    snum = c(start_num, start_num[nrow]+3)
    ncol = 2*nrow
    bins.rl = raster::raster(ncols=ncol, nrows=nrow, xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx)
    bins = integer(ncol*nrow)
    dim(bins) = c(nrow, ncol)
    for (ilat in 1:nrow) {
        bb1 = start_num[ilat+1]
        bb0 = start_num[ilat]
        nb = bb1 - bb0
        bins[1+nrow-ilat,] = bb0 + floor(seq(0, ncol-1)*nb/ncol)
    }
    raster::values(bins.rl) = bins
    return(bins.rl)
}



# create 2d grid of variable, given dataframe with bins and variable columns
#' @export
var_to_rast <- function(df, resolution="4km") {

    # df is a dataframe with column 1 = bin numbers, column 2 = variable data

    # create global grid of bin numbers at selected resolution
    if (resolution=="4km") {
        start_num <- gen_start_bin(4320)
    } else if (resolution=="9km") {
        start_num <- gen_start_bin(2160)
    }
    binGrid <- gen_bin_grid(start_num)

    # get number of rows and columns, and create blank raster layer
    nrows <- length(start_num)
    ncols <- 2*nrows
    data.rl <- raster::raster(nrows=nrows, ncols=ncols)

    # get bins from the df
    bin.dt <- df[,1]

    # get data from the df
    variable <- colnames(df)[2]
    data.dt <- as.numeric(df[,2])

    # create blank vector of appropriate length, and populate the
    # chosen bin indices with data
    # length: take the value of the first bin number in the last row,
    # then add 3 to it since the last row has 3 values
    data.vc <- rep(NA, times=start_num[nrows]+3)
    data.vc[bin.dt] <- data.dt

    # populate the blank raster layer
    # take the values from the data vector at the indices specified in
    # the binGrid, many of which are repeated to stretch it out to a square grid
    raster::values(data.rl) <- data.vc[raster::getValues(binGrid)]
    names(data.rl) <- variable

    return(data.rl)

}


# Given a vector of data from a binned panCanadian NASA ocean colour file, plot
# it on a raster with coastlines.
#' @export
plot_rast_from_bin <- function(vec, ext=c(xmn=-140, xmx=-40, ymn=38, ymx=70), resolution="4km", limits=c(-Inf, Inf)) {
    data("wrld_simpl", package = "maptools")
    data(paste0("panCanadian_bins_", resolution))
    rast <- var_to_rast(data.frame(bin=bins, var=vec))
    return(raster::spplot(raster::crop(rast, raster::extent(ext)), zlim=limits) + latticeExtra::layer(sp::sp.polygons(wrld_simpl)))
}


