#' Get number of rows on L3b global grid for a given resolution
#'
#' This generates the number of rows on the global grid of level-3 binned (L3b) data for a given spatial resolution. For example: For 4km resolution data, 4320 rows; for 9km resolution, 2160 rows
#'
#' - HH: 50m (currently unavailable)
#' - HQ: 100m (currently unavailable)
#' - Q: 250m
#' - H: 0.5km
#' - 1: 1.1km
#' - 2: 2.3km
#' - 4: 4.6km
#' - 9: 9.2km
#' - 18: 18.5km
#' - 36: 36km
#' - QD: 0.25 degree (currently unavailable)
#' - HD: 0.5 degree
#' - 1D: 1 degree
#'
#' @param resolution String representing the spatial resolution (see details for list of accepted strings, e.g. "Q" for 250m or "4" for 4.6km)
#' @return Single numeric value, the number of rows
#' @references
#' https://oceancolor.gsfc.nasa.gov/docs/format/l3bins/
#' https://oceancolor.gsfc.nasa.gov/docs/ocssw/
#'
#' @export
gen_nrows <- function(resolution) {
    nrows_all_res <- list(#"HH" = 50m
                          #"HQ" = 100m
                          "Q" = 69120,
                          "H" = 34560,
                          "1" = 17280,
                          "2" = 8640,
                          "4" = 4320,
                          "9" = 2160,
                          "18" = 1080,
                          "36" = 540,
                          #"QD" = 0.25 degree
                          "HD" = 360,
                          "1D" = 180)
    resolution <- as.character(resolution)
    stopifnot(resolution %in% names(nrows_all_res))
    return(nrows_all_res[[resolution]])
}


#' Given L3b data resolution code, get distance
#'
#' For a given spatial resolution code (see ?gen_nrows), get equivalent distance in km and degrees at the equator (i.e. assuming 1 degree = 111km).
#'
#' @param resolution String representing the spatial resolution (see ?gen_nrows for list of accepted strings, e.g. "Q" for 250m or "4" for 4.6km)
#' @return List with the distance in degrees and km for the selected resolution code
#' @references
#' https://oceancolor.gsfc.nasa.gov/docs/format/l3bins/
#'
#' @export
res_code_to_dist <- function(resolution) {
    dists <- list(#"HH" = list(deg=0.0004504505, km=0.05)
                #"HQ" = list(deg=0.0009009009, km=0.1)
                "Q" = list(deg=0.002252252, km=0.25),
                "H" = list(deg=0.004504505, km=0.5),
                "1" = list(deg=0.00990991, km=1.1),
                "2" = list(deg=0.02072072, km=2.3),
                "4" = list(deg=0.0418018, km=4.64),
                "9" = list(deg=0.08288288, km=9.2),
                "18" = list(deg=0.1666667, km=18.5),
                "36" = list(deg=0.3243243, km=36),
                #"QD" = list(deg=0.25, km=27.75)
                "HD" = list(deg=0.5, km=55.5),
                "1D" = list(deg=1, km=111))
    resolution <- as.character(resolution)
    stopifnot(resolution %in% names(dists))
    return(dists[[resolution]])
}


# From George White's primary production scripts.
#' Generate start bin vector
#'
#' This generates a vector of bin numbers at the start of each row on a global grid, given the number of rows.
#' Example: For 4km resolution, 4320 rows; for 9km resolution, 2160 rows
#'
#' @param nrows Number of rows in the resulting global raster of bin numbers.
#' @return Numeric vector of starting bins.
#' @export
gen_start_bin = function(nrows=4320) {
    latbins = (seq(1:nrows)-0.5)*180/nrows - 90
    numbins = floor(2*nrows*cos(latbins*pi/180.0) + 0.5)
    return(cumsum(c(1,numbins[1:nrows-1]))) # note this is the same as cumsum(c(1,numbins[1:(nrows-1)]))
}


# From George White's primary production scripts.
#' Generate bin grid for viewing
#'
#' This generates a matrix or raster of bin numbers for the selected extent and resolution, ONLY FOR VISUALIZATION - DO NOT USE FOR STATS.
#'
#' This regular 2D grid based on bin numbers is formed like so:
#'
#' For the selected spatial resolution, the number of latitudinal rows is calculated, and the number of matrix columns is double that value (this is the number of bins in the row at the equator). The number of unique bins per row is then calculated (e.g. 3 at the pole, 9 in the row below it, ...). Since the binning scheme aims to make each bin approximately equal area on the globe, this means that for each row, the unique bins take up an equal number of pixels. E.g. if your grid is 9000 pixels wide and you have 3000 unique bins in a row, each bin in that row should be repeated 3 times.
#'
#' LATITUDE AND LONGITUDE ARE AT THE CENTER OF THE BIN.
#'
#' @param resolution String indicating spatial resolution, see ?gen_nrows for list of accepted strings.
#' @param ext Named vector containing the boundaries of the resulting grid (xmn, xmx, ymn, ymx).
#' @param rast TRUE/FALSE, should the resulting bin matrix be converted to raster?
#' @param max_bins Maximum grid size (total number of pixels) that can be generated between the selected latitudes. This is to prevent overloading memory with the higher-resolution grids (e.g. 250m, 1km)
#' @return Global raster containing bin numbers.
#' @export
gen_bin_grid = function(resolution="4", ext=c(xmn=-147, xmx=-41, ymn=39, ymx=86), rast=TRUE, max_bins=50000000) {
    # get the number of rows on the global grid, given a spatial resolution
    nrows_all <- gen_nrows(resolution)
    # get a vector of latitudes from -90 to 90 degrees (note: latitudes and bins here start in the southwest), and subset to the selected extent
    latitudes <- (seq(1:nrows_all) - 0.5) * 180/nrows_all - 90
    lat_inds <- which(dplyr::between(latitudes, ext[3], ext[4]))
    # get number of columns for the global grid
    ncol <- 2 * nrows_all
    # stop if this will result in too many bins
    total_bins <- nrows_all*ncol
    if (total_bins > max_bins) {
        stop("ERROR: This will generate a grid containing ",total_bins," bins. Please reduce the distance between your latitude limits (longitude limit adjustments make no difference here, the full rows must be created first before subsetting). You can also try adjusting the max_bins argument to allow this, but proceed with caution as this can use a lot of memory and crash your session.")
    }
    # subset number of rows based on selected extent
    nrows <- length(lat_inds)
    # get the bin at the start of each row and the number of bins per row, and subset them to the selected extent
    start_bin <- gen_start_bin(nrows_all)
    bin_count <- diff(start_bin)
    bin_count <- c(bin_count,bin_count[1])
    start_bin <- start_bin[lat_inds]
    bin_count <- bin_count[lat_inds]
    # fill the bin numbers in on each row
    # note that to make the grid square, some bins are repeated instead of stretching them
    # e.g. if the grid were 900 pixels wide and a given row had 300 bins, each
    #      bin would be repeated 3 times
    bins <- lapply(nrows:1, function(ilat) {
        start_bin[ilat] + floor(seq(0, ncol-1) * bin_count[ilat]/ncol)
    })
    bins <- do.call(rbind, bins)
    # get the longitudinal differences between the bins in the longest possible row
    londiff <- 360 / ncol
    longitudes <- seq(from=(-180+(londiff/2)), to=(180-(londiff/2)), by=londiff)
    # subset bin matrix by longitude
    lon_inds <- dplyr::between(longitudes, ext[1], ext[2])
    bins <- bins[,lon_inds]
    if (rast) {
        # convert bin matrix to raster
        binGrid <- raster::raster(ncols=ncol(bins), nrows=nrow(bins),
                                  xmn=ext[1], xmx=ext[2], ymn=ext[3], ymx=ext[4])
        raster::values(binGrid) <- bins
        bins <- binGrid
    }
    return(bins)
}


# From George White's primary production scripts.
#' Generate 2D grid of L3b data for viewing
#'
#' Given a dataframe with 2 columns (bin number and variable), create a 2D matrix or raster FOR VISUALIZATION ONLY, NOT TO BE USED FOR STATS.
#'
#' @param df Dataframe with 2 columns: first column="bin" and second column is the variable name (note: the dataframe does not have to be sorted in order of bin number).
#' @param resolution String indicating spatial resolution, see ?gen_nrows for list of accepted strings.
#' @param ext Named vector containing the boundaries of the resulting grid (xmn, xmx, ymn, ymx).
#' @param rast TRUE/FALSE, should the resulting bin matrix be converted to raster?
#' @param max_bins Maximum grid size (total number of pixels) that can be generated between the selected latitudes. This is to prevent overloading memory with the higher-resolution grids (e.g. 250m, 1km)
#' @references See https://oceancolor.gsfc.nasa.gov/docs/format/l3bins/ for more information on bin numbers.
#' @examples
#' # make a bathymetry raster for the Northwest Atlantic
#' var_to_rast(df = get_bins(region="nwa",variables=c("bin","bathymetry")),
#'             ext = c(lon_bounds$NWA, lat_bounds$NWA))
#' @return Raster containing the variable data.
#' @export
var_to_rast <- function(df, resolution="4", ext=c(xmn=-147, xmx=-41, ymn=39, ymx=86), rast=TRUE, max_bins=50000000) {
    # create a bin grid for the selected latitudinal extent
    binGrid <- gen_bin_grid(resolution=resolution, ext=ext, rast=FALSE, max_bins=max_bins)
    binGrid_vec <- c(binGrid)
    newmat <- rep(NA, length(binGrid_vec))
    df_bins <- df$bin
    newmat[binGrid_vec %in% df_bins] <- df[[2]][match(binGrid_vec, df_bins, nomatch = 0)]
    datGrid <- matrix(newmat, nrow=nrow(binGrid))
    # convert bin matrix to raster
    if (rast) {
        datrast <- raster::raster(ncols=ncol(datGrid), nrows=nrow(datGrid),
                                  xmn=ext[1], xmx=ext[2], ymn=ext[3], ymx=ext[4])
        raster::values(datrast) <- datGrid
        datGrid <- datrast
        names(datGrid) <- colnames(df)[2]
    }
    return(datGrid)
}

#' Get bin info at 4km and 9km resolution
#'
#' Get corresponding bin number, latitude, longitude and depth for Pan-Canadian Grid or subregions. These are vectors of bin/lat/lon/bathymetry that have already been generated for easier access.
#'
#' @param region String, either "pancan", "nwa", "nep", or "gosl"
#' @param resolution String, either "4km" or "9km".
#' @param variables String with columns: blank or "all" for all columns, or a subset of c("bin","longitude","latitude","bathymetry")
#' @return Data frame with columns of bin, longitude, latitude, bathymetry
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(oceancolouR)
#'
#' pancan_bins_4km <- get_bins()
#'
#' # Map of the North West Atlantic bin bathymetry
#' get_bins(region = "nwa", resolution = "9km") %>%
#' ggplot(aes(x = longitude, y = latitude, colour = bathymetry)) +
#'    geom_point(size = 0.5) +
#'    scale_colour_gradientn(colours = pals::ocean.deep(30))
#' @export
#'
get_bins <- function(region = "pancan", resolution = "4km", variables = "all") {
    reginfo = paste0(region,"_",resolution)
    # bins <- (function(v) get(data(list=reginfo)))(reginfo)
    bins <- (function(v) get(data(list=reginfo, package="oceancolouR", envir = new.env())))(reginfo)
    if (any(variables != "all")) {
        bins = bins[,which(names(bins) %in% variables)]
    }
    return(bins)
}

#' Plot panCanadian L3b file
#'
#' Given a vector of data from a binned ocean colour file, plot it on a raster with coastlines.
#'
#' This is used to get a quick look at a file from the panCanadian dataset, before manipulating the data. When the data is loaded, it's in vector format and in the same order as the bin vectors pancan_bins_4km, nwa_bins_4km, etc...
#'
#' @param vec Numeric vector of data (must be in the same order as the the bins vector of the same resolution, whether pancan, nwa, nep, or gosl - see details).
#' @param region String, either "pancan", "nwa", "nep", or "gosl"
#' @param ext Named vector containing the boundaries of the resulting grid.
#' @param resolution String, either "4km" or "9km".
#' @param limits Limits of the colour scale (numeric vector, length 2).
#' @return Raster containing variable values with coastlines.
#' @examples
#' # This is an example file with data in the same format as in a panCanadian dataset file.
#' data("example01_A2018252.L3b_DAY_CHL_POLY4_NWA")
#' dat <- example01_A2018252.L3b_DAY_CHL_POLY4_NWA
#' lon_lim <- lon_bounds[["NWA"]]
#' lat_lim <- lat_bounds[["NWA"]]
#' plot_pancan(log10(dat), region="nwa", ext=c(range(lon_lim),range(lat_lim)))
#'
#' @export
plot_pancan <- function(vec, region="pancan", ext=c(xmn=-147, xmx=-41, ymn=39, ymx=86), resolution="4km", limits=NULL) {
    stopifnot(resolution %in% c("4km", "9km"))
    bins <- (function(v) get(data(list=v, package="oceancolouR", envir = new.env())))(paste0(region,"_bins_",resolution))
    rast <- var_to_rast(data.frame(bin=bins, var=vec), resolution=gsub("km","",resolution), ext=ext)
    p <- make_raster_map(rast,xlim=ext[1:2],ylim=ext[3:4],col_limits=limits)
    return(p)
}

#' Condense a matrix by averaging selected columns
#'
#' Given a matrix and a list where each element contains a numeric vector of the column indices to average, calculate a condensed matrix.
#'
#' This can be used to take a matrix where rows = pixels and columns = days of the year, and average columns over 8day or month intervals to return, for example, a corresponding matrix of row=pixels and columns=weeks (where a "week" is 8days).
#'
#' @param mat Numeric matrix
#' @param dlist List of numeric vectors, where each vector contains the column indices that should be merged (see example). If left blank, mat must be in order and have no gaps (i.e. a day with no data should be a column of all NA), and a year and "composite" are also required so that dlist can be automatically calculated using the 8day or monthly system.
#' @param year Integer, only needed if dlist is NULL
#' @param composite String, length of output composite, 8day or monthly (only needed if dlist is NULL)
#' @return Numeric matrix with the same number of rows, and the number of columns equal to the length of dlist
#' @examples
#' mat <- matrix(runif(30), nrow=3)
#' avg_columns(mat, dlist=list(1:5, 6:8))
#'
#' @export
avg_columns <- function(mat, dlist=NULL, year=NULL, composite="8day") {

    if (is.null(dlist)) {
        stopifnot(!is.null(year) & composite %in% c("8day", "monthly"))
        last_day <- ncol(mat)
        if (composite=="8day") {
            dlist <- lapply(1:46, function(x) {tmp <- days_vector(year=year, week=x); tmp[tmp <= last_day]})
            dlist <- dlist[sapply(dlist, length) > 0]
        } else if (composite=="monthly") {
            dlist <- lapply(1:12, function(x) {tmp <- days_vector(year=year, month=x); tmp[tmp <= last_day]})
            dlist <- dlist[sapply(dlist, length) > 0]
        }
    }

    new_mat <- lapply(1:length(dlist), function(x) {rowMeans(matrix(mat[,dlist[[x]]],nrow=nrow(mat)), na.rm=TRUE)})
    mat <- do.call(cbind, new_mat)

    return(mat)

}


#' Get bin number, longitude, latitude
#'
#' This creates a dataframe containing the bin number, longitude, and latitudes for the full globe, for a given resolution, using the Integerized Sinusoidal Binning Scheme used by NASA OBPG for their level-3 binned satellite files (e.g. MODIS-Aqua). More info here: https://oceancolor.gsfc.nasa.gov/docs/format/l3bins/
#'
#' WARNING: This retrieves ALL bins, over both land and water. The pre-made bin vectors for pancan/nwa/nep/gosl regions that are retrieved by the get_bins() function only include bins over water.
#'
#' LATITUDE AND LONGITUDE ARE AT THE CENTER OF THE BIN.
#'
#' @param resolution String indicating spatial resolution, see ?gen_nrows for list of accepted strings
#' @param lonlim Minimum and maximum longitude of the area of interest
#' @param latlim Minimum and maximum latitude of the area of interest
#' @param max_bins Maximum number of bins that can be generated between the selected latitudes. This is to prevent overloading memory with the higher-resolution grids (e.g. 250m, 1km)
#' @return Dataframe with 3 columns: bin number, longitude, latitude
#' @export
binlatlon <- function(resolution="4", lonlim=c(-180,180), latlim=c(-90,90), max_bins=50000000) {

    # get number of rows, given a spatial resolution
    nrows_all <- gen_nrows(resolution)

    # get the latitude for each row, and subset them to selected extent
    latitudes <- (seq(1:nrows_all)-0.5)*180/nrows_all - 90
    lat_inds <- dplyr::between(latitudes, latlim[1], latlim[2])
    latitudes <- latitudes[lat_inds]

    # get the bin at the start of each row and the number of bins per row, and subset them to the selected extent
    start_bin <- gen_start_bin(nrows_all)
    bin_count <- diff(start_bin)
    bin_count <- c(bin_count,bin_count[1])
    start_bin <- start_bin[lat_inds]
    bin_count <- bin_count[lat_inds]

    first_bin <- start_bin[1]
    last_bin <- tail(start_bin,1) + tail(bin_count,1) - 1

    # stop if this will result in too many bins
    total_bins <- last_bin-first_bin
    if (total_bins > max_bins) {
        stop("ERROR: This will generate a grid containing ",total_bins," bins. Please reduce the distance between your latitude limits (longitude limit adjustments make no difference here, the full rows must be created first before subsetting). You can also try adjusting the max_bins argument to allow this, but proceed with caution as this can use a lot of memory and crash your session.")
    }

    # get the distance between each bin in each row
    londiff <- 360 / bin_count

    # get a vector of bin numbers over this range
    bin_nums <- first_bin:last_bin

    # get a vector of longitudes for each row
    longitudes <- lapply(1:length(latitudes), function(i) {
        diff <- londiff[i]
        seq(from=(-180+(diff/2)), to=(180-(diff/2)), by=diff)
    })
    longitudes <- do.call(c, longitudes)

    # expand latitude vector to the same length as the longitude vector,
    # and subset to the selected longitudes
    lon_inds <- dplyr::between(longitudes, lonlim[1], lonlim[2])
    longitudes <- longitudes[lon_inds]
    latitudes <- rep(latitudes, bin_count)[lon_inds]
    bin_nums <- bin_nums[lon_inds]

    return(data.frame(bin = bin_nums,
                      longitude = longitudes,
                      latitude = latitudes,
                      stringsAsFactors = FALSE))

}
