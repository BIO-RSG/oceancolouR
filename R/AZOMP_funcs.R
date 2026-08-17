
# Functions for use in AZOMP-related work (Atlantic Zone Off-Shelf Monitoring Program)


#' Subset a dataset to the AR7W line
#'
#' Given a dataset with coordinates, subset it to the Atlantic Repeat Hydrography West 7 line.
#'
#' See https://www.bio.gc.ca/science/monitoring-monitorage/azomp-pmzao/azomp-pmzao-en.php
#'
#' @param df dataframe, must contain columns "longitude" and "latitude", in decimal degrees (negative for west/south)
#' @param line_dist buffer size, in degrees, around AR7W line (everything within this zone is included)
#' @return df, with rows subset to the AR7W
#'
#' @export
subset_to_ar7w <- function(df, line_dist=0.1) {

    # slope and "intercept" of ar7w line were created by extracting the lats/lons of the stations and fitting a linear model:
    #
    # # list of AR7W stations from Li and Harrison, 2013, Table 2:
    # ar7w_stations <- read.csv("AZOMP/li_harrison_2013_table2.csv")
    # colnames(ar7w_stations) <- c("region","station","latitude","longitude","bathymetry","section_dist")
    # ar7w_stations$longitude <- -ar7w_stations$longitude
    # ar7w_lat <- ar7w_stations$latitude
    # ar7w_lon <- ar7w_stations$longitude
    # # find line and slope/intercept corresponding to ar7w line
    # ar7w_line <- lm(ar7w_lat ~ ar7w_lon)
    # m <- as.numeric(coef(ar7w_line)[2])
    # b <- as.numeric(coef(ar7w_line)[1])

    m <- 0.9368089
    b <- 105.8197
    x <- seq(-56,-48,by=.1)
    y <- m*x + b

    # only use data between 2 lines that enclose the ar7w line (shift line_dist in direction perpendicular to line)
    top_coords <- oceancolouR::shift_line(x=x, y=y, dist=line_dist, dir="up")
    bottom_coords <- oceancolouR::shift_line(x=x, y=y, dist=line_dist, dir="down")
    polyy <- c(top_coords$y[1], top_coords$y[2], bottom_coords$y[2], bottom_coords$y[1], top_coords$y[1])
    polyx <- c(top_coords$x[1], top_coords$x[2], bottom_coords$x[2], bottom_coords$x[1], top_coords$x[1])

    # create the mask for this polygon around the ar7w line
    mask <- sp::point.in.polygon(df$longitude, df$latitude, polyx, polyy)
    mask <- as.logical(mask)

    # return subsetted data around ar7w
    return(df[mask,])

}


