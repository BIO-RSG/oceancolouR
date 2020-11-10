# This is a file where we are planning to add multiple functions for extracting matchups easily.
# Plan include:
# Extract from pixel box of n*m size
# Need to decide on input data formats - accept data frame (columns x, y, z), or matrix, etc.?
# Also don't need to duplicate functionality that is already in raster() package


#' Extract matchups from in situ location
#'
#' This converts a week number (8 days per week) to its corresponding date. Year is required as this will vary for leap years. 8 days is the standard number of days in a composite "week" time period as used for ocean colour by NASA.
#'
#' @param extract_longitude Longitude of pixels to extract data from. Numeric
#' @param extract_latitude Latitude of pixels to extract data from. Numeric
#' @param sat_data The satellite data extracting from..this will take some work defining how it will recognize type, lat, lon, etc
#' @param size vector of box size in number of pixels
#' @param input_format Specify the format of input data - raster, data frame or matrix
#' @param projection If wanted
#' @return data frame with
#' @examples
#' @export
# extract_matchups <- function(extract_longitude, extract_latitude, sat_data, size, input_format, projection, ...) {
#
# }
