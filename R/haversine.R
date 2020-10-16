#' Haversine
#'
#' Calculate the haversine distance between 2 points in metres, given lat/lon for each (in decimal degrees).
#'
#' @param lon1 Decimal longitude of first point (numeric value).
#' @param lat1 Decimal latitude of first point (numeric value).
#' @param lon2 Decimal longitude of second point (numeric value).
#' @param lat2 Decimal latitude of second point (numeric value).
#' @return Distance between the points (in metres).
#' @export
haversine <- function(lon1, lat1, lon2, lat2) {

    # Convert decimal degrees to radians.
    lon1 <- lon1 * pi / 180
    lat1 <- lat1 * pi / 180
    lon2 <- lon2 * pi / 180
    lat2 <- lat2 * pi / 180

    dlon <- lon2 - lon1
    dlat <- lat2 - lat1
    under_root <- sin(dlat/2)^2 + (cos(lat1) * cos(lat2) * sin(dlon/2)^2)
    haver <- 2 * asin(sqrt(under_root))
    km <- 6371 * haver # Radius of Earth ~6371km
    m <- km * 1000
    return(m)

}
