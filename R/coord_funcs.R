#' Convert a latitude or longitude in degrees, minutes, seconds, to decimal degrees
#'
#' Also works for degrees, decimal minutes to decimal degrees (leave seconds blank)
#'
#' To reverse this, use `deg_to_dms()` or `deg_to_dm()`.
#'
#' @param deg degrees
#' @param min minutes
#' @param sec seconds
#' @return Input the degrees, minutes and seconds to return the decimal degrees.
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
#' To reverse this, use `dms_to_deg()`.
#'
#' For degrees and decimal minutes use `deg_to_dm()`.
#'
#' @param decdeg number in decimal degrees
#' @return Returns a data.frame() with columns indicating degrees, minutes, seconds.
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
#' To reverse this, use `dms_to_deg()` and leave the "sec" argument blank.
#'
#' For degrees, minutes, seconds use `deg_to_dms()`.
#'
#' @param decdeg number in decimal degrees
#' @return Returns a data.frame() with columns indicating degrees and decimal minutes.
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
