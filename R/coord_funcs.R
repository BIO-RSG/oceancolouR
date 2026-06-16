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



#' Convert a coordinate between different systems.
#'
#' Convert between the following systems: degree decimal minutes (ddm), decimal degrees (dd), and degree minutes seconds (dms).
#'
#' Required input formats:
#' ddm: A character vector, e.g. "43 56.638"
#' dd: A numeric vector, e.g. 43.94397
#' dms: A character vector, e.g. 48° 51' 30.24"
#'
# Note: Degree decimals minutes are often used in marine and aviation navigation.
#'
#' @param coords Vector of coordinates in the input format (either character or numeric, see details)
#' @param input Character string indicating the input format (either ddm, dd, or dms)
#' @param output Character string indicating the output format (either ddm, dd, or dms)
#' @param decimals Numeric value, how many digits to include after the decimal?
#' @return Returns a vector of coordinates in the selected output format (either a character vector for ddm and dms, or numeric vector for dd)
#' @examples
#'coords_ddm <- c("43 56.638", "42 13.645", "42 16.908")
#'coords_dd <- c(43.94397, 42.22742, 42.2818)
#'coords_dms <- c("43° 56' 38.28","42° 13' 38.7","42° 16' 54.48")
#'
#'# ddm to dms
#'convert_coords(coords_ddm, input="ddm", output="dms")
#'
#'# dms to dd
#'convert_coords(coords_dms, input="dms", output="dd")
#'
#' @export
#' @importFrom magrittr %>%
convert_coords <- function(coords, input, output, decimals=3) {

    stopifnot(input %in% c("ddm","dd","dms") & output %in% c("ddm","dd","dms"))

    if (input=="ddm") {
        deg = abs(as.numeric(unlist(strsplit(coords, " "))[seq(1, length(coords)*2,2)]))
        dec = as.numeric(unlist(strsplit(coords, " "))[seq(2, length(coords)*2,2)])
        if (output=="dd") {
            final <- dms_to_deg(deg=deg, min=dec)
        } else if (output=="dms") {
            tmp_dd <- dms_to_deg(deg=deg, min=dec)
            final <- deg_to_dms(tmp_dd) %>% tidyr::unite(col="output1",degrees,minutes,sep="° ") %>% tidyr::unite(col="output2",output1,seconds,sep="' ") %>% unlist()
        }
    } else if (input=="dd") {
        coords <- as.numeric(coords)
        if (output=="ddm") {
            final <- deg_to_dm(coords) %>% tidyr::unite(col="output",degrees,minutes,sep=" ") %>% unlist()
        } else if (output=="dms") {
            final <- deg_to_dms(coords) %>% tidyr::unite(col="output1",degrees,minutes,sep="° ") %>% tidyr::unite(col="output2",output1,seconds,sep="' ") %>% unlist()
        }
    } else if (input=="dms") {
        degr = strsplit(coords,split=" ") %>% sapply(FUN="[",1) %>% gsub(pattern="°",replacement="") %>% trimws() %>% as.numeric()
        minu = strsplit(coords,split=" ") %>% sapply(FUN="[",2) %>% gsub(pattern="'",replacement="") %>% trimws() %>% as.numeric()
        seco = strsplit(coords,split=" ") %>% sapply(FUN="[",3) %>% trimws() %>% as.numeric()
        if (output=="ddm") {
            tmp_dd <- dms_to_deg(deg = degr, min = minu, sec = seco)
            final <- deg_to_dm(tmp_dd) %>% tidyr::unite(col="output",degrees,minutes,sep=" ") %>% unlist()
        } else if (output=="dd") {
            final <- dms_to_deg(deg = degr, min = minu, sec = seco)
        }
    }

    # fix the number of digits after the decimal
    final_split <- strsplit(as.character(final), split="[.]")
    # make sure it works in cases where there is no decimal value
    fslen <- sapply(final_split, FUN=length)
    first_value <- sapply(final_split, FUN="[", 1)
    second_value <- rep(NA,length(first_value))
    second_value[fslen==2] <- sapply(final_split[fslen==2], FUN="[", 2)
    second_value <- round(as.numeric(paste0("0.",second_value)), decimals)
    final <- paste0(first_value,gsub("0.",".",second_value))
    if (output=="dd") {
        final <- as.numeric(final)
    }
    return(unname(final))

}
