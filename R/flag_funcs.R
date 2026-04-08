
# Functions below were adjusted March/April 2026 by Stephanie Clay and Kristen Wilson to accept 64-bit integers (previously only worked with 32-bit integers), to be compatible with the 54-flag system used in Sentinel ocean colour data.
# Kristen: lapply [used in get_flag_layers()] was throwing way too many error issues with allocation and the variable lists were causing huge headaches. get_flag_layer_64() took 20minutes to make the land mask on a full image.
# FYI, you can check if a number exceeds the 32-bit integer system with: num > .Machine$integer.max


#*******************************************************************************
# FUNCTIONS FOR USE WITH 32-BIT FLAGGING SYSTEM

#' Get vector of bits from number
#'
#' Given a number, deconstruct it into a vector indicating which bits are present. ONLY WORKS WITH 32-BIT INTEGERS (see get_flag_bits_64() to get bits from larger numbers).
#'
#' @param num Integer value
#' @return Integer vector from 0 to 31
#' @examples
#' # load example SGLI dataset containing real data, their quality flags, and flag descriptions
#' data("example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000")
#' data("sgli_flag_df")
#' # get the flag value of the first pixel
#' pixel1_flag <- example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000$flags[1]
#' pixel1_flag
#' # now plug it into the function to see the bits present in this value
#' get_flag_bits(pixel1_flag)
#' @export
get_flag_bits <- function(num) {
    bits_present <- which(as.logical(intToBits(num)))-1
    return(bits_present)
}


#' Get flag data
#'
#' Given a 32-bit integer from a matrix of quality flags in a satellite ocean colour file that uses the bit string system for flags, and a dataframe of flag bits/names, get the names of the flags used by that specific number. THIS DOES NOT WORK WITH 64-BIT INTEGERS (in that case, see get_flag_data_64()).
#'
#' @param num Number in the flag matrix
#' @param flag_df Dataframe containing two columns: "bit" (e.g. 0, 1, 2, 4, 8, 16, ..., a numeric column) and "name" (flag name)
#' @return Subsetted flag_df containing only the names of the flags contained within the "num" value
#' @examples
#' # load example SGLI dataset containing real data, their quality flags, and flag descriptions
#' data("example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000")
#' data("sgli_flag_df")
#' # look at the descriptions of the flags
#' sgli_flag_df
#' # get the flag value of the first pixel
#' pixel1_flag <- example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000$flags[1]
#' pixel1_flag
#' # now plug it into the function to see which flags are used in this value
#' get_flag_data(num=pixel1_flag,flag_df=sgli_flag_df)
#' @export
get_flag_data <- function (num, flag_df) {
    bits_present <- get_flag_bits(num)
    ind <- flag_df$bit %in% bits_present
    return(flag_df[ind,])
}


#' Separate flag masks
#'
#' Given a vector or matrix of quality flags using the bit string system for flags, and the bits to use, separate the flag mask into individual flags. THIS DOES NOT WORK WITH 64-BIT INTEGERS (in that case, see get_flag_layer_64() - you will need to use this for Sentinel data).
#'
#' @param flags Matrix or vector of flags
#' @param which_bits Bit masks to retrieve, a vector of integers from 0 to 31
#' @param flagnames Vector of flag names corresponding to which_bits
#' @return List, in the same order as which_bits, where each list item is a logical matrix or vector (same size as "flags") that contains only the information for a particular bit
#' @examples
#' # load example SGLI dataset containing real data, their quality flags, and flag descriptions
#' data("example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000")
#' data("sgli_flag_df")
#' flag_dat <- example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000$flags
#' # subset to see a quick example
#' flag_dat <- flag_dat[1:100]
#' # bits to retrieve
#' bits <- c(0:5,8)
#' bit_names <- sgli_flag_df$name[sgli_flag_df$bit %in% bits]
#' # try splitting flags in vector format
#' sfl <- get_flag_layers(flag_dat, bits, bit_names)
#' sfl <- do.call(cbind,sfl)
#' sfl
#' # now try it in matrix format
#' flag_dat <- matrix(flag_dat,nrow=10)
#' sfl <- get_flag_layers(flag_dat, bits, bit_names)
#' # plot the STRAYLIGHT flag
#' raster::spplot(raster::raster(sfl$STRAYLIGHT))
#' @export
get_flag_layers <- function(flags, which_bits, flagnames) {
    stopifnot(class(flags)[1] %in% c("matrix","integer","numeric"))
    flag_bits <- lapply(as.numeric(flags),get_flag_bits)
    flag_layers <- lapply(which_bits, function(x) sapply(flag_bits, function(y) {x %in% y}))
    if (class(flags)[1]=="matrix") {
        flag_layers <- lapply(flag_layers,function(x) matrix(x,nrow=nrow(flags),ncol=ncol(flags)))
    }
    names(flag_layers) <- flagnames
    return(flag_layers)
}

#' @rdname get_flag_layers
#' @export
separate_flags <- get_flag_layers



#*******************************************************************************
# FUNCTIONS FOR USE WITH 64-BIT FLAGGING SYSTEM

#' Get vector of bits from number
#'
#' Given a number, deconstruct it into a vector indicating which bits are present. WORKS WITH 64-BIT INTEGERS. This is used to determine which flags affect a satellite ocean colour pixel, given a single integer value for that pixel.
#'
#' @param num Integer value
#' @return Integer vector from 0 to 63
#' @examples
#' # load example SGLI dataset containing real data, their quality flags, and flag descriptions
#' data("example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000")
#' data("sgli_flag_df")
#' # get the flag value of the first pixel
#' pixel1_flag <- example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000$flags[1]
#' pixel1_flag
#' # now plug it into the function to see which flags are used in this value
#' get_flag_bits_64(pixel1_flag)
#' @export
get_flag_bits_64 = function(num) {
    bits_present = which(as.logical(as.numeric(rev(strsplit(bit64::as.bitstring(bit64::as.integer64(num)),split="")[[1]]))))-1
    return(bits_present)
}


#' Get flag data
#'
#' Given an integer (can be up to 64-bit) from a matrix of quality flags in a satellite ocean colour file that uses the bit string system for flags, and the dataframe of flag bits/names, get the names of the flags used by that specific number.
#'
#' @param num Number in the flag matrix
#' @param flag_df Dataframe containing two columns: "bit" (e.g. 0, 1, 2, 4, 8, 16, ..., a numeric column) and "name" (flag name)
#' @return Subsetted flag_df containing only the info of the flags contained within the "num" value
#' @examples
#' # load example SGLI dataset containing real data, their quality flags, and flag descriptions
#' data("example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000")
#' data("sgli_flag_df")
#' # look at the descriptions of the flags
#' sgli_flag_df
#' # get the flag value of the first pixel
#' pixel1_flag <- example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000$flags[1]
#' pixel1_flag
#' # now plug it into the function to see which flags are used in this value
#' get_flag_data_64(num=pixel1_flag,flag_df=sgli_flag_df)
#' @export
get_flag_data_64 <- function (num, flag_df) {
    bits_present <- get_flag_bits_64(num)
    ind <- flag_df$bit %in% bits_present
    return(flag_df[ind,])
}


#' Check if a specific bit is "on" for an integer
#'
#' Given an integer (can be up to 64-bit), convert it to its corresponding bit string and check if the bit at a specific location is "on" (i.e. 1).
#'
#' @param num Number in the flag matrix
#' @param flag_df Dataframe containing two columns: "bit" (e.g. 0, 1, 2, 4, 8, 16, ..., a numeric column) and "name" (flag name)
#' @return TRUE/FALSE value indicating whether the bit is 1
#' @export
get_flag_bits_64_checkout = function(num, which_bits) {
    flag.return = get_flag_bits_64(num)
    return(any(flag.return%in%which_bits))
}


#' Get individual flag mask from 64-bit integer flag matrix
#'
#' Given a matrix of quality flags using 64-bit integers, extract a matrix that indicates when an individual bit (e.g., flag) is "on". This can be used to extract a mask for a specific flag from Sentinel-3 quality flag layer.
#'
#' @param flags Matrix of 64-bit integers
#' @param which_bit Bit to retrieve, an integer from 0 to 63
#' @return Logical matrix
#' @examples
#' data = as.integer64(c(2^0, 2^1, 2^2,
#'                       sum(2^(0:1)), 2^3, 2^1,
#'                       sum(2^(0:4)), sum(2^(0:1)), 2^0,
#'                       2^0, 2^1, 2^2))
#' dim(data) = c(4,3)
#' data
#' which_bit = 0
#' matrix_byrow = T
#' # output a TRUE/FALSE matrix for the flag that uses bit 0
#' get_flag_layer_64(data, which_bit, matrix_byrow)
#'
#' @export
get_flag_layer_64 = function(data, which_bit, matrix_byrow=TRUE){
    stopifnot(class(data)[1] %in% c("integer64") & length(which_bit)==1)
    bit.vect = mapply(get_flag_bits_64_checkout, data, which_bit)
    bit.mat = matrix(bit.vect, nrow=nrow(data),ncol=ncol(data), byrow=matrix_byrow)
    return(bit.mat)
}
