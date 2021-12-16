#' Read h5
#'
#' Read h5 level-3 binned file contents.
#'
#' @param h5_file String, h5 filename.
#' @param var_name String, name of variable to extract (example: "chlor_a").
#' @return List containing the contents of the file, attributes, selected variable, bin list, and bin index.
#' @export
read_h5_L3b <- function(h5_file, var_name) {

    h5 <- hdf5r::H5File$new(h5_file, mode="r")
    h5_ls <- h5$ls(recursive=TRUE) # view contents
    h5_att <- hdf5r::h5attributes(h5)
    output_var <- h5[[paste0("level-3_binned_data/", var_name)]][]
    bin_list <- h5[["level-3_binned_data/BinList"]][]
    BinIndex <- h5[["level-3_binned_data/BinIndex"]][]
    h5$close_all()

    return(list(h5_ls = h5_ls,
                h5_att = h5_att,
                output_var = output_var,
                bin_list = bin_list,
                BinIndex = BinIndex))

}


#' Get flag data
#'
#' Given a number from a matrix of quality flags in a satellite file that uses the bit string system for flags, and the dataframe of all flag bits/names/descriptions, get the descriptions of the flags used by that specific number.
#'
#' @param num Number in the flag matrix
#' @param flag_df Dataframe containing bits and their corresponding info, in order
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
#' get_flag_data(num=pixel1_flag,flag_df=sgli_flag_df)
#'
#' @export
get_flag_data <- function(num,flag_df) {
    flag_df[as.logical(intToBits(num)),]
}


#' Separate flag masks
#'
#' Given a vector or matrix of quality flags using the bit string system for flags, and the indices of the bits to use (starting at 1, for bit0), separate the flag mask into individual flags.
#'
#' @param flags Matrix or vector of flags
#' @param which_bits Bit masks to retrieve, starting at 0 (bit 0 value = 2^0 = 1)
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
#' sfl <- separate_flags(flag_dat, bits)
#' sfl <- do.call(cbind,sfl)
#' colnames(sfl) <- bit_names
#' sfl
#' # now try it in matrix format
#' flag_dat <- matrix(flag_dat,nrow=10)
#' sfl <- separate_flags(flag_dat, bits)
#' # plot the STRAYLIGHT flag
#' names(sfl) <- bit_names
#' raster::spplot(raster::raster(sfl$STRAYLIGHT))
#'
#' @export
separate_flags <- function(flags, which_bits) {
    stopifnot(class(flags)[1] %in% c("matrix","integer","numeric"))
    flag_bits <- lapply(as.numeric(flags),intToBits)
    flag_bits <- matrix(do.call(rbind,flag_bits)[,which_bits+1],nrow=length(flag_bits))
    flag_bits <- matrix(as.logical(flag_bits),nrow=nrow(flag_bits))
    if (class(flags)[1]=="matrix") {
        flag_bits <- lapply(1:ncol(flag_bits),function(x) matrix(flag_bits[,x],nrow=nrow(flags)))
    } else {
        flag_bits <- lapply(1:ncol(flag_bits),function(x) as.logical(flag_bits[,x]))
    }
    names(flag_bits) <- which_bits
    return(flag_bits)
}