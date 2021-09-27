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


#
# # num = base 2 flag number to convert to binary string
# get_flag_data <- function(num,dict) {
#     dict[as.logical(intToBits(num)),]
# }
#
# # given a matrix of quality flags, separate them into matrices for individual flags
# # note we're using bit INDICES, not bit numbers (i.e. for bit 0, use which_bit_inds=1)
# separate_flags <- function(flag_mat, which_bit_inds) {
#     flag_bits <- lapply(as.numeric(flag_mat),intToBits)
#     flag_bits <- matrix(do.call(rbind,flag_bits)[,which_bit_inds],nrow=length(flag_bits))
#     flag_bits <- matrix(as.logical(flag_bits),nrow=nrow(flag_bits))
#     flag_bits <- lapply(1:ncol(flag_bits),function(x) matrix(flag_bits[,x],nrow=nrow(flag_mat)))
#     return(flag_bits)
# }






