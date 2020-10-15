# Read h5 level-3 binned file contents
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
