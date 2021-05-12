# Source: https://stackoverflow.com/questions/24956546/capitalizing-letters-r-equivalent-of-excel-proper-function
#' Capitalization
#'
#' Capitalize the first letter of a word.
#'
#' @param x The word (string).
#' @return The capitalized word.
#' @export
proper <- function(x) {
    paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
}


# Source: https://stackoverflow.com/questions/26503350/how-to-extract-one-specific-group-in-dplyr
#' Select groups in dplyr
#'
#' This returns the selected groups in a dataframe by index number.
#'
#' @param data Dataframe that has already been grouped using dplyr::group_by()
#' @param groups Numeric vector of group numbers to use as the indices of the groups to select
#' @return Same format as input dataframe, with only the selected groups, in the order listed. Duplicate group IDs are removed.
#' @examples
#' # This grabs the first 2 groups listed.
#' mtcars %>%
#' group_by(cyl) %>%
#' select_groups(1:2)
#'
#' @export
select_groups <- function(data, groups, ...) {
    groups <- unique(as.numeric(groups))
    num_groups <- nrow(attr(data, "groups"))
    if (any(groups > num_groups)) {
        groups <- groups[groups <= num_groups]
        warning("Some requested group indices exceed number of groups (", num_groups, ")")
    }
    if (length(groups)==0) {
        stop("No valid group indices requested.")
    }
    inds <- unlist(attr(data, "groups")[groups, ])
    inds <- inds[(length(groups)+1):length(inds)]
    return(data[inds, ])
}


#' Convert raster stack to matrix
#'
#' Given a raster stack with layer names, and a vector of names to extract from that raster stack, use the raster::getValues() function to extract the values from each layer and put each flattened layer in the column of a matrix.
#'
#' @param r Raster stack with layer names
#' @param rnames Vector of names from the raster stack
#' @return Numeric matrix with column names matching rnames
#' @export
raster_to_matrix <- function(r, rnames) {
    rstack <- raster::subset(r, rnames)
    mat <- lapply(1:length(rnames), function(i) raster::getValues(rstack[[i]]))
    mat <- do.call(cbind, mat)
    colnames(mat) <- rnames
    return(mat)
}


#' Order the characters in a single string
#'
#' @param x String
#' @return String with individual characters in order
#' @examples
#' x = "023521"
#' order_string(x)
#' @export
order_string <- function(x) {
    paste0(stringr::str_sort(unlist(strsplit(x, split = ""))), collapse="")
}
