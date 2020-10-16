#' Create folder
#'
#' Check for existence of a folder, and create it if necessary.
#'
#' @param path Path to create (a string). Absolute or relative.
#' @return The path name.
#' @export
get_dir <- function(path) {
    if (!dir.exists(path)) {dir.create(path, showWarnings=F)}
    path
}


#' Number padding
#'
#' Pad a number with leading zeroes to make it length "len".
#'
#' @param num The number.
#' @param len The length, single numeric value.
#' @return The number as a string with leading zeroes.
#' @export
pad_num <- function(num, len) {
    num <- as.numeric(num)
    len <- as.numeric(len)
    num_len <- nchar(as.character(floor(num)))
    if (num_len > len) {len <- num_len}
    paste0(paste(replicate(len - num_len, '0'), collapse=''), num)
}


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

