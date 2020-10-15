#' Create folder.
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


# Pad a number with leading zeroes to make it length "len".
#' @export
pad_num <- function(num, len) {
    num_len <- nchar(as.character(floor(num)))
    if (num_len > len) {len <- num_len}
    paste0(paste(replicate(len - num_len, '0'), collapse=''), num)
}


# Capitalize the first letter of a word.
# Source: https://stackoverflow.com/questions/24956546/capitalizing-letters-r-equivalent-of-excel-proper-function
#' @export
proper <- function(x) {
    paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
}
