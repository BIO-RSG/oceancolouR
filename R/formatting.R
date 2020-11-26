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

