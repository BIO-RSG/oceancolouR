#' @export
asinh_trans <- function() {
    # Source: https://stackoverflow.com/questions/14504869/histogram-with-negative-logarithmic-scale-in-r
    # To use: scale_y_continuous(trans="asinh")
    scales::trans_new(name = "asinh", transform = function(x) asinh(x), inverse = function(x) sinh(x))
}

#' @export
sinh_trans <- function() {
    scales::trans_new(name = "sinh", transform = function(x) sinh(x), inverse = function(x) asinh(x))
}
