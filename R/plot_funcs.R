#' asinh ggplot scale transformation
#'
#' This can be used to do an arcsinh transformation of a ggplot scale, similar to the log transformation (but with the ability to handle values <= 0). To do a sinh transformation, use "sinh" instead of "asinh" (see ?sinh_trans).
#'
#' @examples
#' library(ggplot2)
#' ggplot(data.frame(x=1:10,y=1:10), aes(x=x,y=y)) +
#'     geom_point() +
#'     scale_y_continuous(trans="asinh")
#'
#' @export
asinh_trans <- function() {
    # Source: https://stackoverflow.com/questions/14504869/histogram-with-negative-logarithmic-scale-in-r
    scales::trans_new(name = "asinh", transform = function(x) asinh(x), inverse = function(x) sinh(x))
}


#' sinh ggplot scale transformation
#'
#' This can be used to do an sinh transformation of a ggplot scale. To do an asinh transformation, use "asinh" instead of "sinh" (see ?asinh_trans).
#'
#' @examples
#' library(ggplot2)
#' ggplot(data.frame(x=1:10,y=1:10), aes(x=x,y=y)) +
#'     geom_point() +
#'     scale_y_continuous(trans="sinh")
#'
#' @export
sinh_trans <- function() {
    scales::trans_new(name = "sinh", transform = function(x) sinh(x), inverse = function(x) asinh(x))
}
