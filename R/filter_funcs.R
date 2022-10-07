#' Get single value from pixel window
#'
#' Given a square matrix of pixel values, return a single value representing the whole box.
#'
#' This is used when reading L2 satellite points matched to in situ data in order to get a single pixel value representing the box (e.g. 5x5 pixels) around the center pixel closest to the in situ sampling location.
#'
#' @param var Numeric vector or square matrix, length input_ws^2
#' @param input_ws Integer, length of one side of the input square pixel box
#' @param output_ws Integer, length of one side of the output square pixel box (<= input_ws)
#' @param min_pix Integer, minimum number of valid pixels required
#' @param useCVfilter TRUE/FALSE - if median CV (coefficient of variation, filteredSD/filteredMean) is > max_CV, return NA?
#' @param max_CV Maximum allowed CV
#' @param stat_to_use String, either median, mean, or geomean - the statistic used on the final filtered set of var
#' @return Single numeric value representing the input window
#' @export
collapse_window <- function(var, input_ws, output_ws, min_pix, useCVfilter=TRUE, max_CV=0.15, stat_to_use="median") {
  stopifnot(output_ws <= input_ws)
  var <- matrix(var, nrow=input_ws)
  center <- ceiling(input_ws/2)
  p1 <- center-floor(output_ws/2)
  p2 <- center+floor(output_ws/2)
  var <- as.numeric(var[p1:p2,p1:p2])
  var <- var[is.finite(var)]
  if (length(var) < min_pix) {return(NA)}
  if (useCVfilter) {
    CV <- filtered_mean(var)$coef_of_variation
    if (CV > max_CV) {return(NA)}
  }
  if (stat_to_use=="median") {
    return(median(var))
  } else if (stat_to_use=="mean") {
    return(mean(var))
  } else if (stat_to_use=="geomean") {
    return(geoMean(var))
  }
}
