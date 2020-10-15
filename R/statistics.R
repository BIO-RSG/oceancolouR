# Compute root mean square error (RMSE).
#' @export
rmse <- function(v1=v1, v2=v2) {
    return(sqrt(sum((v1 - v2)^2, na.rm=TRUE) / sum(!is.na(v1) & !is.na(v2))))
}


# Given vectors x and y of the same length, compute various forms of error between the two.
#' @export
vector_errors <- function(x, y) {

    # Compute model errors.
    error <- y - x
    error_mag <- abs(y - x)

    # Compute model percent errors.
    percent_error <- (y - x)/x * 100
    percent_error_mag <- abs((y - x)/x * 100)

    # Compute log error.
    log_error <- log10(y) - log10(x)
    log_error_mag <- abs(log10(y) - log10(x))

    # Remove non-finite values.
    error[!is.finite(error)] <- NA
    error_mag[!is.finite(error_mag)] <- NA
    percent_error[!is.finite(percent_error)] <- NA
    percent_error_mag[!is.finite(percent_error_mag)] <- NA
    log_error[!is.finite(log_error)] <- NA
    log_error_mag[!is.finite(log_error_mag)] <- NA

    return(data.frame(error = error,
                      error_mag = error_mag,
                      percent_error = percent_error,
                      percent_error_mag = percent_error_mag,
                      log_error = log_error,
                      log_error_mag = log_error_mag,
                      stringsAsFactors = FALSE))

}


# Calculate the statistics on a vector:
# Number of valid observations, mean, median, standard deviation, and coefficient of variation.
#' @export
vector_stats <- function(x=x) {

    output <- rep(NA,5)
    output[1] <- sum(is.finite(x))
    output[2] <- mean(x,na.rm=T)
    output[3] <- median(x,na.rm=T)
    output[4] <- sd(x,na.rm=T)
    output[5] <- sd(x,na.rm=T)/mean(x,na.rm=T)
    names(output) <- c("nobs", "mean", "median", "sd", "cv")

    return(output)

}

