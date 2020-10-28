#' Get start days, given interval
#'
#' Get a list of days of the year for a given year and selected months. This can return every day, or the start of every week, second week, or month. It also can use either the 8-day/week 46 week/year system as used by NASA, or the 4 week/month 48 week/year system as used at BIO (only relevant for weekly or biweekly intervals).
#'
#' @param year
#' @param months Numeric vector of month numbers
#' @param eightday Logical value (use eightday system?)
#' @param interval String, either "daily", "weekly", "biweekly", or "monthly"
#' @return Character vector containing days of the year, each zero-padded to length 3.
#' @export
get_doys <- function(year, months=1:12, eightday=TRUE, interval="daily") {

    if (interval=="daily") {

        if (lubridate::leap_year(year)) {d <- 1:366
        } else {d <- 1:365}

    } else if (interval=="monthly") {

        d <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
        # If it's a leap year, adjust dates after February.
        if (lubridate::leap_year(year)) {d[3:length(d)] <- d[3:length(d)] + 1}

    } else {

        # Get the starting day of each "week".
        if (eightday) {

            # Note: The first day of the week does not change for leap years when using NASA's 8-day per week system.
            d <- as.integer(8*(0:45)+1)
            # Take a subset of weeks if using biweekly sets.
            if (interval=="biweekly") {d <- d[seq(1,length(d),2)]}

        } else {

            d <- c(1,9,16,24,32,40,47,55,60,68,75,83,91,99,106,114,121,129,136,
                   144,152,160,167,175,182,190,197,205,213,221,228,236,244,252,
                   259,267,274,282,289,297,305,313,320,328,335,343,350,358)
            if (lubridate::leap_year(year)) {d[9:length(d)] <- d[9:length(d)] + 1}
            if (interval=="biweekly") {d <- d[seq(1,length(d),2)]}

        }

    }

    # Get the month from each day
    m <- as.numeric(sapply(1:length(d), function(i) lubridate::month(lubridate::parse_date_time(paste0(year,pad_num(d[i],3)),"%Y%j"))))
    m_ind <- m %in% months

    # Get the day numbers for the selected months
    d <- d[m_ind]

    result <- sapply(d, oceancolouR::pad_num, len=3)

    return(result)

}

# This function returns the 8-day week number of a given date.
# Example: week8("2019-01-28")
week8 <- function(x,..) (lubridate::yday(x) - 1)%/%8 + 1
