#' Get start days, given interval
#'
#' Get a list of days of the year for a given year and selected months. This can return every day, or the start of every week, second week, or month. It also can use either the 8-day/week 46 week/year system as used by NASA, or the 4 week/month 48 week/year system as used at BIO (only relevant for weekly or biweekly intervals).
#'
#' @param year Numeric value
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
    tmp_datestrs <- paste0(year,stringr::str_pad(d,width=3,side="left",pad="0"))
    m <- as.numeric(sapply(1:length(d), function(i) lubridate::month(lubridate::parse_date_time(tmp_datestrs[i],"%Y%j"))))
    m_ind <- m %in% months

    # Get the day numbers for the selected months
    d <- d[m_ind]

    result <- stringr::str_pad(d, width=3, side="left", pad="0")

    return(result)

}

#' 8-day week number
#'
#' This converts a date or date vector to its corresponding 8-day week number(s). 8 days is the standard number of days in a composite "week" time period as used for ocean colour by NASA. Use similarly to lubridate functions like lubridate::week().
#'
#' @param dateval String or date formatted with as.Date()
#' @return numeric value of week number
#' @examples
#' week8("2020-03-27")
#' week8(your_data_frame$Dates)
#' @export
week8 <- function(dateval) {
    (lubridate::yday(dateval) - 1)%/%8 + 1
}

#' Date from 8-day week number
#'
#' This converts a week number (8 days per week) to its corresponding date. Year is required as this will vary for leap years. 8 days is the standard number of days in a composite "week" time period as used for ocean colour by NASA.
#'
#' @param yearnum Numeric year number or vector
#' @param weeknum Numeric week number or vector
#' @return date value formatted with as.Date()
#' @examples
#' week8_date(2010, 8)
#' week8_date(yearnum = c(2010,2010), weeknum = c(8,9))
#' @export
week8_date <- function(yearnum, weeknum) {
    x <- weeknum*8-7
    x <- as.Date(paste(yearnum,x), "%Y %j")
    return(x)
}

#' Date group numbers
#'
#' This assigns a date or date vector to a corresponding group of dates of some length in a given year. This is a helper function for making composites. The group is calculated from day 1 of the year.
#' If no group length entered, this just uses the lubridate::week() function (7-day week length)
#'
#' @param dateval Date string or date formatted with as.Date()
#' @param grp_length Number indicating length of group of days
#' @return numeric value of "week" number
#' @examples
#' dategrp(dateval = "2020-03-27")
#' dategrp(dateval = c("2020-03-27","2020-04-27"), grp_length = 4)
#' @export
dategrp <- function(dateval, grp_length) {
    if(missing(grp_length)) {
        return(lubridate::week(dateval))
    } else {
        return(((lubridate::yday(dateval) - 1)%/%grp_length) + 1)
    }
}

#' List days of the year within a month
#'
#' Get a list of days of the year within a specific month or week (following the 8day-per-week system)
#'
#' @param year Numeric 4-digit value
#' @param month Numeric value (1-12)
#' @param week Numeric value (1-46)
#' @return Vector of numeric values (days of the year)
#' @export
days_vector <- function(year, month=NULL, week=NULL) {
    if (!is.null(month) & !is.null(week)) {
        return("Error: Enter only a month number OR week number")
    } else if (!is.null(month)) {
        month <- stringr::str_pad(month,width=2,side="left",pad="0")
        first_day <- as.numeric(format(as.Date(paste0(year,"-",month,"-01")), "%j"))
        last_day <- first_day + as.numeric(lubridate::days_in_month(as.Date(paste0(year,"-",month,"-01")))) - 1
    } else if (!is.null(week)) {
        if (lubridate::leap_year(year)) {
            year_end <- 366
        } else {
            year_end <- 365
        }
        jvec <- 8*(0:45)+1
        first_day <- jvec[week]
        if (first_day==361) {
            last_day <- year_end
        } else {
            last_day <- first_day + 7
        }
    }
    return(first_day:last_day)
}

#' Get season name
#'
#' Given a date object, get the name of the season.
#'
#' Feb-Apr = Spring,
#' May-Jul = Summer,
#' Aug-Oct = Fall,
#' Nov-Feb = Winter
#'
#' @param date
#' @return String (either Spring, Summer, Fall, or Winter)
#' @export
get_season <- function(date) {
    stopifnot(class(date)=="Date")
    month <- lubridate::month(date)
    if (month %in% 2:4) {
        return("Spring")
    } else if (month %in% 5:7) {
        return("Summer")
    } else if (month %in% 8:10) {
        return("Fall")
    } else if (month %in% c(1,11,12)) {
        return("Winter")
    }
}
