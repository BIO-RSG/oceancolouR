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


#' Make scales of list of ggplot objects the same
#'
#' Given a list of ggplots, make the x and/or y scales of the plots the same.
#'
#' Note that if you want to manually adjust scales (ymin, ymax, etc), you should do it in each plot using, for example, scale_y_continuous limits. Here, xmin/xmax are only used if xsame=TRUE, and ymin/ymax are only used if ysame=TRUE.
#'
#' @param p List of ggplot objects
#' @param xsame TRUE/FALSE, should x axes be the same?
#' @param ysame TRUE/FALSE, should y axes be the same?
#' @param xmin Optional minimum value for x axis (see details)
#' @param xmax Optional maximum value for x axis (see details)
#' @param ymin Optional minimum value for y axis (see details)
#' @param ymax Optional maximum value for y axis (see details)
#' @return Same list of ggplots, with axes adjusted
#' @examples
#' library(ggplot2)
#' library(patchwork) # for wrap_plots
#'
#' p1 <- ggplot(data.frame(x=1:100,y1=1:100), aes(x=x,y=y1)) + geom_point()
#' p2 <- ggplot(data.frame(x=1:100,y2=rnorm(100)), aes(x=x,y=y2)) + geom_point()
#' p3 <- ggplot(data.frame(x=1:100,y3=50:-49), aes(x=x,y=y3)) + geom_point()
#' p4 <- ggplot(data.frame(x=4:5, y4=1:2), aes(x=x,y=y4)) + geom_point()
#' p <- list(p1, p2, p3, p4)
#'
#' wrap_plots(p)
#' wrap_plots(same_scales(p))
#' wrap_plots(same_scales(p, xsame=FALSE))
#'
#' @export
same_scales <- function(p, xsame=TRUE, ysame=TRUE, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) {
    p_new <- p
    if (xsame) {
        p_ranges_x <- sapply(1:length(p), function(i) get_ranges(p[[i]])$x)
        new_xmin <- max(c(min(p_ranges_x), xmin))
        new_xmax <- min(c(max(p_ranges_x), xmax))
        p_new <- lapply(1:length(p_new), function(i) {p_new[[i]] + ggplot2::xlim(new_xmin, new_xmax)})
    }
    if (ysame) {
        p_ranges_y <- sapply(1:length(p), function(i) get_ranges(p[[i]])$y)
        new_ymin <- max(c(min(p_ranges_y), ymin))
        new_ymax <- min(c(max(p_ranges_y), ymax))
        p_new <- lapply(1:length(p_new), function(i) {p_new[[i]] + ggplot2::ylim(new_ymin, new_ymax)})
    }
    return(p_new)
}


#' Get ggplot scale ranges
#'
#' Given a ggplot object, get the ranges of the x and y scales.
#'
#' @param p A ggplot object
#' @return Named list with x and y axis ranges
#' @examples
#' library(ggplot2)
#' p <- ggplot(data.frame(x1=1:100,y1=50:-49), aes(x=x1,y=y1)) + geom_point()
#' get_ranges(p)
#'
#' @export
get_ranges <- function(p) {
    xranges <- ggplot2::ggplot_build(p)$layout$panel_scales_x[[1]]$range$range
    yranges <- ggplot2::ggplot_build(p)$layout$panel_scales_y[[1]]$range$range
    return(list(x=xranges,y=yranges))
}

