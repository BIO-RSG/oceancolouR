#' Sparkle fill: fill small holes in raster
#'
#' Given a RasterLayer with small gaps (sparkles), fill using specs defined in "min_sides" and "fun"
#'
#' @param x RasterLayer to fill gaps for
#' @param min_sides Number of non NA sides required (e.g. 5 out of a possible 8 pixels directly adjacent to "sparkle")
#' @param fun String of function to fill gap. Current options are "mean" and "median"
#' @return Filled RasterLayer
#' @export
sparkle_fill <- function(x, min_sides, fun, ...) {
    idx_na <- which(as.vector(is.na(x)) == TRUE)
    adj <- adjacent(x, idx_na, directions= 8, include = F, id=T)
    adj <- as.data.frame(adj)
    adj$val <- x[adj$to]
    adj <- adj %>% group_by(id) %>% mutate(n = sum(!is.na(val))) %>% ungroup() %>%
        filter(n >= (8-min_sides))
    if (fun == "mean") {
        adj <- adj %>% group_by(id) %>% summarise(fillval = mean(val, na.rm = T)) %>% ungroup()
        x[idx_na[adj$id]] <- adj$fillval
    } else if (fun == "median") {
        adj <- adj %>% group_by(id) %>% summarise(fillval = median(val, na.rm = T)) %>% ungroup()
        x[idx_na[adj$id]] <- adj$fillval
    }
    return(x)
}
