#' Sparkle fill: fill small holes in raster
#'
#' Given a RasterLayer with small gaps (sparkles), fill using specs defined in "min_sides" and "fun"
#'
#' @param x RasterLayer to fill gaps for
#' @param min_sides Number of non NA sides required (e.g. 5 out of a possible 8 pixels directly adjacent to "sparkle")
#' @param fun String of function to fill gap. Current options are "mean" and "median"
#' @return Filled RasterLayer
#' @export
sparkle_fill <- function(x, min_sides, fun, matlon, matlat, ...) {
    if (class(x)[1] == "RasterLayer") {
        require(dplyr)
        idx_na <- which(as.vector(is.na(x)) == TRUE)
        adj <- raster::adjacent(x, idx_na, directions= 8, include = F, id=T)
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
        } else if (fun == "bilinear") {
            x2 <- raster::resample(x, x, "bilinear")
            x[idx_na[adj$id]] <- x2[idx_na[adj$id]]
        }
        return(x)
    } else if (is.matrix(x) == TRUE) {
        if (missing(matlon) | missing(matlat)) {
            print("Please enter matlat and matlon matrices")
        } else {
            print("exist")
            require(dplyr)
            x <- raster::raster(x, xmn = min(matlon), xmx = max(matlon),
                                 ymn = min(matlat), ymx = max(matlat))
            idx_na <- which(as.vector(is.na(x)) == TRUE)
            adj <- raster::adjacent(x, idx_na, directions= 8, include = F, id=T)
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
            } else if (fun == "bilinear") {
                x2 <- raster::resample(x, x, "bilinear")
                x[idx_na[adj$id]] <- x2[idx_na[adj$id]]
            }
            x <- as.matrix(x)
            return(x)
        }
    } else {
        print("Data type is not one of matrix or RasterLayer")
    }
}
