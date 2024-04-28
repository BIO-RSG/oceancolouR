#' Sparkle fill: fill small holes in raster or matrix
#'
#' Fill RasterLayer or matrix with small gaps (sparkles) using specs defined in "min_sides" and "fun"
#'
#' @param x RasterLayer or matrix to fill gaps for
#' @param min_sides Number of non NA sides required (e.g. min_sides = 5 means 5 out of a possible 8 pixels directly adjacent to "sparkle" must be non NA)
#' @param fun String of function to fill gap. Current options are "mean", "median" and "bilinear"
#' @param matlon If x is a matrix, it needs a matrix of latitudes with dim(matlon) == dim(x)
#' @param matlat If x is a matrix, it needs a matrix of longitudes with dim(matlat) == dim(x)
#' @return Filled RasterLayer or matrix
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "group_by"
#' @importFrom dplyr "ungroup"
#' @importFrom dplyr "filter"
#' @importFrom dplyr "mutate"
#' @importFrom dplyr "summarise"
#' @export
sparkle_fill <- function (x, min_sides = 4, fun = "median", matlon, matlat, ...) {
    require(tidyr)
    if (class(x)[1] == "RasterLayer") {
        idx_na <- which(as.vector(is.na(x)) == TRUE)
        adj <- raster::adjacent(x, idx_na, directions = 8, include = F,
                                id = T)
        adj <- as.data.frame(adj)
        adj$val <- x[adj$to]
        adj <- adj %>% group_by(id) %>% mutate(n = sum(!is.na(val))) %>%
            ungroup() %>% filter(n >= (min_sides)) # Was 8 - min_sides before...
        if (fun == "mean") {
            adj <- adj %>% group_by(id) %>% summarise(fillval = mean(val, na.rm = T)) %>% ungroup()
            x[idx_na[adj$id]] <- adj$fillval
        }
        else if (fun == "median") {
            adj <- adj %>% group_by(id) %>% summarise(fillval = median(val, na.rm = T)) %>% ungroup()
            x[idx_na[adj$id]] <- adj$fillval
        }
        else if (fun == "geomean") {
          adj <- adj %>% group_by(id) %>% summarise(fillval = oceancolouR::geoMean(val, na.rm = T)) %>% ungroup()
          x[idx_na[adj$id]] <- adj$fillval
        }
        else if (fun == "bilinear") {
            x2 <- raster::resample(x, x, "bilinear")
            x[idx_na[adj$id]] <- x2[idx_na[adj$id]]
        }
        return(x)
    }
    else if (is.matrix(x) == TRUE) {
        if (missing(matlon) | missing(matlat)) {
            message("Please enter lat and lon matrices")
        }
        else {
            x <- raster::raster(x, xmn = min(matlon), xmx = max(matlon),
                                ymn = min(matlat), ymx = max(matlat))
            idx_na <- which(as.vector(is.na(x)) == TRUE)
            adj <- raster::adjacent(x, idx_na, directions = 8,
                                    include = F, id = T)
            adj <- as.data.frame(adj)
            adj$val <- x[adj$to]
            adj <- adj %>% group_by(id) %>% mutate(n = sum(!is.na(val))) %>% ungroup() %>% filter(n >= (min_sides))
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
            x <- raster::as.matrix(x)
            return(x)
        }
    } else if (class(x)[1] == "SpatRaster") {
        idx_na <- which(as.vector(is.na(x)) == TRUE)
        adj <- terra::adjacent(x, idx_na, directions = 8, include = T)
        adj <- as.data.frame(adj)
        adj = pivot_longer(adj, cols = "V2":"V9") %>% filter(!is.na(value))
        adj = adj %>% rename(from=V1, to=value)
        adj$val <- x[adj$to]$chlor_a
        adj <- adj %>% group_by(from) %>% mutate(n = sum(!is.na(val))) 
        adj <- adj %>% filter(n >= (min_sides)) # Was 8 - min_sides before...
        if (fun == "mean") {
          adj <- adj %>% group_by(from) %>% summarise(fillval = mean(val, na.rm = T)) %>% ungroup()
          x[adj$from] <- adj$fillval
        }
        else if (fun == "median") {
          adj <- adj %>% group_by(from) %>% summarise(fillval = median(val, na.rm = T)) %>% ungroup()
          x[adj$from] <- adj$fillval
        }
        else if (fun == "geomean") {
          adj <- adj %>% group_by(from) %>% summarise(fillval = oceancolouR::geoMean(val, na.rm = T)) %>% ungroup()
          x[adj$from] <- adj$fillval
        }
        else if (fun == "bilinear") {
          # For now, converting to raster then back to spatraster
          x2=raster::raster(x)
          x2 <- raster::resample(x2, x2, "bilinear")
          x2=rast(x2)
          x[adj$from] <- x2[adj$from]
        }
        return(x)
  }
    
    else {
        message("Data type is not one of matrix or RasterLayer")
    }
}

