# Create a basic template for a region of interest (ROI, currently only AZMP or extNA).
#' @export
create_blank_raster <- function(ROI) {

    if (ROI=="AZMP") {
        # AZMP (Atlantic Zone Monitoring Program) template (resolution used in tifs).
        ROI_extent <- raster::extent(-71, -42, 39, 62.5)
        ROI_crs <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
        ROI_res <- c(0.0213235294117647, 0.0134902411021814)
    } else if (ROI=="extNA") {
        # extNA (extended North Atlantic) template.
        ROI_extent <- raster::extent(-95, -42, 39, 82)
        ROI_crs <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
        ROI_res <- c(0.0213235294117647, 0.0134902411021814)
    }

    ROI_template <- raster::raster(ROI_extent, crs=ROI_crs, resolution=ROI_res, vals=NA)

    return(ROI_template)

}
