#' Calculate OCI, the chlorophyll-a algorithm that blends OCx and Hu.
#'
#' This is the algorithm currently used by NASA for chlor_a (as of April 2021).
#'
#' If Hu <= CI_bound1, the Hu is used. If it is between the two bounds, Hu and OCx are blended as described in Hu et al (2012).
#'
#' Default waveband and coefficients are calculated based on sensor and whether or not the 443nm waveband should be used for OCx (note that there will be an error if use443nm=FALSE and 443nm is the only blue band available for the selected sensor).
#'
#' @param rrs Either: Numeric matrix where rows = records, columns = Rrs wavebands, with named columns ("Rrs_XXX", where XXX is a wavelength in nanometres), OR: RasterStack of rrs layers with stack layers following the same naming convention. Names must match Rrs wavebands used in OCx (blue and green bands) and Hu (blue/green/red bands), from shortest waveband to longest, starting with "Rrs_".
#' @param sensor String, either modis (for MODIS-Aqua), seawifs, viirs (for VIIRS-SNPP), landsat8, or sentinel2
#' @param use_443nm Logical value, TRUE to make the 443nm band an option in the band ratio (OCx). If false, 443nm will be removed from the list of blue bands before calculating the band ratio. Note that this option does NOT affect hu.
#' @param ocx_bands Sensor-dependent wavebands used in the OCx algorithm - named list of "blues" and "green" bands, e.g. list("blues"=c("Rrs_443","Rrs_488"), "green"="Rrs_547"). See ?get_ocx_bands
#' @param ocx_coefs Numeric vector of coefficients corresponding to terms in the ocx polynomial (lowest degree to highest). See ?get_ocx_coefs
#' @param hu_bands Numeric vector of wavebands (in nanometres) used in the hu algorithm (blue, green, and red, in that order), e.g. c(443,547,667). See ?get_ci_bands
#' @param hu_coefs Numeric vector of coefficients for the hu algorithm. These are NOT sensor-dependent, and there are only two options: 1, for the original set of coefficients, and 2, for a more recent set. See ?get_ci_coefs. Note that these do not start with "Rrs_", but that is added later in the function in order to match them to rrs column names.
#' @param CI_bound1 Numeric value, the lower value where blending between OCI and OCX should begin
#' @param CI_bound2 Numeric value, the higher value where blending between OCI and OCX should end
#' @references
#' See ?hu and ?ocx.
#'
#' @return List containing OCI chlorophyll (For matrix rrs: Numeric value (or vector) -- chlorophyll as computed by OCI for the given Rrs. For RasterStack rrs: equivalent raster with OCI chlorophyll-a), also the indices that were used for Hu and for blending.
#' @examples
#' in_situ_chl <- c(0.589,1.835,0.151,5.368,1.715,0.3,5.092,4.132,0.418,0.182,2.075,1.023,5.671,2.153,0.416,0.53,0.88,2.593,1.067,3.01,0.396,2.834,4.019,4.984,1.259,1.875,2.739,0.411,9.533,0.22,0.514,2.279,0.856,2.332,0.421,0.996,0.331,0.266,0.849,2.656,1.746,0.208,1.251,2.019,0.808,2.23,2.073,0.564,0.666,0.122,2.381,1.571,0.183,1.01,0.517,1.568,0.195,1.824,1.153,0.533,2.611,5.901,2.041,0.627,1.261,6.832,1.094,1.053,3.297,1.232)
#' rrs <- matrix(c(0.0042,0.0036,0.011,0.0047,0.003,0.0072,0.0044,0.0153,0.0058,0.0053,3e-04,0.0052,0.0124,0.0044,0.0066,0.0013,0.0054,0.0064,0.0015,0.0046,0.003,0.004,0.0132,0.0022,0.0022,0.0066,0.0046,0.0067,0.0114,0.0031,0.0135,0.0063,0.006,0.0043,0.0067,0.0035,0.0049,0.0035,0.0022,0.0026,0.014,0.0202,0.0013,0.0015,0.004,0.012,0.0061,0.0101,0.0035,0.0034,0.0045,0.0074,0.0037,0.0025,0.0044,0.0113,0.0019,0.0099,0.0038,0.0158,0.0064,0.0043,0.0059,0.0014,0.0048,0.0042,0.0022,0.0017,0.0025,0.004,0.0046,0.0041,0.0083,0.005,0.0034,0.0071,0.0038,0.0101,0.0096,0.0045,0.0016,0.0048,0.0084,0.0036,0.007,0.0032,0.0054,0.0066,0.002,0.0049,0.0038,0.0042,0.0083,0.0028,0.0032,0.0051,0.0052,0.0062,0.0087,0.0032,0.0082,0.0056,0.0051,0.0045,0.0066,0.0045,0.0052,0.0035,0.0028,0.003,0.0095,0.0128,0.0022,0.0037,0.0042,0.0085,0.0045,0.0085,0.0036,0.0037,0.0055,0.0075,0.0041,0.003,0.0045,0.0096,0.003,0.0068,0.0038,0.0116,0.0057,0.0041,0.0049,0.0022,0.0055,0.0047,0.0037,0.0029,0.0031,0.0043,0.0045,0.0045,0.0042,0.0041,0.0028,0.0034,0.002,0.0056,0.0116,0.0034,0.002,0.0037,0.0052,0.0035,0.0035,0.0032,0.0043,0.0041,0.002,0.0042,0.0034,0.0028,0.0052,0.0039,0.0031,0.0038,0.0031,0.0045,0.0048,0.0027,0.0051,0.003,0.0042,0.0035,0.0034,0.0034,0.0044,0.003,0.0032,0.0028,0.0043,0.0062,0.0028,0.0028,0.0035,0.0045,0.0034,0.0039,0.0023,0.0033,0.005,0.0074,0.0036,0.0035,0.004,0.0049,0.0028,0.0035,0.0032,0.005,0.0033,0.0028,0.0035,0.003,0.0035,0.0041,0.0033,0.0027,0.0029,0.0036,9e-04,9e-04,3e-04,8e-04,4e-04,3e-04,1e-04,8e-04,0.0043,5e-04,2e-04,6e-04,0.0012,8e-04,5e-04,3e-04,8e-04,5e-04,3e-04,8e-04,8e-04,2e-04,7e-04,9e-04,6e-04,5e-04,3e-04,5e-04,5e-04,4e-04,4e-04,4e-04,7e-04,7e-04,3e-04,4e-04,7e-04,5e-04,7e-04,5e-04,5e-04,4e-04,6e-04,4e-04,6e-04,6e-04,1e-04,1e-04,2e-04,5e-04,7e-04,0.0044,8e-04,7e-04,7e-04,5e-04,3e-04,5e-04,5e-04,5e-04,4e-04,2e-04,4e-04,7e-04,4e-04,8e-04,6e-04,3e-04,5e-04,8e-04), ncol=4)
#' colnames(rrs) <- c("Rrs_443", "Rrs_488", "Rrs_547", "Rrs_667")
#' lambdas <- get_ocx_bands("modis", use_443nm = TRUE)
#' chl_oci <- oci(rrs, sensor="modis", ocx_coefs=get_ocx_coefs("modis","nwa","ocx"))
#' library(ggplot2)
#' p <- ggplot(data.frame(x=in_situ_chl, y=chl_oci$oci_chl,
#'                   algorithm=ifelse(chl_oci$hu_ind, "hu", ifelse(chl_oci$blend_ind, "blend", "ocx")))) +
#'     geom_point(aes(x=x, y=y, color=algorithm)) +
#'     geom_abline(slope=1, intercept=0) +
#'     scale_x_log10(limits=c(0.05, 20)) +
#'     scale_y_log10(limits=c(0.05, 20)) +
#'     theme_minimal() +
#'     labs(x="in situ chl", y="satellite chl")
#' print(p)
#'
#' # transform rrs matrix into RasterStack, and test it
#' library(raster)
#' library(magrittr)
#' rrs_stack <- stack(lapply(1:4, function(i) raster(matrix(rrs[1:9,i], nrow=3))))
#' names(rrs_stack) <- colnames(rrs)
#' chl_raster <- oci(rrs_stack, sensor="modis", ocx_coefs=get_ocx_coefs("modis","nwa","ocx"))$oci_chl
#' plot(chl_raster)
#'
#' @export
oci <- function(rrs, sensor="seawifs", use_443nm=TRUE, ocx_bands=get_ocx_bands(sensor,use_443nm), ocx_coefs=get_ocx_coefs(sensor), hu_bands=get_ci_bands(sensor), hu_coefs=get_ci_coefs(2), CI_bound1=0.15, CI_bound2=0.2) {

    blues <- ocx_bands$blues
    green <- ocx_bands$green

    input_class <- class(rrs)[1]

    stopifnot(input_class %in% c("matrix", "RasterStack"))

    hu_bands <- get_ci_bands(sensor)

    if (input_class == "RasterStack") {
        rast <- rrs[[1]] # for reformatting later
        ocx_rrs <- raster_to_matrix(r = rrs, rnames = c(blues, green))
        ci_rrs <- raster_to_matrix(r = rrs, rnames = paste0("Rrs_", hu_bands))
    } else if (input_class == "matrix") {
        if (nrow(rrs)==1) {
            ocx_rrs <- matrix(rrs[,sort(c(blues, green))], nrow=1)
            ci_rrs <- matrix(rrs[,sort(paste0("Rrs_", hu_bands))], nrow=1)
            colnames(ocx_rrs) <- sort(c(blues, green))
            colnames(ci_rrs) <- sort(paste0("Rrs_", hu_bands))
        } else {
            ocx_rrs <- rrs[,sort(c(blues, green))]
            ci_rrs <- rrs[,sort(paste0("Rrs_", hu_bands))]
        }
    }

    chl_ocx <- ocx(rrs=ocx_rrs, blues=blues, green=green, coefs=ocx_coefs, use_443nm=use_443nm)
    chl_hu <- hu(rrs=ci_rrs, wave=hu_bands, coefs=hu_coefs)

    oci_chl <- rep(NA, length(chl_hu))

    # get indices to use OCI, and indices to blend OCI and OCX
    hu_ind <- chl_hu <= CI_bound1 | !is.finite(chl_hu)
    ocx_ind <- chl_hu >= CI_bound2 & is.finite(chl_hu) & is.finite(chl_ocx) & chl_ocx > 0
    blend_ind <- chl_hu > CI_bound1 & chl_hu < CI_bound2 & is.finite(chl_hu) & is.finite(chl_ocx) & chl_ocx > 0

    # calculate coefficients for blending
    a <- (chl_hu[blend_ind] - CI_bound1)/(CI_bound2 - CI_bound1)
    b <- (CI_bound2 - chl_hu[blend_ind])/(CI_bound2 - CI_bound1)

    # change values to hu or a blend of hu/ocx at appropriate indices
    oci_chl[hu_ind] <- chl_hu[hu_ind]
    oci_chl[blend_ind] <- a*chl_ocx[blend_ind] + b*chl_hu[blend_ind]
    oci_chl[ocx_ind] <- chl_ocx[ocx_ind]

    if (input_class == "RasterStack") {
        oci_chl <- raster::raster(crs=raster::crs(rast), ext=raster::extent(rast), resolution=raster::res(rast), vals=oci_chl)
        hu_ind <- raster::raster(crs=raster::crs(rast), ext=raster::extent(rast), resolution=raster::res(rast), vals=hu_ind)
        blend_ind <- raster::raster(crs=raster::crs(rast), ext=raster::extent(rast), resolution=raster::res(rast), vals=blend_ind)
    }

    return(list(oci_chl=oci_chl, hu_ind=hu_ind, blend_ind=blend_ind))

}
