
#' Get red waveband used in Nechad's SPM model
#'
#' Given a sensor name, get the waveband used in Nechad's SPM model (doi:10.1016/j.rse.2009.11.022)
#'
#' @param sensor String, currently only occci is accepted
#' @return Character vector in the form "Rrs_XXX" where XXX is the waveband in nanometers
#' @export
get_spm_nechad_band <- function(sensor="occci") {
    all_wvs <- list("occci"="Rrs_665")
    return(all_wvs[[sensor]])
}

#' Get Ap and Cp coefficients used in Nechad's SPM model
#'
#' Given a sensor name, get the Ap and Cp coefficients used in Nechad's SPM model (doi:10.1016/j.rse.2009.11.022)
#'
#' @param sensor String, currently only occci is accepted
#' @return Named list with Ap and Cp values
#' @export
get_spm_nechad_coefs <- function(sensor="occci") {
    Ap <- Cp <- NA
    if (sensor=="occci") {
        Ap <- 355.85
        Cp <- 0.1728
    }
    return(list(Ap=Ap, Cp=Cp))
}

#' Get Nechad SPM
#'
#' Given a sensor name, calculate SPM using Nechad's model (doi:10.1016/j.rse.2009.11.022)
#'
#' @param spm_rrs Either: Numeric vector of Rrs from the red band used in the model (see ?get_spm_nechad_band), OR: RasterLayer or SpatRaster of these Rrs
#' @param sensor String of the sensor name, currently only occci is accepted
#' @return Numeric vector (or RasterLayer or SpatRaster) of the spm values
#' @examples
#' # example using a vector
#' spm_rrs <- c(0.00032,0.00044,0.00075,2e-04,0.00023,8e-05,0.00022,5e-05,4e-05,4e-05,1e-05,0.00013,0.00029,0.00043,6e-04,0.00053,0.00048,0.00097,0.0022,0.00238,0.00126,0.00048,0.00171,0.00118,0.00214,0.00027,0.00011,0.00027,0.00029,0.00094,0.00018,0.00134,0.00076,0.00124,0.00083,4e-04,0.00051,0.00027,0.00024,0.00137,0.00079,0.00039,0.00056,0.00167,0.00046,5e-04,0.00056,0.00034,3e-04,0.00039,0.00058,0.00084,0.00042,0.00067,0.00115,0.00093,0.00054,0.00043,0.00061,0.00076,0.00044,0.0013,0.00057,0.00101,0.00106,0.00084,0.00133,0.00103,6e-04,0.00034,0.00017,0.00014,0.00018,0.00029,0.00039,0.00038,0.00048,0.00057,0.00067,0.00048,0.00014,0.00036,0.00018,0.00141,8e-05,0.00141,0.00025,0.00108,0.00045,0.00079,0.00198,0.00115,0.00036,0.00042,0.00025,0.00013,0.00043,3e-04,0.00151,0.00081,0.00116,0.00155,0.00164,8e-05,0.00029,0.00015,0.00078,0.0014,0.00105,0.00103,0.00102,0.00084,2e-04,0.00031,0.00031,0.00045,4e-04,0.00089,0.00078,0.00072,0.00139,0.00034,0.00037,0.00052,0.00046,0.00065,0.00015,3e-04,0.00043,0.00013,0.00058,0.00016,0.00021,0.00022,0.00061,0.00045,0.0014,0.0035,0.00239,0.00694,0.00142,8e-05,0.00036,0.00035,0.00069,0.00043,0.00049,0.00046,0.00245,0.00074,0.00107,0.00087,0.00025,0.00027,0.00022,0.00022,0.00027,0.00059,0.00069,0.00033,0.00055,0.00049,0.00034,0.00028,0.00131,0.00202,0.00074,0.00083,0.00043,0.00042,0.00101,9e-04,0.00111,0.00094,0.00031,0.00025,0.00027,0.00024,2e-04,0.00022,0.00055,0.00013,0.00137,0.00115,0.00152,0.00017,0.00022,0.00023,8e-04,0.00043,0.001,0.00118,0.00255,0.00128,0.00191,0.00074,0.00057,0.00015,0.00024,0.00021,0.00032,0.00023,0.00023,0.00017,0.00014,0.00017,0.00016,0.00021,0.00022,0.00062,0.00057,0.00044,0.00054,0.00097,0.00178,0.00182,0.00154,0.00121,0.00039)
#' spm <- get_spm_nechad(spm_rrs, sensor="occci")
#' plot(spm)
#'
#' # example using a spatraster
#' library(terra)
#' data("example04_OCCCI.20240703.L3b.DAY.RRS.PANCAN")
#' spm_rrs <- terra::rast(example04_OCCCI.20240703.L3b.DAY.RRS.PANCAN$Rrs_665) # Daily Rrs_665 from OC-CCI multisensor product
#' spm <- get_spm_nechad(spm_rrs, sensor="occci")
#' plot(spm)
#'
#' @export
get_spm_nechad <- function(spm_rrs, sensor="occci") {
    input_class <- class(spm_rrs)[1]
    stopifnot(input_class %in% c("numeric", "RasterLayer", "SpatRaster"))
    spm_coefs <- get_spm_nechad_coefs(sensor)
    rho <- spm_rrs*pi
    spm <- spm_coefs$Ap*rho/(1-(rho/spm_coefs$Cp))
    return(spm)
}









# library(ncdf4)
# library(pracma) # for 2d interpolation
# library(raster)
# library(rasterVis)
# library(parallel)
# library(gridExtra)
# library(ggplot2)
#
#
# #===============================================================================
# # DESCRIPTION
# #===============================================================================
#
# # Created by Stephanie Clay, March 2018.
# #
# # Using the steps outlined in Loisel et al. (2018), retrieve bbp spectral slope
# # from MODIS 1km-resolution L2 files containing Rrs, chlor_a, and solar zenith,
# # for wavelengths 412, 443, 488, and 547.
# #
# # Use this information to retrieve the particle size distribution (PSD) slope
# # ("Junge" slope), and No from a set of look-up tables (LUTS) for the equation
# #
# #       N(D) = No(D/Do)^-PSD_slope
# #
# # from Kostadinov et al (2009), where N is the number of particles per volume of
# # seawater normalized by the size bin width (units m^-4), D is the particle
# # diameter in metres, and No and Do are reference values.
#
# # Credit to Tihomir Kostadinov for the updated look-up tables (2018) for the PSD
# # slope and "No" value for the equation.
#
# # Output netCDF layers:
# #   1. bbp443
# #   2. bbp slope
# #   3. psd slope
# #   4. No
# #   5. N
# #   6. latitude
# #   7. longitude
#
#
#
#
# #===============================================================================
# # INSTRUCTIONS AND VARIABLES TO CHANGE
# #===============================================================================
#
# # L2 files for extraction must be stored in script subfolder "downloads/processed"
# # Reference files, listed below, must be stored in the same location as the scripts:
# # fit_poly3_bb_Kd1vsRrs_all.txt
# # LUT_No_A.csv
# # LUT_xi_A.csv
#
# atmcor <- "MUMM_SWIR" # MUMM_SWIR, MUMM_NIR, NIR_SWIR, NIR, or NIR_RH
#
# Do <- 2e-6 #reference value
# # Find the number of particles, N, of diameter D:
# D <- 1e-4
#
# # # Choose test points below if you only want a specific area to be extracted.
# # # outside plume: 48.328453, -125.78515
# # # inside plume: 49.127478, -123.386433
# # test_lat <- 49.127478
# # test_lon <- -123.386433
#
#
#
#
#
# #===============================================================================
# # FUNCTIONS
# #===============================================================================
#
#
# # Use 2-D interpolation to retrieve coefficients in step 7.
# # (This can be tested with a single value for xp, yp, and NA_ind)
# get_coefs <- function(xp, yp, x, y, Z, NA_ind) {
#     if (length(xp)==1) {nrow <- 1; ncol <- 1
#     } else {nrow <- nrow(yp); ncol <- ncol(yp)}
#     result <- matrix(NA,nrow=nrow,ncol=ncol)
#     result[!NA_ind] <- interp2(x,y,Z,xp[!NA_ind],yp[!NA_ind])
#     result
# }
#
# # # TEST THE 2D INTERPOLATION FUNCTION TO GET COEFS.
# # coefs <- read.table("fit_poly3_bb_Kd1vsRrs_all.txt")
# # names(coefs) <- c("eta","muw","thetas","c1","c2","c3")
# # c1_mat <- matrix(coefs[,"c1"],nrow=8,ncol=21)[rev(1:8),] # row=muw, col=eta
# # eta_coefs <- unique(coefs[,"eta"])
# # muw_coefs <- coefs[rev(1:8),"muw"]
# # example_bad_values <- F
# # # Numbers found in table.
# # example_muw <- 0.712903
# # example_eta <- 0.100
# # # Numbers not found in table (test interpolation).
# # example_muw <- 0.73
# # example_eta <- 0.089
# # c1 <- get_coefs(example_eta, yp=example_muw, x=eta_coefs, y=muw_coefs, Z=c1_mat, NA_ind=example_bad_values)
#
# get_bbp_slope <- function(i, bbp, lambda) {
#     x <- log10(lambda)
#     y <- sapply(1:length(lambda),function(j) {log10(bbp[[j]][[i]])})
#     if (any(is.na(y)) | any(is.infinite(y)) | any(is.nan(y))) {return(NA)}
#     bbp_lm <- lm(y ~ x)
#     bbp_lm$coefficients[[2]]
# }
#
#
# # Plot a grid of rasters based on lambda, which is length 4.
# # "mats" is a list of objects that can be coerced to rasters.
# # Remove values below lower or above upper.
# # https://oscarperpinan.github.io/rastervis/
# raster_plot <- function(lambda,mats,names,lower=-Inf,upper=Inf) {
#     # All the flipping and transposing nonsense is necessary to fix the orientation of the raster.
#     g1 <- mats[[1]]; g1[g1 > upper | g1 < lower] <- NA
#     g2 <- mats[[1]]; g2[g2 > upper | g2 < lower] <- NA
#     g3 <- mats[[1]]; g3[g3 > upper | g3 < lower] <- NA
#     g4 <- mats[[1]]; g4[g4 > upper | g4 < lower] <- NA
#     r1 <- flip(t(flip(raster(g1),1)),1)
#     r2 <- flip(t(flip(raster(g2),1)),1)
#     r3 <- flip(t(flip(raster(g3),1)),1)
#     r4 <- flip(t(flip(raster(g4),1)),1)
#     lambda_stack <- stack(list(r1,r2,r3,r4))
#     names(lambda_stack) <- paste0(names,lambda)
#     levelplot(lambda_stack)
# }
#
# # Make jpgs of intermediate plots.
# make_jpg <- function(f,rast,path,name,lower=-Inf,upper=Inf) {
#     rast[rast < lower | rast > upper] <- NA
#     pngname <- paste0(path,f,'_',name,'.png')
#     png(pngname,width=3000,height=2400,units="px",res=600,pointsize = 10)
#     par(omi=rep(0.01,4),mgp=c(2,1,0),mai=c(0.75,0.5,0.5,0.5))
#     plot(flip(t(flip(raster(rast),1)),1),main=name)
#     dev.off()
# }
#
#
#
#
#
# #===============================================================================
# # MORE VARIABLES
# #===============================================================================
#
# in_path <- paste0("downloads/processed/")
# # Location of L2 files.
# l2_path <- paste0(in_path,"L2/")
# # Get a list of L2 files in that folder.
# l2_files <- list.files(l2_path,pattern=".L2")
# # Remove files from the list that don't end in L2.
# l2_files <- l2_files[endsWith(l2_files,".L2")]
# # Remove files that were not create with the selected atmospheric correction method.
# if (atmcor=="MUMM_SWIR") {
#     l2_files <- l2_files[which(grepl("MUMM-SWIR",l2_files))]
# } else if (atmcor=="NIR_SWIR") {
#     l2_files <- l2_files[which(grepl("aeropt-9",l2_files))]
# } else if (atmcor=="NIR") {
#     l2_files <- l2_files[which(grepl("aeropt-3",l2_files))]
# } else if (atmcor=="NIR_RH") {
#     l2_files <- l2_files[which(grepl("aeropt-2",l2_files))]
# } else if (atmcor=="MUMM_NIR") {
#     l2_files <- l2_files[which(grepl("aeropt-10",l2_files))]
# }
#
# # Set the output paths for csv and nc files.
# out_path_csv <- get_dir(paste0(in_path,"csv/"))
# out_path_nc <- get_dir(paste0(in_path,"nc/"))
#
# # Refractive indices of air and water.
# na <- 1
# nw <- 1.341
#
# # Scattering coefficient of water at wavelength=500nm
# bw500 <- 0.00288
#
# # Absorption of water.
# aw <- c() # 4 values here (one for each wavelength)
#
# # MODIS wavelengths
# lambda <- c(412,443,488,547)
#
# # v coefficients for wavelengths 412,443,488,547, from step 2
# v1 <- c(-4.7636, -4.6216, -3.6636, -2.0152)
# v2 <- c(-2.1269, -2.3587, -2.3116, -1.5296)
# v3 <- c(3.1752, 3.1235, 2.5648, 1.7751)
#
# # Get coefficients needed to compute total backscattering in step 7.
# coefs <- read.table("fit_poly3_bb_Kd1vsRrs_all.txt")
# names(coefs) <- c("eta","muw","thetas","c1","c2","c3")
# # If computed values of eta and muw are outside the range of this lookup table (LUT),
# # they will be converted to the closest value. Below are the maximum differences
# # allowed between the satellite muw/eta values and the limits of the LUT. If the
# # satellite value is too far from the LUT, it will be converted to NA.
# # Defaults: 0.0001, 0.005
# max_eta_diff <- 0.0001
# max_muw_diff <- 0.005
#
# # Get tables containing the PSD slope and No values based on bbp spectral slope.
# ref_vals <- read.csv("LUT_slopes_modis.csv")
# psd_slope <- ref_vals$psd_slope
# logbbp443No <- ref_vals$log10_bbp443.No
#
#
#
#
#
# #===============================================================================
# # MAIN CODE
# #===============================================================================
#
# # Loop through and process each L2 file.
# for (f in 628:length(l2_files)) {
#
#     fname <- l2_files[[f]]
#
#     # TEST INDIVIDUAL FILE
#     # #===========================================================================
#     # if (fname != "A2014223212000.L2") {next}
#     # #===========================================================================
#
#     cat(paste0("\n\nProcessing ",fname,"...\n"))
#
#     # Get the variables from the first L2 file.
#     l2 <- nc_open(paste0(l2_path,fname))
#     chlor_a <- ncvar_get(l2,"geophysical_data/chlor_a") # SAT_OCX: OC3M/OCI blend
#     #chlor_a <- ncvar_get(l2,"geophysical_data/chl_gsm") # SAT_GSM
#     # # Get solz (solar zenith angle) and convert from degrees to radians if using bbp_chl model (need it for sin/cos functions).
#     # solz <- ncvar_get(l2,"geophysical_data/solz") # solar zenith angle
#     # solz <- solz * pi/180
#     Rrs_412 <- ncvar_get(l2,"geophysical_data/Rrs_412")
#     Rrs_443 <- ncvar_get(l2,"geophysical_data/Rrs_443")
#     Rrs_488 <- ncvar_get(l2,"geophysical_data/Rrs_488")
#     Rrs_547 <- ncvar_get(l2,"geophysical_data/Rrs_547")
#     l2_lat <- ncvar_get(l2,"navigation_data/latitude")
#     l2_lon <- ncvar_get(l2,"navigation_data/longitude")
#     bbp_412_qaa <- ncvar_get(l2,"geophysical_data/bbp_412_qaa")
#     bbp_443_qaa <- ncvar_get(l2,"geophysical_data/bbp_443_qaa")
#     bbp_488_qaa <- ncvar_get(l2,"geophysical_data/bbp_488_qaa")
#     bbp_547_qaa <- ncvar_get(l2,"geophysical_data/bbp_547_qaa")
#     nc_close(l2)
#
#     # # Define a 11x11 grid for testing.
#     # #===========================================================================
#     # dist_grid <- abs(haversine(l2_lon, l2_lat, test_lon, test_lat))
#     # match_l2 <- which(dist_grid==min(dist_grid,na.rm=T),arr.ind=T)
#     # mrow <- match_l2[[1]]
#     # mcol <- match_l2[[2]]
#     # mr1 <- mrow - 40
#     # mr2 <- mrow + 40
#     # mc1 <- mcol - 40
#     # mc2 <- mcol + 40
#     # # Create subsets.
#     # solz <- solz[mr1:mr2,mc1:mc2]
#     # #chlor_a <- chlor_a[mr1:mr2,mc1:mc2]
#     # Rrs_443 <- Rrs_443[mr1:mr2,mc1:mc2]
#     # Rrs_547 <- Rrs_547[mr1:mr2,mc1:mc2]
#     # Rrs_412 <- Rrs_412[mr1:mr2,mc1:mc2]
#     # Rrs_488 <- Rrs_488[mr1:mr2,mc1:mc2]
#     # l2_lat <- l2_lat[mr1:mr2,mc1:mc2]
#     # l2_lon <- l2_lon[mr1:mr2,mc1:mc2]
#     # bbp_412_qaa <- bbp_412_qaa[mr1:mr2,mc1:mc2]
#     # bbp_443_qaa <- bbp_443_qaa[mr1:mr2,mc1:mc2]
#     # bbp_488_qaa <- bbp_488_qaa[mr1:mr2,mc1:mc2]
#     # bbp_547_qaa <- bbp_547_qaa[mr1:mr2,mc1:mc2]
#     # #===========================================================================
#
#
#
#     # # TEST NEW_OCX
#     # #===========================================================================
#     # ocx_coefs <- c(0.220223619,-2.352905117,-0.081255038,-0.137666479)
#     # rrs_ocx <- Rrs_488/Rrs_547
#     # ys = log10(rrs_ocx)
#     # chlor_a <- 10^(ocx_coefs[1] + ocx_coefs[2]*ys + ocx_coefs[3]*ys^2 + ocx_coefs[4]*ys^3)
#     # #===========================================================================
#
#
#
#     fname <- paste0(strsplit(fname,"_")[[1]][1],".L2")
#
#     Rrs <- list(Rrs_412,Rrs_443,Rrs_488,Rrs_547)
#
#     # Get the indices of invalid data. This index doesn't need to be applied until
#     # later (step 3, to subset the chla layer based on low and high values, and
#     # then step 7, when it's updated to include bad muw/eta values and applied
#     # to all necessary rasters).
#     bad_values <- (is.na(chlor_a) | is.nan(chlor_a) | is.infinite(chlor_a)
#                    | is.na(Rrs_412) | is.nan(Rrs_412) | is.infinite(Rrs_412)
#                    | is.na(Rrs_443) | is.nan(Rrs_443) | is.infinite(Rrs_443)
#                    | is.na(Rrs_488) | is.nan(Rrs_488) | is.infinite(Rrs_488)
#                    | is.na(Rrs_547) | is.nan(Rrs_547) | is.infinite(Rrs_547))
#
#     if (length(chlor_a)==sum(bad_values)) {
#         cat(paste0("\nWarning: Zero valid pixels in region of interest. Skipping file ",fname,"\n\n"))
#         next
#     }
#
#     # PLOT RASTERS
#     # temp_chl <- chlor_a
#     # temp_chl[temp_chl > 2] <- NA
#     # plot(flip(t(flip(raster(temp_chl),1)),1),main="chlor_a")
#     #plot(flip(t(flip(raster(chlor_a),1)),1),main="chlor_a")
#     #plot(flip(t(flip(raster(solz),1)),1),main="solz (radians)") # radians
#     #plot(flip(t(flip(raster(solz * 180/pi),1)),1),main="solz (degrees)") # degrees
#     #raster_plot(lambda,Rrs,"Rrs",lower=0,upper=0.02)
#
#
#
#
#     # GET BBP FOR EACH WAVELENGTH
#     # Different options for formula: bbp(chl) from Loisel et al, or one of NASA's
#     # current formulas: bbp_giop or bbp_qaa
#     #===========================================================================
#
#     cat("\nComputing bbp...\n")
#
#
#     # # COMMENT OUT STARRED SECTION IF USING NASA'S BBP BELOW
#     # #***************************************************************************
#     #
#     # # STEP 1A=======================================
#     #
#     # muw <- cos(asin(sin(solz)/nw))
#     #
#     # # PLOT RESULTS
#     # # All the flipping and transposing nonsense is necessary to fix the orientation of the raster.
#     # #plot(flip(t(flip(raster(muw),1)),1),main="muw")
#     # cat("Step 1A completed.\n")
#     #
#     # # STEP 1B=======================================
#     #
#     # q <- Rrs_443/Rrs_547 # try 488/547? original = 443/547
#     # kd_exp <- lapply(1:length(lambda),function(i) {(v1[[i]]*log(q) + v2[[i]]) / (v3[[i]] + log(q))})
#     # kd <- lapply(1:length(lambda),function(i) {10^kd_exp[[i]]})
#     #
#     # # PLOT RESULTS
#     # # qtest <- q
#     # # qtest[qtest > 0.9 | qtest < 0.6] <- NA
#     # # plot(flip(t(flip(raster(qtest),1)),1),main="q")
#     # #plot(flip(t(flip(raster(q),1)),1),main="q")
#     # #raster_plot(lambda,kd_exp,"kd_exp",lower=-1,upper=1)
#     # #raster_plot(lambda,kd,"kd",upper=1.3)
#     #
#     # cat("Step 1B completed.\n")
#     #
#     # # STEP 1C=======================================
#     #
#     # # Calculate particulate scattering, bp (NOTE: NOT backscattering)
#     # bp550 <- 0.416 * (chlor_a ^ 0.766)
#     #
#     # vc <- matrix(NA,nrow=dim(chlor_a)[[1]],ncol=dim(chlor_a)[[2]])
#     # lowchl <- chlor_a > 0.02 & chlor_a < 2 & !bad_values
#     # highchl <- chlor_a >= 2 & !bad_values
#     # vc[lowchl] <- 0.5 * (log10(chlor_a[lowchl]) - 0.3)
#     # vc[highchl] <- 0
#     #
#     # # Scattering of particulate matter
#     # # This returns a matrix for each wavelength
#     # bp <- lapply(1:length(lambda),function(i) {bp550 * ((lambda[[i]]/550) ^ vc)})
#     #
#     # # Scattering of water
#     # bw <- bw500 * ((lambda/500) ^ (-4.3))
#     #
#     # # PLOT RESULTS
#     # #plot(flip(t(flip(raster(bp550),1)),1),main="bp550")
#     # #plot(flip(t(flip(raster(vc),1)),1),main="vc")
#     # #raster_plot(lambda,bp,"bp")
#     #
#     # cat("Step 1C completed.\n")
#     #
#     # # STEP 1D=======================================
#     #
#     # # This returns a matrix for each wavelength
#     # eta <- lapply(1:length(lambda),function(i) {bw[[i]] / (bp[[i]] + bw[[i]])})
#     #
#     # # PLOT RESULTS
#     # #raster_plot(lambda,eta,"eta")
#     #
#     # cat("Step 1D completed.\n")
#     #
#     # # # STEP 1E=======================================
#     # # # Total absorption (untested, don't need this now)
#     # #
#     # # # Coefficients provided by a LUT, depending on eta and muw (note: eta is 3d)
#     # # c1, c2, c3, c4
#     # #
#     # # denom <- lapply(1:length(lambda),function(i) {c1[[i]] + c2[[i]] * Rrs[[i]] + c3[[i]] * (Rrs[[i]])^2 + c4[[i]] * (Rrs[[i]])^3})
#     # # a <- lapply(1:length(lambda),function(i) {kd / denom[[i]]})
#     # #
#     # # cat("Step 1E completed.\n")
#     #
#     # # # STEP 1F=======================================
#     # # # More absorption
#     # #
#     # # anw <- lapply(1:length(lambda),function(i) {a[[i]] - aw[[i]]})
#     # #
#     # # cat("Step 1F completed.\n")
#     #
#     # # STEP 1G=======================================
#     #
#     # # Coefficients provided by LUT
#     # c1_mat <- matrix(coefs[,"c1"],nrow=8,ncol=21)[rev(1:8),] # row=muw, col=eta
#     # c2_mat <- matrix(coefs[,"c2"],nrow=8,ncol=21)[rev(1:8),]
#     # c3_mat <- matrix(coefs[,"c3"],nrow=8,ncol=21)[rev(1:8),]
#     #
#     # # Update "bad_values" index: exclude NA, infinite, or values of muw or eta too far outside the acceptable range.
#     # # Get the number of bad values before and after removing invalid muw/eta to print the number of pixels containing bad muw/eta values.
#     # old_bad_vals <- sum(bad_values)
#     # # Update with bad muw values.
#     # bad_values <- bad_values | (muw < (0.712903 - max_muw_diff)) | (muw > (1 + max_muw_diff)) | is.na(muw) | is.nan(muw) | is.infinite(muw)
#     # # Update with bad eta values.
#     # for (i in 1:length(lambda)) {bad_values <- bad_values | (eta[[i]] > (0.2 + max_eta_diff)) | is.na(eta[[i]]) | is.nan(eta[[i]]) | is.infinite(eta[[i]])}
#     # new_bad_vals <- sum(bad_values)
#     #
#     # # Print update of values removed due to problems with muw/eta.
#     # muw_eta_removed <- new_bad_vals - old_bad_vals
#     # if (muw_eta_removed > 0) {
#     #     cat(paste0("\nWarning: ",muw_eta_removed," pixels outside acceptable range for muw and eta LUT.\n",
#     #         "eta LUT range: ",(min(coefs[["eta"]]) - max_eta_diff)," - ",(max(coefs[["eta"]]) + max_eta_diff),"\n",
#     #         "muw LUT range: ",(min(coefs[["muw"]]) - max_muw_diff)," - ",(max(coefs[["muw"]]) + max_muw_diff),"\n\n"))
#     # }
#     #
#     # if (length(eta[[1]])==sum(bad_values)) {
#     #     cat(paste0("\nWarning: ",muw_eta_removed," pixels outside acceptable ",
#     #                "range for muw and eta LUT.\nZero valid pixels remaining. Skipping file ",
#     #                fname,"\n\n"))
#     #     next
#     # }
#     #
#     # # If muw values are outside the range of the LUT muws but still close, convert them to the min (or max) LUT values.
#     # # This is necessary to make the 2D interpolation work - if they're outside the range, they automatically become NA.
#     # # This might convert the full image to a single value (if all are outside the acceptable range).
#     # new_muw <- muw
#     # new_muw[(new_muw < 0.712903) & (new_muw >= (0.712903 - max_muw_diff))] <- 0.712903
#     # new_muw[(new_muw > 1) & (new_muw <= (1 + max_muw_diff))] <- 1
#     # new_muw[bad_values] <- NA
#     #
#     # # Same as above, but with the eta values from each wavelength.
#     # new_eta <- lapply(1:length(lambda),function(i) {eta[[i]][eta[[i]] < 0] <- 0; eta[[i]]})
#     # new_eta <- lapply(1:length(lambda),function(i) {eta[[i]][(eta[[i]] > 0.2) & (eta[[i]] <= (0.2 + max_eta_diff))] <- 0.2; eta[[i]]})
#     # new_eta <- lapply(1:length(lambda),function(i) {eta[[i]][bad_values] <- NA; eta[[i]]})
#     #
#     # # 2D interpolation of coefficients based on eta and muw values.
#     # eta_coefs <- unique(coefs[,"eta"])
#     # muw_coefs <- coefs[rev(1:8),"muw"]
#     # c1 <- lapply(new_eta, FUN=get_coefs, yp=new_muw, x=eta_coefs, y=muw_coefs, Z=c1_mat, NA_ind=bad_values)
#     # c2 <- lapply(new_eta, FUN=get_coefs, yp=new_muw, x=eta_coefs, y=muw_coefs, Z=c2_mat, NA_ind=bad_values)
#     # c3 <- lapply(new_eta, FUN=get_coefs, yp=new_muw, x=eta_coefs, y=muw_coefs, Z=c3_mat, NA_ind=bad_values)
#     #
#     # # Flag other variables by bad_values
#     # new_Rrs <- lapply(1:length(lambda), function(i) {Rrs[[i]][bad_values] <- NA; Rrs[[i]]})
#     # new_kd <- lapply(1:length(lambda), function(i) {kd[[i]][bad_values] <- NA; kd[[i]]})
#     #
#     # # Total backscattering
#     # factor <- lapply(1:length(lambda),function(i) {c1[[i]] * new_Rrs[[i]] + c2[[i]] * (new_Rrs[[i]])^2 + c3[[i]] * (new_Rrs[[i]])^3})
#     # bb <- lapply(1:length(lambda),function(i) {new_kd[[i]] * factor[[i]]})
#     #
#     # # Remove problem values... when plotting bb for image A2003010211000.L2, there is still an "infinite" pixel after this
#     # bb <- lapply(1:length(lambda),function(i) {bb[[i]][which(is.na(bb[[i]]) | is.nan(bb[[i]]) | is.infinite(bb[[i]]),arr.ind=T)] <- NA; bb[[i]]})
#     #
#     # # PLOT NEW RASTERS WITH "BAD" VALUES REMOVED
#     # #raster_plot(lambda,new_Rrs,"new_Rrs")
#     # #plot(flip(t(flip(raster(new_muw),1)),1),main="new_muw")
#     # #raster_plot(lambda,new_eta,"new_eta")
#     # #raster_plot(lambda,new_kd,"new_kd")
#     # #raster_plot(lambda,factor,"factor")
#     #
#     # # PLOT RESULTS
#     # #raster_plot(lambda,c1,"c1")
#     # #raster_plot(lambda,c2,"c2")
#     # #raster_plot(lambda,c3,"c3")
#     # #raster_plot(lambda,bb,"bb")
#     #
#     # cat("Step 1G completed.\n")
#     #
#     # # STEP 1H=======================================
#     #
#     # # Backscattering of water
#     # bbw <- 0.5 * bw
#     #
#     # # Backscattering of particulate matter
#     # bbp <- lapply(1:length(lambda),function(i) {bb[[i]] - bbw[[i]]})
#     #
#     # #***************************************************************************
#
#
#     # # NASA BBP
#     bbp <- list(bbp_412_qaa,bbp_443_qaa,bbp_488_qaa,bbp_547_qaa)
#     #bbp <- lapply(1:length(lambda), function(i) {bbp[[i]][bad_values] <- NA; bbp[[i]]})
#
#     # # Kostadinov et al (2009): remove values outside the range [10^-5 to 10^-1]
#     #bbp <- lapply(1:length(lambda),function(i) {bbp[[i]][which(bbp[[i]] < 1e-5 | bbp[[i]] > 1e-1,arr.ind=T)] <- NA; bbp[[i]]})
#     # # Or at least remove values > 10
#     bbp <- lapply(1:length(lambda),function(i) {bbp[[i]][which(is.na(bbp[[i]]) | is.nan(bbp[[i]]) | is.infinite(bbp[[i]]) | (bbp[[i]] > 0.303835) | (bbp[[i]] < 0),arr.ind=T)] <- NA; bbp[[i]]})
#
#     # PLOT RESULTS
#     #raster_plot(lambda,bbp,"bbp",lower=1e-5,upper=1e-1)
#
#     stop()
#
#     # GET BBP SPECTRAL SLOPE
#     #===========================================================================
#
#     cat("\nComputing spectral slope...\n")
#     bbp_slope <- sapply(1:length(bbp[[2]]), get_bbp_slope, bbp=bbp, lambda=lambda)
#     bbp_slope <- matrix(bbp_slope,nrow=nrow(bbp[[2]]),ncol=ncol(bbp[[2]]))
#
#     # PLOT RESULTS
#     #plot(flip(t(flip(raster(bbp_slope),1)),1),main="bbp_slope")
#
#
#
#
#     # GET PSD SLOPE
#     #===========================================================================
#
#     cat("\nComputing PSD slope...\n")
#
#     bbp_slopes <- as.numeric(ref_vals$bbp_slope)
#     psd_slopes <- as.numeric(psd_slope)
#
#     # Interpolate to find values for the necessary bbp spectral slopes.
#     final_psd <- as.numeric(approx(bbp_slopes,psd_slopes,as.vector(bbp_slope))$y)
#     # Reshape the result.
#     final_psd <- matrix(final_psd,nrow=nrow(bbp[[2]]),ncol=ncol(bbp[[2]]))
#
#     # PLOT RESULTS
#     #plot(flip(t(flip(raster(final_psd),1)),1),main="PSD_slope")
#
#
#
#     # GET No
#     #===========================================================================
#
#     cat("\nComputing No...\n")
#
#     bbp_slopes <- as.numeric(ref_vals$bbp_slope)
#     logbbp443Nos <- as.numeric(logbbp443No) #log10(bbp(443)/No)
#
#     # Interpolate to find values for the necessary bbp spectral slopes.
#     final_logbbp443No <- as.numeric(approx(bbp_slopes,logbbp443Nos,as.vector(bbp_slope))$y)
#
#     # col1: bbp_slope, col2: log10(bbp[[2]]/No), where bbp[[2]] = bbp443
#     No = as.vector(bbp[[2]]) / (10^as.vector(final_logbbp443No))
#
#     # Reshape the result.
#     final_No <- matrix(No,nrow=nrow(bbp[[2]]),ncol=ncol(bbp[[2]]))
#
#     # PLOT RESULTS
#     #plot(flip(t(flip(raster(final_No),1)),1),main="No")
#
#
#
#     # COMPUTE N(D), NUMBER OF PARTICLES OF DIAMETER D PER VOLUME OF SEAWATER
#     #===========================================================================
#
#     cat(paste0("\nComputing N for D = ",D,"m...\n"))
#
#     # Reference diameter Do = 2 micrometres = 2e(-6)
#     final_N <- final_No * (D / Do) ^ (-final_psd)
#
#     # PLOT RESULTS
#     #plot(flip(t(flip(raster(final_N),1)),1),main="N")
#
#
#
#
#
#     # # JPG-IFY INDIVIDUAL STEPS FOR ANALYSIS
#     # #===========================================================================
#     #
#     # # Single plots
#     # make_jpg(fname,chlor_a,out_path_nc,"chlor_a",lower=2,upper=6)
#     # make_jpg(fname,solz,out_path_nc,"solz")
#     # make_jpg(fname,muw,out_path_nc,"muw")
#     # make_jpg(fname,q,out_path_nc,"q",lower=0.6,upper=0.9)
#     # make_jpg(fname,final_N,out_path_nc,"N")
#     # make_jpg(fname,final_No,out_path_nc,"No")
#     # make_jpg(fname,final_psd,out_path_nc,"psd_slope")
#     # make_jpg(fname,bbp_slope,out_path_nc,"bbp_slope",lower=-3,upper=0)
#     # make_jpg(fname,new_muw,out_path_nc,"new_muw")
#     # make_jpg(fname,bp550,out_path_nc,"bp550")
#     # make_jpg(fname,vc,out_path_nc,"vc")
#     #
#     # # Plots by wavelength
#     # # Which wavelength (options: 412, 443, 488, 547)? 3rd = 488nm
#     # wv <- 3
#     #
#     # make_jpg(fname,Rrs[[wv]],out_path_nc,paste0("Rrs",lambda[wv]))
#     # make_jpg(fname,kd_exp[[wv]],out_path_nc,paste0("kd_exp",lambda[wv]))
#     # make_jpg(fname,kd[[wv]],out_path_nc,paste0("kd",lambda[wv]),lower=0.5,upper=2.5)
#     # make_jpg(fname,bbp[[wv]],out_path_nc,paste0("bbp",lambda[wv]),lower=0,upper=1)
#     # make_jpg(fname,c1[[wv]],out_path_nc,paste0("c1",lambda[wv]))
#     # make_jpg(fname,c2[[wv]],out_path_nc,paste0("c2",lambda[wv]))
#     # make_jpg(fname,c3[[wv]],out_path_nc,paste0("c3",lambda[wv]))
#     # make_jpg(fname,bb[[wv]],out_path_nc,paste0("bb",lambda[wv]))
#     # make_jpg(fname,new_Rrs[[wv]],out_path_nc,paste0("new_Rrs",lambda[wv]))
#     # make_jpg(fname,new_eta[[wv]],out_path_nc,paste0("new_eta",lambda[wv]))
#     # make_jpg(fname,new_kd[[wv]],out_path_nc,paste0("new_kd",lambda[wv]))
#     # make_jpg(fname,factor[[wv]],out_path_nc,paste0("factor",lambda[wv]))
#     # make_jpg(fname,eta[[wv]],out_path_nc,paste0("eta",lambda[wv]))
#     # make_jpg(fname,bp[[wv]],out_path_nc,paste0("bp",lambda[wv]))
#     #
#     # #===========================================================================
#
#
#
#
#     # CREATE OUTPUT FOR THIS FILE
#     #===========================================================================
#
#     cat("\nWriting output to csv...\n")
#
#     bbp_mat <- cbind(as.vector(bbp[[1]]),as.vector(bbp[[2]]),
#                      as.vector(bbp[[3]]),as.vector(bbp[[4]]),
#                      as.vector(bbp_slope),as.vector(final_psd),
#                      as.vector(final_No),as.vector(final_N))
#     colnames(bbp_mat) <- c(paste0("bbp",lambda[[1]]),paste0("bbp",lambda[[2]]),
#                            paste0("bbp",lambda[[3]]),paste0("bbp",lambda[[4]]),
#                            "bbp_slope","PSD_slope","No","N")
#     # Omit rows containing any NA.
#     bbp_mat <- na.omit(bbp_mat)
#     write.csv(bbp_mat,paste0(out_path_csv,fname,"_particle-size-",D,"_bbpqaa.csv"),row.names=F)
#
#     # cat("\nWriting maps to NetCDF...\n\n")
#     #
#     # # The code below orients the raster the right way and writes lat and lon as
#     # # separate variable layers. There must be an easier way to assign lat/lon
#     # # to each bbp cell instead...
#     # bbp443_rast <- flip(t(flip(raster(bbp[[2]]),1)),1)
#     # bbp_slope_rast <- flip(t(flip(raster(bbp_slope),1)),1)
#     # final_psd_rast <- flip(t(flip(raster(final_psd),1)),1)
#     # final_No_rast <- flip(t(flip(raster(final_No),1)),1)
#     # final_N_rast <- flip(t(flip(raster(final_N),1)),1)
#     # latrast <- flip(t(flip(raster(l2_lat),1)),1)
#     # lonrast <- flip(t(flip(raster(l2_lon),1)),1)
#     #
#     # # Stack layers and write to NetCDF.
#     # total <- stack(bbp443_rast,bbp_slope_rast,final_psd_rast,final_No_rast,
#     #                final_N_rast,latrast,lonrast)
#     # names(total) <- c('bbp443','bbp_slope','psd_slope','No','N','latitude','longitude')
#     # writeRaster(total, filename=paste0(out_path_nc,fname,"_",atmcor,
#     #             "_particle-size-",D,"_bbpqaa.nc"),format="CDF",
#     #             overwrite=T, datatype="FLT4S",NAflag=-32767)
#
# }
