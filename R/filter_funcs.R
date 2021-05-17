#' Filter in situ / satellite matchups
#'
#' Given a set of in situ / satellite matchups, filter according to the selected criteria for quality control. Square pixel boxes around a satellite pixel are collapsed into a single value to compare to the matching in situ record.
#'
#' Defaults to the criteria used in Bailey/Werdell (2006, see references) - with exceptions (see below):
#'
#' Bailey/Werdell 2006 criteria:
#'    - L2 satellite passes
#'    - flags used: land, cloud or ice, stray light, sun glint, high TOA radiance, low Lwn(555) for identifying cloud-shadowed pixels, or atmospheric correction failure
#'    - some in situ records excluded based on in situ radiances (see paper for details)
#'    - * use only the single closest satellite match to the in situ sample location
#'    - * extract 5x5 pixel box around center (matching) pixel
#'    - * maximum allowed time difference between satellite pass and in situ sampling: +/- 3 hours
#'    - * >= 50% of the pixels in the 5x5 pixel box must be valid
#'    - ** median CV of 5x5 box of Lwn (not Rrs) must be <= 0.15
#'          - CV = filtered_SD/filtered_mean
#'          - filtered_mean = sum(mean +/- 1.5sd)/N
#'          - N = number of pixels within +/-1.5sd of the mean
#'    - * sensor zenith angle < 60 degrees
#'    - * solar zenith angle < 75 degrees
#'    - * for 1 in situ record and >1 satellite passes: keep only the record closest in TIME
#'    - ** for >1 in situ records for 1 satellite pass, and in situ records overlap: keep only the record closest in TIME
#'
#' Details:
#'    - * applied in filter_matchups using defaults
#'    - ** applied in filter_matchups, but details are slightly different:
#'          - if selected, Rrs is filtered by CV instead of filtering Lwn by CV
#'          - some satellite passes might have >2 in situ matchups where all of them overlap; in this case, only the closest match in time is used (Bailey/Werdell compares 2 overlapping matches but it's unclear how to filter >2 matchups in this case)
#'
#' Some filters are automatically applied (not optional):
#'    - duplicate in situ IDs are removed if all other variables are the same
#'    - shallowest in situ record is used, if there are multiple records present for single datetime/lat/lon within max_depth
#'    - for 1 in situ record and 1 satellite pass with multiple matchups, only the closest is used
#'    - for 1 in situ record and >1 satellite passes, only one record is used (either closest in time or space, user-selected)
#'    - records with nonfinite chla (satellite or in situ) removed
#'    - matchups with all nonfinite Rrs removed
#'
#' df formatting requirements:
#'    - Required columns: is_chla, is_lon, is_lat, is_datetime, sat_time
#'    - In situ data columns must start with "is_", and satellite columns must start with "sat_".
#'    - Datetime column should be "is_datetime" and be in the format "YYYY-MM-DD HH:MM:SS", in UTC (or same as satellite pass timezone).
#'    - Satellite pass time column should be "sat_time" and be a numeric value representing seconds since 1970-01-01 UTC.
#'    - If in situ data records have IDs, they should be in column "is_id".
#'    - If in situ data has depth information, it should be in column "is_depth".
#
#' @param sat_rrs Named list of satellite remote sensing reflectances - each list element must be a matrix containing the reflectances for that waveband, where rows=matchups and columns=pixels (e.g. a flattened 5x5 box around the matching pixel). Names must be in the format Rrs_XXX, where XXX is the waveband in nanometres
#' @param sat_chla Matrix of satellite chla, where rows=matchups and columns=pixels (e.g. a flattened 5x5 box around the matching pixel).
#' @param sat_lat Matrix of satellite latitudes, where rows=matchups and columns=pixels (e.g. a flattened 5x5 box around the matching pixel).
#' @param sat_lon Matrix of satellite longitudes, where rows=matchups and columns=pixels (e.g. a flattened 5x5 box around the matching pixel).
#' @param df Other in situ/satellite matchup data corresponding to the 2d satellite variables above, with matchups in the same order (see details for formatting requirements)
#' @param window_size Integer, size of one side of the pixel box you want to use, must be <= input box size (e.g. if input contains Rrs matchups with 5x5 box around the center pixel, window_size can be <= 5)
#' @param rm_rrs_LE_0 TRUE/FALSE, set satellite Rrs <= 0 to NA?
#' @param rm_chla_LE_0 TRUE/FALSE, set satellite chla <= 0 to NA?
#' @param min_pix Integer, required number of valid pixels in the pixel box (if there are not enough, the match = NA)
#' @param rrsCVfilter TRUE/FALSE, filter matchups by CV (coefficient of variation) in the pixel box?
#' @param max_CV Numeric value, maximum allowed CV if rrsCVfilter=TRUE
#' @param max_sensor_zen Numeric value, maximum allowed sensor zenith angle. Set to Inf to ignore
#' @param max_solar_zen Numeric value, maximum allowed solar zenith angle. Set to Inf to ignore
#' @param rrs_window String (either median, mean, or geomean) - the statistic to use to collapse each Rrs pixel box into one value
#' @param chla_window String (either median, mean, or geomean) - the statistic to use to collapse each chla pixel box into one value
#' @param max_depth Numeric value, maximum allowed depth of in situ measurement (in metres). Set to Inf to ignore
#' @param max_timediff Numeric value, maximum allowed time difference between in situ measurement and satellite pass (in hours). Set to Inf to ignore
#' @param max_dist Numeric value, maximum allowed distance between in situ measurement and satellite pixel (in metres). Set to Inf to ignore
#' @param rm_sat_dup_by String (either timediff or dist) - if one in situ measurement matches to multiple satellite passes, should you keep the pass closest in time or in space?
#' @param rm_insitu_dup TRUE/FALSE - if one satellite pass matches to multiple in situ measurements, and pixel boxes around the measurements overlap (i.e. center pixel of one inside the pixel box of a separate measurement), should you remove all but one of the overlapping records?
#' @param rm_insitu_dup_by String (either timediff or dist) - if rm_insitu_dup=TRUE, should you choose the in situ measurement closest in time or space to the satellite pass/pixel?
#' @param rm_invalid_rrs TRUE/FALSE - remove matches with any invalid Rrs? If FALSE, only matches with no valid Rrs will be removed
#' @references
#' Bailey, Sean & Werdell, Jeremy. (2006). A multi-sensor approach for the on-orbit validation of ocean color satellite data products. Remote Sensing of Environment. 102. 12-23. 10.1016/j.rse.2006.01.015.
#' (See Fig.1)
#' @return df with added columns: satellite Rrs and chla (one Rrs and one chla pixel representing each pixel "window"), latitude/longitude of the center pixel of each pixel window, distance between in situ sample location and center pixel, and time difference between in situ sample and satellite pass.
#' @importFrom magrittr "%>%"
#' @export
filter_matchups <- function(sat_rrs, sat_chla, sat_lat, sat_lon, df,
                            # for filtering satellite values
                            window_size=5, rm_rrs_LE_0=TRUE, rm_chla_LE_0=TRUE,
                            min_pix=13, rrsCVfilter=TRUE, max_CV=0.15,
                            max_sensor_zen=60, max_solar_zen=75,
                            rrs_window="median", chla_window="median",
                            # for filtering in situ values
                            max_depth=10,
                            # for filtering matchups
                            max_timediff=3, max_dist=10000,
                            rm_sat_dup_by="timediff",
                            rm_insitu_dup=TRUE,
                            rm_insitu_dup_by="timediff",
                            rm_invalid_rrs=TRUE) {

    #***************************************************************************
    # STEP 1: run checks on input

    stopifnot(is.numeric(window_size),
              is.logical(rm_rrs_LE_0) & length(rm_rrs_LE_0)==1,
              is.logical(rm_chla_LE_0) & length(rm_chla_LE_0)==1,
              is.numeric(min_pix),
              is.logical(rrsCVfilter) & length(rrsCVfilter)==1,
              is.numeric(max_CV),
              is.numeric(max_sensor_zen),
              is.numeric(max_solar_zen),
              rrs_window %in% c("median", "mean", "geomean"),
              chla_window %in% c("median", "mean", "geomean"),
              is.numeric(max_depth),
              is.numeric(max_timediff),
              is.numeric(max_dist),
              rm_sat_dup_by %in% c("timediff", "dist"),
              is.logical(rm_insitu_dup) & length(rm_insitu_dup)==1,
              rm_insitu_dup_by %in% c("timediff", "dist"))

    input_ws <- sqrt(dim(sat_lat)[2])
    output_ws <- window_size

    # make sure the window size makes sense
    if (input_ws %% 2 != 1 | output_ws %% 2 != 1) {
        stop("Window size must be an odd value.")
    }
    if (input_ws < output_ws) {
        stop("Selected window size < length of a side of the existing window.")
    }



    #***************************************************************************
    # STEP 2: filter satellite data

    rrs_names <- names(sat_rrs)

    # if selected, remove rrs and/or chla <= 0
    if (rm_rrs_LE_0) {
        sat_rrs <- lapply(1:length(sat_rrs), function(i) {sat_rrs[[i]][sat_rrs[[i]] <= 0] <- NA; sat_rrs[[i]]})
        names(sat_rrs) <- rrs_names
    }
    if (rm_chla_LE_0) {
        sat_chla[sat_chla <= 0] <- NA
    }

    # extract a single satellite rrs for each waveband and each matchup
    single_rrs <- lapply(1:length(sat_rrs), FUN=function(i) apply(sat_rrs[[i]], MARGIN=1, FUN=collapse_window, input_ws=input_ws, output_ws=output_ws, min_pix=min_pix, useCVfilter=rrsCVfilter, max_CV=max_CV, stat_to_use=rrs_window))
    sat_rrs <- do.call(cbind, single_rrs)
    colnames(sat_rrs) <- rrs_names

    # extract a single satellite chla for each matchup
    sat_chla <- apply(sat_chla, MARGIN=1, FUN=collapse_window, input_ws=input_ws, output_ws=output_ws, min_pix=min_pix, useCVfilter=FALSE, stat_to_use=chla_window)

    # add satellite rrs, chla, and lat/lon to the dataframe
    colnames(sat_lat) <- paste0("sat_lat", stringr::str_pad(1:ncol(sat_lat), width=nchar(ncol(sat_lat)), side="left", pad="0"))
    colnames(sat_lon) <- paste0("sat_lon", stringr::str_pad(1:ncol(sat_lon), width=nchar(ncol(sat_lon)), side="left", pad="0"))
    df <- cbind(df, sat_rrs, sat_chla, sat_lat, sat_lon)

    # set everything except is_datetime to numeric
    num_cols <- !(colnames(df) %in% c("is_id", "is_datetime"))
    df[,num_cols] <- lapply(df[,num_cols], as.numeric)

    # if selected, filter by sensor zenith angle and solar zenith angle
    if (is.finite(max_sensor_zen)) {
        df <- df[df$sensor_zen < max_sensor_zen,]
    }
    if (is.finite(max_solar_zen)) {
        df <- df[df$solar_zen < max_solar_zen,]
    }



    #***************************************************************************
    # STEP 3: filter in situ data

    # remove matchups where all in situ data except the ID is identical
    df <- df %>% dplyr::distinct(!!! rlang::syms(colnames(df)[colnames(df) != "is_id"]), .keep_all=TRUE)

    # remove matchups where the in situ sample was too deep
    if (is.finite(max_depth)) {
        df <- df[df$is_depth <= max_depth,]
    }

    # take the record with the shallowest in situ sample depth
    if ("is_depth" %in% colnames(df)) {
      df_colnames <- colnames(df)[!(colnames(df) %in% c("is_chla", "is_depth"))]
      df <- df %>%
        dplyr::arrange(is_depth) %>%
        dplyr::group_by(!!! rlang::syms(df_colnames)) %>%
        dplyr::summarize_all(.funs=dplyr::first) %>%
        dplyr::ungroup() %>%
        as.data.frame()
    }



    #***************************************************************************
    # STEP 4: filter matchups

    # calculate distance between satellite center pixel and in situ sample location, for filtering
    center_ind <- ceiling(input_ws^2/2)
    tmp_x <- df[,c(paste0("sat_lon",center_ind), paste0("sat_lat",center_ind))]
    tmp_y <- df[,c("is_lon", "is_lat")]
    colnames(tmp_x) <- colnames(tmp_y) <- c("longitude", "latitude")
    df$dist <- as.numeric(geodist::geodist(x=tmp_x, y=tmp_y, paired = TRUE, measure="geodesic"))

    # calculate time difference between satellite pass and in situ sampling time
    t1 <- lubridate::as_datetime(as.numeric(df$sat_time))
    t2 <- lubridate::as_datetime(df$is_datetime, tz="UTC")
    df$timediff <- as.numeric(abs(lubridate::time_length(lubridate::interval(t1, t2), unit="hour")))

    # remove matchups that are too far apart
    if (is.finite(max_dist)) {
        df <- df[df$dist <= max_dist,]
    }

    # remove matchups with time difference > timediff
    if (is.finite(max_timediff)) {
        df <- df[df$timediff <= max_timediff,]
    }

    # for one in situ record and one satellite pass with multiple matchups, reduce to the closest one
    df_colnames <- colnames(df)[startsWith(colnames(df), "is_") | colnames(df)=="sat_time"]
    df <- df %>%
        dplyr::arrange(dist) %>%
        dplyr::group_by(!!! rlang::syms(df_colnames)) %>%
        dplyr::summarize_all(.funs=dplyr::first) %>%
        dplyr::ungroup() %>%
        as.data.frame()

    # if you have one in situ record matching to multiple satellite passes,
    # use the one closest in time or space (user-selected)
    df_colnames <- colnames(df)[startsWith(colnames(df), "is_")]
    if (rm_sat_dup_by=="timediff") {
        df <- df %>% dplyr::arrange(timediff)
    } else if (rm_sat_dup_by=="dist") {
        df <- df %>% dplyr::arrange(dist)
    }
    df <- df %>%
        dplyr::group_by(!!! rlang::syms(df_colnames)) %>%
        dplyr::summarize_all(.funs=dplyr::first) %>%
        dplyr::ungroup() %>%
        as.data.frame()

    # if you have one satellite pass matching to multiple in situ records,
    # remove extra records WHERE THE WINDOW/BOX OVERLAPS, keeping only the closest
    # one in time or space (user-selected)
    if (rm_insitu_dup) {

      duplicate_passes <- duplicated(df$sat_time)
      if (sum(duplicate_passes) > 0) {

        noncenter_pixels <- (1:input_ws^2)[(1:input_ws^2) != center_ind]
        for (i in noncenter_pixels) {
          df <- df %>%
            tidyr::unite(col=!!paste0("sat_latlon", stringr::str_pad(i,nchar(ncol(sat_lat)),"left","0")),
                         matches(paste0(c("sat_lat", "sat_lon"), stringr::str_pad(i,nchar(ncol(sat_lat)),"left","0"))),
                         sep=",")
        }
        df <- df %>%
          tidyr::unite(col="center_latlon",
                       !!paste0("sat_lat", stringr::str_pad(center_ind,nchar(ncol(sat_lat)),"left","0")),
                       !!paste0("sat_lon", stringr::str_pad(center_ind,nchar(ncol(sat_lon)),"left","0")),
                       sep=",") %>%
          tidyr::pivot_longer(cols = starts_with("sat_latlon"), values_to="sat_latlon_val", names_to="sat_latlon_id") %>%
          dplyr::group_by(sat_time) %>%
          dplyr::mutate(overlap = center_latlon %in% sat_latlon_val) %>%
          dplyr::ungroup()

        df_no_overlap <- df %>% dplyr::filter(!overlap)
        df_overlap <- df %>% dplyr::filter(overlap)
        if (rm_insitu_dup_by=="timediff") {
          df_overlap <- df_overlap %>% dplyr::arrange(timediff)
        } else if (rm_insitu_dup_by=="dist") {
          df_overlap <- df_overlap %>% dplyr::arrange(dist)
        }
        df_overlap <- df_overlap %>%
          dplyr::group_by(sat_time) %>%
          dplyr::summarize_all(.funs=dplyr::first) %>%
          dplyr::ungroup()

        df <- dplyr::bind_rows(df_no_overlap, df_overlap) %>%
          tidyr::separate(col=center_latlon, into=c("center_lat", "center_lon"), sep=",") %>%
          as.data.frame() %>%
          dplyr::select(-sat_latlon_id, -sat_latlon_val, -overlap) %>%
          dplyr::distinct()

        # head(newdf[newdf$sat_time==1027531072,])
        # head(df[df$sat_time==1027531072,])

      }

    }

    # remove invalid chla
    chla_col_inds <- grepl("chla", colnames(df))
    all_finite <- apply(is.finite(as.matrix(df[,chla_col_inds])), MARGIN=1, sum)==sum(chla_col_inds)
    df <- df[all_finite,]

    # if rm_invalid_rrs, remove any matches that contain ANY invalid rrs
    # otherwise, only remove matches that contain ALL invalid rrs
    rrs_col_inds <- grepl("Rrs_[0-9]{3}$", colnames(df))
    if (rm_invalid_rrs) {
      all_finite <- apply(is.finite(as.matrix(df[,rrs_col_inds])), MARGIN=1, sum)==sum(rrs_col_inds)
      df <- df[all_finite,]
    } else {
      some_finite <- apply(is.finite(as.matrix(df[,rrs_col_inds])), MARGIN=1, sum)>0
      df <- df[some_finite,]
    }

    return(df)

}



#' Get single value from pixel window
#'
#' Given a square matrix of pixel values, return a single value representing the whole box.
#'
#' This is used when reading L2 satellite points matched to in situ data in order to get a single pixel value representing the box (e.g. 5x5 pixels) around the center pixel closest to the in situ sampling location.
#'
#' @param var Numeric vector or square matrix, length input_ws^2
#' @param input_ws Integer, length of one side of the input square pixel box
#' @param output_ws Integer, length of one side of the output square pixel box (<= input_ws)
#' @param min_pix Integer, minimum number of valid pixels required
#' @param useCVfilter TRUE/FALSE - if median CV (coefficient of variation, filteredSD/filteredMean) is > max_CV, return NA?
#' @param max_CV Maximum allowed CV
#' @param stat_to_use String, either median, mean, or geomean - the statistic used on the final filtered set of var
#' @return Single numeric value representing the input window
#' @export
collapse_window <- function(var, input_ws, output_ws, min_pix, useCVfilter=TRUE, max_CV=0.15, stat_to_use="median") {
  stopifnot(output_ws <= input_ws)
  var <- matrix(var, nrow=input_ws)
  center <- ceiling(input_ws/2)
  p1 <- center-floor(output_ws/2)
  p2 <- center+floor(output_ws/2)
  var <- as.numeric(var[p1:p2,p1:p2])
  var <- var[is.finite(var)]
  if (length(var) < min_pix) {return(NA)}
  if (useCVfilter) {
    vmean <- mean(var)
    vsd <- sd(var)
    valid_ind <- var > vmean-1.5*vsd & var < vmean+1.5*vsd
    filtered_mean <- mean(var[valid_ind])
    filtered_sd <- sd(var[valid_ind])
    CV <- filtered_sd/filtered_mean
    if (median(CV) > max_CV) {return(NA)}
  }
  if (stat_to_use=="median") {
    return(median(var))
  } else if (stat_to_use=="mean") {
    return(mean(var))
  } else if (stat_to_use=="geomean") {
    return(geoMean(var))
  }
}
