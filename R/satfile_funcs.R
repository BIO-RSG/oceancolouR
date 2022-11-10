#' Get L2 / L3 filenames
#'
#' Query the CMR Search site for files falling in a specific area. Most codes should work. A few useful ones:
#' * MODISA_L2_OC (_IOP, _SST)
#' * OLCIS3A_L2_EFR_OC (S3A_) (_ERR_) (_IOP, _OC_NRT, _IOP_NRT)
#' * VIIRSN_L2_OC (VIIRSJ1_) (_IOP, _SST)
#' * SEAWIFS_L2_OC (_IOP)
#' * CZCS_L2_OC
#' * MODISA_L3b_SST (_SST_NRT)
#' * MODISA_L3b_NSST (_NSST_NRT)
#' * MODISA_L3b_SST4 (_SST4_NRT)
#' * MODISA_L3b_CHL (_CHL_NRT)
#'
#' @param dataset Code indicating what dataset: see https://cmr.earthdata.nasa.gov/search/site/collections/directory/OB_DAAC/gov.nasa.eosdis for full list
#' @param minlat Minimum latitude (y)
#' @param maxlat Maximum latitude (y)
#' @param minlon Minimum longitude (x)
#' @param maxlon Maximum longitude (x)
#' @param mindate Earliest image as string of year or date (YYYY-MM-dd)
#' @param maxdate Latest image as string of year or date (YYYY-MM-dd)
#' @return Data.frame with metadata and URLs of files to download.
#' @export
get_imglist_multi = function(dataset, minlat, maxlat, minlon, maxlon, mindate, maxdate) {
    if(missing(mindate)) {mindate = 2022}
    if(missing(maxdate)) {maxdate = 2022}
    if(missing(minlat)) {minlat = 44.585138}
    if(missing(maxlat)) {maxlat = 44.806227}
    if(missing(minlon)) {minlon = -63.753582}
    if(missing(maxlon)) {maxlon = -63.398581}
    if(missing(dataset)) {dataset = "MODISA_L2_OC"}

    # Dates can be in format "YYYY" or "YYYY-MM-DD"
    minyear = substr(mindate, 1, 4) # Get min year value
    maxyear = substr(maxdate, 1, 4)
    if(nchar(mindate) == 4) { mindate = paste0(mindate,"-01-01") } # If no month and day given, assume whole year
    if(nchar(maxdate) == 4) { maxdate = paste0(maxdate,"-12-31") }
    # Query CMR Search for files by year
    yearlist = list()
    cy = 1
    # Loop by year
    for (i in minyear:maxyear) {
        pagenum = 1 # There are a limited number of results per page, so we loop through all pages available
        while(pagenum > 0) {
            print(paste(mindate, maxdate))
            # Get files in date range and bounding box
            url = paste0("https://cmr.earthdata.nasa.gov/search/granules.csv?provider=OB_DAAC&short_name=",dataset,"&bounding_box=",
                         minlon,",",minlat,",",maxlon,",",maxlat,"&temporal=",mindate,",",maxdate,"&page_size=2000&page_num=", pagenum)
            #print(url)
            res = httr::GET(url)
            data = (rawToChar(res$content))
            data <- unlist(strsplit(data,"\n"))
            # If there are results on the page, save to file
            if (length(data) > 1) {
                # Save to list
                cols = unlist(strsplit(data[1],","))
                test = strsplit(data[2:length(data)],",")
                test2 = do.call(rbind, test)
                t = as.data.frame(test2)
                colnames(t) = cols[1:ncol(t)]
                yearlist[[cy]] <- t
                pagenum = pagenum+1
            } else {
                pagenum = 0
            }
            cat(pagenum)
            cat("...")
        }
    }

    yearlist = do.call(rbind, yearlist)
    yearlist$`Cloud Cover` = as.numeric(yearlist$`Cloud Cover`)
    return(yearlist)

}

#' Get L1A filenames
#'
#' Query the CMR Search site for files falling in a specific area. This currently converts the L2 names to L1 as the L1 files aren't fully listed on the CMR site.
#'
#' @param sensor Currently works for options "MODISA" and "SEAWIFS"
#' @param minlat Minimum latitude (y)
#' @param maxlat Maximum latitude (y)
#' @param minlon Minimum longitude (x)
#' @param maxlon Maximum longitude (x)
#' @param mindate Earliest image as string of year or date (YYYY-MM-dd)
#' @param maxdate Latest image as string of year or date (YYYY-MM-dd)
#' @return Data.frame with metadata and URLs of files to download.
#' @export
get_imglist_l1 = function(sensor="MODISA", minlat, maxlat, minlon, maxlon, mindate, maxdate) {
    if(missing(mindate)) {mindate = 2022}
    if(missing(maxdate)) {maxdate = 2022}
    if(missing(minlat)) {minlat = 44.585138}
    if(missing(maxlat)) {maxlat = 44.806227}
    if(missing(minlon)) {minlon = -63.753582}
    if(missing(maxlon)) {maxlon = -63.398581}
    # if(sensor == "SEAWIFS") { dataset = "SEAWIFS_L2_OC"}
    if(sensor == "MODISA") {
        dataset = "MODISA_L2_OC"
    } else if(sensor == "VIIRSN") {
        dataset = "VIIRSN_L2_OC"
    } else if(sensor == "VIIRSJ") {
        dataset = "VIIRSJ1_L2_OC"
    } else {
        message("Sensor not in function")
        return(NA)
    }
    namelist = get_imglist_multi(dataset, minlat = minlat, maxlat = maxlat, minlon = minlon, maxlon = maxlon, mindate = mindate, maxdate = maxdate)
    namelist$`Granule UR` = NULL
    namelist$Size = NULL
    if(dataset == "MODISA_L2_OC") {
        namelist$`Producer Granule ID` = paste0(substr(namelist$`Producer Granule ID`, 1, nchar(namelist$`Producer Granule ID`)-13), ".L1A_LAC")
        namelist$`Online Access URLs` = paste0("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/",namelist$`Producer Granule ID`,".bz2")
    # } else if(dataset == "SEAWIFS_L2_OC") {

    } else if(dataset == "VIIRSN_L2_OC") {
        namelist$`Producer Granule ID` = paste0(substr(namelist$`Producer Granule ID`, 1, nchar(namelist$`Producer Granule ID`)-14), ".L1A_SNPP.nc")
        namelist$`Online Access URLs` = paste0("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/",namelist$`Producer Granule ID`)
    } else if(dataset == "VIIRSJ1_L2_OC") {
        namelist$`Producer Granule ID` = paste0(substr(namelist$`Producer Granule ID`, 1, nchar(namelist$`Producer Granule ID`)-15), ".L1A_JPSS1.nc")
        namelist$`Online Access URLs` = paste0("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/",namelist$`Producer Granule ID`)
    }

    return(namelist)
}

#' Download ocean colour files
#'
#' Download file lists from functions get_imglist_multi() and get_imglist_l1()
#'
#' Uses wget (works on Mac, Linux, or Cygwin from Windows), and you need to have the OBPG .netrc credentials, i.e. you need to create an Earthdata account, and then type the following on the command line, replacing USERNAME and PASSWD with your own username and password:
#'
#'   echo "machine urs.earthdata.nasa.gov login USERNAME password PASSWD" > ~/.netrc ; > ~/.urs_cookies
#'
#'   chmod 0600 ~/.netrc
#'
#' See "Download Methods" here for more details: https://oceancolor.gsfc.nasa.gov/data/download_methods/
#'
#' @param filenames Filename to download
#' @param urls URLs of files corresponding to filenames
#' @return Data.frame with metadata and URLs of files to download.
#' @examples
#' filename = "A2003001162000.L2_LAC_OC.nc"
#' url = "https://oceandata.sci.gsfc.nasa.gov/cmr/getfile/A2003001162000.L2_LAC_OC.nc"
#' download_oc(filename, url)
#' @export
download_oc = function(filenames, urls) {
    if(length(filenames) != length(urls)) {
        print("Filenames not same length as URLs")
    } else {
        message(paste("Downloading",length(filenames)))
        for(i in 1:length(urls)) {
            dl_command = paste0("wget -q --show-progress --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --auth-no-challenge=on --content-disposition ",urls[i])
            system(dl_command)
            # unzip_command = paste0("bunzip2 ",filenames[i],".bz2")
            # system(unzip_command)
        }
    }
}

#' Read h5
#'
#' Read h5 level-3 binned file contents.
#'
#' @param h5_file String, h5 filename.
#' @param var_name String, name of variable to extract (example: "chlor_a").
#' @return List containing the contents of the file, attributes, selected variable, bin list, and bin index.
#' @export
read_h5_L3b <- function(h5_file, var_name) {

    h5 <- hdf5r::H5File$new(h5_file, mode="r")
    h5_ls <- h5$ls(recursive=TRUE) # view contents
    h5_att <- hdf5r::h5attributes(h5)
    output_var <- h5[[paste0("level-3_binned_data/", var_name)]][]
    bin_list <- h5[["level-3_binned_data/BinList"]][]
    BinIndex <- h5[["level-3_binned_data/BinIndex"]][]
    h5$close_all()

    return(list(h5_ls = h5_ls,
                h5_att = h5_att,
                output_var = output_var,
                bin_list = bin_list,
                BinIndex = BinIndex))

}


#' Get flag data
#'
#' Given a number from a matrix of quality flags in a satellite file that uses the bit string system for flags, and the dataframe of all flag bits/names/descriptions, get the descriptions of the flags used by that specific number.
#'
#' @param num Number in the flag matrix
#' @param flag_df Dataframe containing bits and their corresponding info, in order
#' @return Subsetted flag_df containing only the info of the flags contained within the "num" value
#' @examples
#' # load example SGLI dataset containing real data, their quality flags, and flag descriptions
#' data("example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000")
#' data("sgli_flag_df")
#' # look at the descriptions of the flags
#' sgli_flag_df
#' # get the flag value of the first pixel
#' pixel1_flag <- example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000$flags[1]
#' pixel1_flag
#' # now plug it into the function to see which flags are used in this value
#' get_flag_data(num=pixel1_flag,flag_df=sgli_flag_df)
#'
#' @export
get_flag_data <- function(num,flag_df) {
    flag_df[as.logical(intToBits(num)),]
}


#' Separate flag masks
#'
#' Given a vector or matrix of quality flags using the bit string system for flags, and the indices of the bits to use (starting at 1, for bit0), separate the flag mask into individual flags.
#'
#' @param flags Matrix or vector of flags
#' @param which_bits Bit masks to retrieve, starting at 0 (bit 0 value = 2^0 = 1)
#' @return List, in the same order as which_bits, where each list item is a logical matrix or vector (same size as "flags") that contains only the information for a particular bit
#' @examples
#' # load example SGLI dataset containing real data, their quality flags, and flag descriptions
#' data("example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000")
#' data("sgli_flag_df")
#' flag_dat <- example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000$flags
#' # subset to see a quick example
#' flag_dat <- flag_dat[1:100]
#' # bits to retrieve
#' bits <- c(0:5,8)
#' bit_names <- sgli_flag_df$name[sgli_flag_df$bit %in% bits]
#' # try splitting flags in vector format
#' sfl <- separate_flags(flag_dat, bits)
#' sfl <- do.call(cbind,sfl)
#' colnames(sfl) <- bit_names
#' sfl
#' # now try it in matrix format
#' flag_dat <- matrix(flag_dat,nrow=10)
#' sfl <- separate_flags(flag_dat, bits)
#' # plot the STRAYLIGHT flag
#' names(sfl) <- bit_names
#' raster::spplot(raster::raster(sfl$STRAYLIGHT))
#'
#' @export
separate_flags <- function(flags, which_bits) {
    stopifnot(class(flags)[1] %in% c("matrix","integer","numeric"))
    flag_bits <- lapply(as.numeric(flags),intToBits)
    flag_bits <- matrix(do.call(rbind,flag_bits)[,which_bits+1],nrow=length(flag_bits))
    flag_bits <- matrix(as.logical(flag_bits),nrow=nrow(flag_bits))
    if (class(flags)[1]=="matrix") {
        flag_bits <- lapply(1:ncol(flag_bits),function(x) matrix(flag_bits[,x],nrow=nrow(flags)))
    } else {
        flag_bits <- lapply(1:ncol(flag_bits),function(x) as.logical(flag_bits[,x]))
    }
    names(flag_bits) <- which_bits
    return(flag_bits)
}


#' Get percent coverage of netCDF satellite image
#'
#' Given a filename, W/E/S/N boundaries, and a variable name, check the percent coverage for that variable within the boundaries.
#'
#' @param file String, netCDF filename
#' @param w,e,s,n Numeric values, west/east/south/north in decimal degrees
#' @param var String, name of variable to check for percent coverage (full path to the variable within the netCDF, e.g. geophysical_data/Rrs_555)
#' @param latvar String, full path and name of the latitude variable
#' @param lonvar String, full path and name of the longitude variable
#' @return Dataframe containing filename, number of valid pixels, total number of pixels, and percent coverage. If file can't be read, "try-error" is returned instead.
#' @export
nc_image_stats <- function(file, w, e, s, n, var="geophysical_data/Rrs_555", latvar="navigation_data/latitude", lonvar="navigation_data/longitude") {
    ncfile <- try({
        nc <- ncdf4::nc_open(file)
        latitude <- ncdf4::ncvar_get(nc,latvar)
        longitude <- ncdf4::ncvar_get(nc,lonvar)
        rvar <- ncdf4::ncvar_get(nc,var)
        ncdf4::nc_close(nc)
    }, silent=TRUE)
    if (class(ncfile)=="try-error") return(ncfile)
    reg_ind <- longitude >= w & longitude <= e & latitude >= s & latitude <= n
    nbvalpxl <- sum(is.finite(rvar) & reg_ind, na.rm=TRUE)
    nbpxltot <- sum(reg_ind, na.rm=TRUE)
    perccov <- nbvalpxl/nbpxltot*100
    data.frame(file=file, nbvalpxl=nbvalpxl, nbpxltot=nbpxltot, perccov=perccov,
               stringsAsFactors = FALSE)
}


#' Extract the edges of a matrix
#'
#' Get the edge values of a matrix, starting at the top left and moving clockwise.
#'
#' @param mat Matrix
#' @return Vector of edge values
#' @export
get_edges <- function(mat) {
    nrm <- nrow(mat)
    ncm <- ncol(mat)
    betweenrows <- 2:(nrm-1)
    edges <- c(mat[1,],
               mat[betweenrows,ncm],
               rev(mat[nrm,]),
               rev(mat[betweenrows,1]))
    return(edges)
}


#' Plot satellite swath boundary
#'
#' @param lon,lat 2D numeric matrices, same size, containing the coordinates for the corresponding matrix containing the satellite data
#' @param color Color of the swath boundary
#' @param size Width of the swath outline
#' @param mapfill Color of the continents on the map
#' @param mapcolor Color of the outlines of countries on the map
#' @param mapalpha Transparency of the continents on the map
#' @return Map with a polygon showing the satellite swath boundary
#' @examples
#' # load the coordinate data from an example GCOM-C SGLI satellite file
#' data("example03_GC1SG1_201801031430Q31808_L2SG_IWPRK_3000")
#' str(longitude)
#' str(latitude)
#' plot_swath(lon=longitude, lat=latitude)
#' @export
plot_swath <- function(lon, lat, color="red", size=2, mapfill="darkgreen", mapcolor="#444444", mapalpha=0.5) {
    worldmap <- rnaturalearth::ne_countries(scale="small", returnclass="sf")
    df <- data.frame(lat=get_edges(lat), lon=get_edges(lon))
    p <- ggplot2::ggplot() +
        ggplot2::geom_sf(data=worldmap, fill=mapfill, color=mapcolor, alpha=mapalpha) +
        ggplot2::geom_polygon(data=df, ggplot2::aes(x=lon, y=lat), fill=NA, color=color, size=size) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.title=ggplot2::element_blank())
    return(p)
}

