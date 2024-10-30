#' asinh ggplot scale transformation
#'
#' This can be used to do an arcsinh transformation of a ggplot scale, similar to the log transformation (but with the ability to handle values <= 0). To do a sinh transformation, use "sinh" instead of "asinh" (see ?sinh_trans).
#'
#' @examples
#' library(ggplot2)
#' ggplot(data.frame(x=1:10,y=1:10), aes(x=x,y=y)) +
#'     geom_point() +
#'     scale_y_continuous(trans="asinh")
#'
#' @export
asinh_trans <- function() {
    # Source: https://stackoverflow.com/questions/14504869/histogram-with-negative-logarithmic-scale-in-r
    scales::trans_new(name = "asinh", transform = function(x) asinh(x), inverse = function(x) sinh(x))
}


#' sinh ggplot scale transformation
#'
#' This can be used to do an sinh transformation of a ggplot scale. To do an asinh transformation, use "asinh" instead of "sinh" (see ?asinh_trans).
#'
#' @examples
#' library(ggplot2)
#' ggplot(data.frame(x=1:10,y=1:10), aes(x=x,y=y)) +
#'     geom_point() +
#'     scale_y_continuous(trans="sinh")
#'
#' @export
sinh_trans <- function() {
    scales::trans_new(name = "sinh", transform = function(x) sinh(x), inverse = function(x) asinh(x))
}


#' Plot a georeferenced raster on a map
#'
#' Plot a raster on a map. If a RasterStack is provided instead, facet_wrap will be used to plot the raster layers on separate maps, and nrow specifies the number of rows of maps (note: they will be plotted by row rather than by column). The names of the raster layers in the stack will be used as titles, and the maps will all use the same color scale.
#'
#' @param rast Georeferenced RasterLayer or RasterStack
#' @param title Optional title of the map
#' @param xlim Longitude limits. Set to NULL to let the function decide.
#' @param ylim Latitude limits. Set to NULL to let the function decide.
#' @param xbreaks x-axis (longitude) breaks/tick positions. Set to NULL to let the function decide.
#' @param ybreaks x-axis (latitude) breaks/tick positions. Set to NULL to let the function decide.
#' @param xlabs x-axis (longitude) labels to use. Set to NULL to let the function decide.
#' @param ylabs y-axis (latitude) labels to use. Set to NULL to let the function decide.
#' @param sec.axis.x Function to define secondary axis on opposite side of plot from x axis.
#' @param sec.axis.y Function to define secondary axis on opposite side of plot from y axis.
#' @param col_limits Color scale limits
#' @param cm Color scale
#' @param set_extremes TRUE/FALSE, should values outside the range in col_limits be set to the min/max? If not, they will be transparent. Ignored if col_limits=NULL
#' @param na.value Color to use for NA values in raster
#' @param rast_alpha Transparency of the raster on the map, 0-1
#' @param map_alpha Transparency of landmasses on map, 0-1
#' @param map_fill Colour of the landmasses in the map
#' @param map_colour Colour of the outline of the landmasses in the map
#' @param nrow Number of rows of plots, for raster stacks
#' @param show_legend Display the raster legend next to the map?
#' @param hires_land TRUE/FALSE, use the high resolution "world2Hires" land map or lower resolution "world" map?
#' @param ... Extra arguments to scale_fill_gradientn()
#' @return Raster or grid of rasters on maps with coastlines.
#' @import ggplot2
#' @importFrom magrittr "%>%"
#' @examples
#' library(terra)
#' library(ggplot2)
#' # use SGLI L2 data and make a terra raster
#' data("example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000")
#' pts <- example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000
#' rast <- rasterize(as.matrix(pts[,1:2]), rast(extent=terra::ext(c(range(pts$lon),range(pts$lat)))), pts$chl, fun=mean, na.rm=TRUE)
#' # plot it on the map
#' make_raster_map(rast,title=NULL,xlim=c(-95,-42),ylim=c(39,82),trans="log10") +
#'     theme(axis.title=element_blank(),
#'           legend.title=element_blank(),
#'           legend.margin=margin(0,0,0,0),
#'           legend.box.margin=margin(-10,0,-10,-10)) +
#'     guides(fill = guide_colourbar(title.hjust = 0,
#'                                   ticks.colour = "black",
#'                                   barwidth = unit(0.6, "cm"),
#'                                   barheight = unit(8, "cm"),
#'                                   frame.colour = "black"))
#' @export
make_raster_map <- function(rast,title=NULL,xlim=NULL,ylim=NULL,xbreaks=NULL,ybreaks=NULL,xlabs=NULL,ylabs=NULL,sec.axis.x=NULL,sec.axis.y=NULL,col_limits=NULL,cm=colorRampPalette(c("#00007F","blue","#007FFF","cyan","#7FFF7F","yellow","#FF7F00","red","#7F0000"))(100),set_extremes=FALSE,na.value="transparent",rast_alpha=1,map_alpha=0.8,map_fill="grey",map_colour="darkgrey",nrow=1,show_legend=TRUE,hires_land=FALSE,...) {
    stopifnot(class(rast) %in% c("RasterBrick","RasterStack","RasterLayer","SpatRaster"))
    if (hires_land) {
        data(world2HiresMapEnv, package="mapdata")
        worldmap <- ggplot2::map_data("world2Hires") %>% dplyr::mutate(long=-1*(360-long))
        regs <- worldmap %>% dplyr::filter(between(long,xlim[1],xlim[2]) & between(lat,ylim[1],ylim[2])) %>% dplyr::distinct(region)
        # France is broken in this map
        worldmap <- worldmap %>% dplyr::filter(region %in% regs$region & region!="France")
    } else {
        worldmap <- ggplot2::map_data("world")
    }
    if (is.null(xlabs)) {xlabs <- waiver()}
    if (is.null(ylabs)) {ylabs <- waiver()}
    if (is.null(xbreaks)) {xbreaks <- waiver()}
    if (is.null(ybreaks)) {ybreaks <- waiver()}
    if (is.null(sec.axis.x)) {sec.axis.x <- waiver()}
    if (is.null(sec.axis.y)) {sec.axis.y <- waiver()}
    if (class(rast) %in% c("RasterBrick","RasterStack","RasterLayer")) {rast <- terra::rast(rast)}
    num_layers <- dim(rast)[3]
    if (any(duplicated(names(rast)))) {
        nr <- data.frame(value=names(rast)) %>%
            dplyr::group_by(value) %>%
            dplyr::mutate(serial_number=1:n()) %>%
            dplyr::ungroup() %>%
            tidyr::unite(col="name",value,serial_number,sep="") %>%
            unlist()
        names(rast) <- nr
    }
    if (!is.null(col_limits) & set_extremes) {
        rast[rast < col_limits[1]] <- col_limits[1]
        rast[rast > col_limits[2]] <- col_limits[2]
    }
    p <- ggplot() +
        tidyterra::geom_spatraster(data=rast, show.legend=show_legend, alpha=rast_alpha, maxcell=terra::ncell(rast)) +
        geom_map(data=worldmap, map=worldmap,
                 aes(x=long, y=lat, group=group, map_id=region),
                 fill=map_fill, colour=map_colour, linewidth=0.5, alpha=map_alpha) +
        theme_bw() +
        theme(axis.title=element_blank(),
              plot.title=element_text(hjust=0.5)) +
        scale_fill_gradientn(colours=cm, limits=col_limits, na.value=na.value, ...) +
        scale_x_continuous(limits=xlim, breaks=xbreaks, labels=xlabs, expand=c(0, 0), sec.axis=sec.axis.x) +
        scale_y_continuous(limits=ylim, breaks=ybreaks, labels=ylabs, expand=c(0, 0), sec.axis=sec.axis.y) +
        ggtitle(title)
    if (num_layers > 1) {
        p <- p + facet_wrap(~lyr, nrow=nrow)
    }
    return(p)
}

