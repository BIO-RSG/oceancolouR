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
#' @param xlim Longitude limits
#' @param ylim Latitude limits
#' @param xlabs x-axis (longitude) labels to use. Set to NULL to let the function decide.
#' @param ylabs y-axis (latitude) labels to use. Set to NULL to let the function decide.
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
#' @param ... Extra arguments to scale_fill_gradientn()
#' @return Raster or grid of rasters on maps with coastlines.
#' @import ggplot2
#' @examples
#' # use SGLI L2 data
#' library(raster)
#' library(sp)
#' # create a raster
#' data("example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000")
#' pts <- example02_GC1SG1_202109031518L33309_L2SG_IWPRK_2000
#' coordinates(pts) = ~lon+lat
#' tr <- rasterize(pts, raster(ext=extent(pts)), pts$chl, fun = mean, na.rm = TRUE)
#' # plot it on the map
#' make_raster_map(log10(tr),title=NULL)
#'
#' @export
make_raster_map <- function(rast,title=NULL,xlim=c(-95,-42),ylim=c(39,82),xlabs=NULL,ylabs=NULL,col_limits=NULL,cm=colorRampPalette(c("#00007F","blue","#007FFF","cyan","#7FFF7F","yellow","#FF7F00","red","#7F0000"))(100),set_extremes=FALSE,na.value="transparent",rast_alpha=1,map_alpha=0.8,map_fill="white",map_colour="#7f7f7f",nrow=1,show_legend=TRUE,...) {
    stopifnot(class(rast) %in% c("RasterStack","RasterLayer"))
    worldmap <- ggplot2::map_data("world")
    if (!is.null(col_limits)) {
        if (set_extremes) {
            if (raster::nlayers(rast) == 1) {
                rast[rast < col_limits[1]] <- col_limits[1]
                rast[rast > col_limits[2]] <- col_limits[2]
            } else {
                rast <- lapply(1:(raster::nlayers(rast)),
                               function(lx)
                                   {r <- rast[[lx]]; r[r<col_limits[1]] <- col_limits[1]; r[r>col_limits[2]] <- col_limits[2]; r})
                rast <- raster::stack(rast)
            }
        }
        colscale <- scale_fill_gradientn(colours = cm, limits=col_limits, na.value=na.value, ...)
    } else {
        colscale <- scale_fill_gradientn(colours = cm, na.value=na.value, ...)
    }
    p <- rasterVis::gplot(rast) +
        geom_tile(aes(fill = value), show.legend=show_legend, alpha=rast_alpha) +
        geom_map(data = worldmap, map = worldmap,
                 aes(x = long, y = lat, group = group, map_id=region),
                 fill = map_fill, colour = map_colour, size=0.5, alpha=map_alpha) +
        scale_x_continuous(limits=xlim,expand=c(0,0)) +
        scale_y_continuous(limits=ylim,expand=c(0,0)) +
        coord_fixed(1.5) +
        theme_bw() +
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.title=element_blank(),
              legend.margin=margin(0,0,0,0),
              legend.box.margin=margin(-10,0,-10,-10),
              plot.title=element_text(hjust=0.5)) +
        guides(fill = guide_legend(ticks.colour = "black")) +
        colscale +
        ggtitle(title)
    if (is.null(xlabs)) {
        p <- p + scale_x_continuous(limits = xlim, expand = c(0, 0))
    } else {
        p <- p + scale_x_continuous(limits = xlim, breaks = xlabs, labels = xlabs, expand = c(0, 0))
    }
    if (is.null(ylabs)) {
        p <- p + scale_y_continuous(limits = ylim, expand = c(0, 0))
    } else {
        p <- p + scale_y_continuous(limits = ylim, breaks = ylabs, labels = ylabs, expand = c(0, 0))
    }
    if (class(rast)=="RasterStack") {
        p <- p + facet_wrap(~ variable, nrow=nrow)
    }
    return(p)
}

