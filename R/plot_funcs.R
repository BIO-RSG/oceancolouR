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
#' @param rast Georeferenced raster
#' @param title Optional title of the map
#' @param xlim Longitude limits
#' @param ylim Latitude limits
#' @param col_limits Color scale limits
#' @param cm Color scale
#' @return Raster containing variable values with coastlines.
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
make_raster_map <- function(rast,title=NULL,xlim=c(-95,-42),ylim=c(39,82),col_limits=NULL,cm=colorRampPalette(c("#00007F","blue","#007FFF","cyan","#7FFF7F","yellow","#FF7F00","red","#7F0000"))(100)) {
    worldmap <- map_data("world")
    p <- rasterVis::gplot(rast) +
        geom_tile(aes(fill = value)) +
        geom_map(data = worldmap, map = worldmap,
                 aes(x = long, y = lat, group = group, map_id=region),
                 fill = "white", colour = "#7f7f7f", size=0.5) +
        scale_x_continuous(limits=xlim,expand=c(0,0)) +
        scale_y_continuous(limits=ylim,expand=c(0,0)) +
        ggtitle(title) +
        coord_fixed(1.5) +
        theme_bw() +
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.title=element_blank(),
              legend.margin=margin(0,0,0,0),
              legend.box.margin=margin(-10,0,-10,-10),
              plot.title=element_text(hjust=0.5)) +
        guides(fill = guide_colorbar(ticks.colour = "black"))
    if (!is.null(col_limits)) {
        p <- p + scale_fill_gradientn(colours = cm, limits=col_limits)
    } else {
        p <- p + scale_fill_gradientn(colours = cm)
    }
    return(p)
}

