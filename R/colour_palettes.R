#' Colour Palettes
#'
# --------------------------------------
#'
#' GMT Colour palettes
#'
#' These are a few of the standard palettes included in GMT. See: https://docs.generic-mapping-tools.org/6.2/cookbook/cpts.html
#'
#' @references
#'
#' GMT 6: Wessel, P., Luis, J. F., Uieda, L., Scharroo, R., Wobbe, F., Smith, W. H. F., & Tian, D. (2019).
#' The Generic Mapping Tools version 6. Geochemistry, Geophysics, Geosystems, 20, 5556-5564.
#' https://doi.org/10.1029/2019GC008515
#'
#'
#' @param n Number of colours to return
#' @return A vector of hexadecimal colours (#RRGGBB)
#' @example
#'
#' thirtycols <- drywet(30)
#'
#' library(ggplot2)
#' ggplot(data = reshape2::melt(volcano), aes(x = Var1, y = Var2, fill = value)) +
#' geom_tile() +
#' scale_fill_gradientn(colours = gmt_drywet(20))
#'
#' @name gmt_palettes
NULL

#' @export
gmt_drywet <- function(n = 25) {
  drywet <- rev(rgb(r = c(134,238,180,50,12,38,8),
                    g = c(97,199,238,238,120,1,51),
                    b = c(42,100,135,235,238,183,113),
                    maxColorValue = 255))
  colorRampPalette(drywet)(n)
}

#' @export
gmt_haxby <- function(n = 25) {
  message("Recommended palette limits: 0, 32")
  haxby <- rgb(r = c(10,40,20,0,0,0,26,13,25,50,68,97,106,124,138,172,205,223,240,247,255,255,244,238,255,255,255,245,255,255,255,255),
               g = c(0,0,5,10,25,40,102,129,175,190,202,225,235,235,236,245,255,245,236,215,189,160,117,80,90,124,158,179,196,215,235,255),
               b = c(121,150,175,200,212,224,240,248,255,255,255,240,225,200,174,168,162,141,121,104,87,69,75,78,90,124,158,174,196,215,235,255),
               maxColorValue = 255)
  colorRampPalette(haxby)(n)
}

#' @export
gmt_jet <- function(n = 25) {
  message("Recommended palette limits: 0, 1")
  jet <- rgb(r = c(0,0,0,255,255,255,255,127),
             g = c(0,0,255,255,255,255,0,0),
             b = c(127,255,255,255,127,0,0,0),
             maxColorValue = 255)
  colorRampPalette(jet)(n)
}

#' @export
gmt_no_green <- function(n = 25) {
  message("Recommended palette limits: -32, 32")
  no_green <- rgb(r = c(32,32,32,0,42,85,127,170,255,255,255,255,255,255,255,255),
                  g = c(96,159,191,207,255,255,255,255,255,240,191,168,138,112,77,0),
                  b = c(255,255,255,255,255,255,255,255,84,0,0,0,0,0,0,0),
                  maxColorValue = 255)
  colorRampPalette(no_green)(n)
}

#' @export
gmt_ocean <- function(n = 25) {
  message("Recommended palette limits: -8000, 0")
  ocean <- rgb(r = c(0,0,0,0,0,86,172,211,250),
               g = c(0,5,10,80,150,197,245,250,255),
               b = c(0,25,50,125,200,184,168,211,255),
               maxColorValue = 255)
  colorRampPalette(ocean)(n)
}

#' @export
gmt_polar <- function(n = 25) {
  message("Recommended palette limits: -1, 1")
  polar <- rgb(r = c(0,255,255),
                  g = c(0,255,0),
                  b = c(255,255,0),
                  maxColorValue = 255)
  colorRampPalette(polar)(n)
}
