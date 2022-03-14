# This is a logo idea that has turned into a CHALLENGE because of the constraints of coding with spatial data
# - Andrea, July 15, 2021

library(hexSticker)
library(sf)
library(dplyr)
library(ggplot2)
library(raster)
library(lwgeom)
library(sp)
library(rgeos)
theme_set(theme_void())

x = seq(0,13,0.1)
y = sin(x)

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}
# Make a circle
circ <- circleFun(center = c((max(x)-min(x))/2,2), diameter = 6, npoints=100)
circle <- SpatialLines(list(
    Lines(list(Line(list(cbind(circ$x, circ$y)))), "circ")
))
# Same circle but as a polygon
poly <- SpatialPolygons(list(
    Polygons(list(Polygon(list(cbind(circ$x, circ$y)))), "circ")
))

# Here are some wavy lines
l1 = cbind(x, y)
l2 = cbind(l1[,1],l1[,2]+1)
l3 = cbind(l1[,1],l1[,2]+2)
l4 = cbind(l1[,1],l1[,2]+3)
l5 = cbind(l1[,1],l1[,2]+4)
l6 = cbind(l1[,1],l1[,2]+5)
l7 = cbind(l1[,1],l1[,2]+6)
l8 = cbind(l1[,1],l1[,2]+7)
l9 = cbind(l1[,1],l1[,2]+8)
l10 = cbind(l1[,1],l1[,2]-1)
l11 = cbind(l1[,1],l1[,2]-2)
l12 = cbind(l1[,1],l1[,2]-3)
Sl1 = Line(l1)
Sl2 = Line(l2)
Sl3 = Line(l3)
Sl4 = Line(l4)
Sl5 = Line(l5)
Sl6 = Line(l6)
Sl7 = Line(l7)
Sl8 = Line(l8)
Sl9 = Line(l9)
S10 = Line(l10)
S11 = Line(l11)
S12 = Line(l12)
S1 = Lines(list(Sl1), ID="a")
S2 = Lines(list(Sl2), ID="b")
S3  = Lines(list(Sl3), ID="c")
S4 = Lines(list(Sl4), ID="d")
S5 = Lines(list(Sl5), ID="e")
S6  = Lines(list(Sl6), ID="f")
S7  = Lines(list(Sl7), ID="g")
S8  = Lines(list(Sl8), ID="h")
S9  = Lines(list(Sl9), ID="i")
S10  = Lines(list(S10), ID="j")
S11  = Lines(list(S11), ID="k")
S12  = Lines(list(S12), ID="l")
Sl = SpatialLines(list(S1,S2, S3, S4, S5, S6, S7, S8, S9, S10,S11,S12))

# Here is a bounding box that might not be necessary:
sq <- extent(c(min(x), max(x)), c(min(y)-3, max(y)+8))


plot(Sl, col = "blue")
plot(poly, add=T)

#
# sl_sf = st_as_sf(Sl)
# poly_sf = st_as_sf(poly)
#
# t = gIntersection(poly, Sl)
#
# t = gUnion(poly_sf, sl_sf)
# t = st_intersection(sl_sf, poly_sf)
# plot(t)
# plot(t2)
#
# plot(poly, add  = T, lwd = 2)
# plot(sq, add=T)

# Now do cookie cutter on the lines with the circle
# intersected <- raster::intersect(Sl, poly)
# u <- union(intersected, circle)
# test = st_as_sf(intersected)
# test2 = st_polygonize(test)
# spPolygons(u)
#
#
# st_split(poly_sf, test)

lpi <- gIntersection(poly, Sl)
blpi <- gBuffer(lpi, width = 0.000001)
dpi <- gDifference(poly, blpi)
t = disaggregate(dpi)
dpisf = st_as_sf(t)
dpisf$name = seq(1, nrow(dpisf), 1)
dpisf$name2 = c(4,5,6,7,8,3,2,1)
dpisf$name3 = rev(dpisf$name2)
# dpisf$name3 = c(4,6,8,2,1,3,7,5)
g = ggplot(data = dpisf) +
    geom_sf(aes(fill = name2), colour = "#3a524c") +
    scale_fill_gradientn(colours = pals::tol.rainbow(8)[2:5]) +
    # scale_fill_identity() +
    theme(legend.position = "none")

g
# g <-
    # ggplot() +
    # geom_path(data = Slf, aes(x = long, y = lat, group=group), colour = "grey80", size = 2) +
    # geom_polygon(data = uf %>% filter(id!=10), aes(x = long, y = lat, group = id, fill = as.factor(id)), colour = "black")
    # theme(legend.position = "none")

sysfonts::font_add_google("Baloo Tamma 2", "baloo", regular.wt = 800)
# PNG
sticker(
    g,
    package = "oceancolouR",
    s_width = 1.6,
    s_height = 1.6,
    s_x = 1,
    s_y = 1.05,
    filename = "logoraw.png",
    h_color = "#69ffdc",
    h_fill = "#3a524c",
    p_y = 1.,
    p_family = "baloo",
    p_color = "#69ffdc",
    white_around_sticker = F,
    p_size = 17,
    dpi = 300,
    asp = 1
    # spotlight = T, l_x = 1, l_y = 1 # THIS IS NEAT tho it makes it look like a golf ball
)

# SVG
sticker( # P = package, s = subplot, h = hex border, l = spotlight
    g,
    package = "oceancolouR",
    s_width = 1.6,
    s_height = 1.6,
    s_x = 1,
    s_y = 1.,
    filename = "logoraw.svg",
    h_color = "#69ffdc",
    h_fill = "#3a524c",
    p_y = 1.,
    p_family = "baloo",
    p_color = "#69ffdc",
    white_around_sticker = T,
    p_size = 5 # behaves diff on mac vs PC
    # spotlight = T, l_x = 1, l_y = 1 # THIS IS NEAT tho it makes it look like a golf ball
)
