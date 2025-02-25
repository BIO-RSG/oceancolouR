#' Get predefined polygons from Northwest Atlantic
#'
#' Given a polygon abbreviation (see Details), get the polygon as a Simple Features (sf) object for simplified use in extracting data from that region of interest. The polygons defined in this function are those used in regular reporting of the state of the Northwest Atlantic by the AZMP and AZOMP programs, and predefined in the PhytoFit app (https://cioosatlantic.ca/phytofit/).
#'
#'
#'
#' @param polys Vector of polygon abbreviations. If NULL, all polygons will be returned.
#' @return sf object containing the selected polygons
#' @import sf
#' @import tibble
#' @examples
#' scotian_shelf <- get_polygons(c("ESS_V02","CSS_V02","WSS_V02"))
#' plot(scotian_shelf)
#' print(scotian_shelf)
#' @export
get_polygons <- function(polys=NULL) {
    # Define ID, full name, and coordinates for each polygon
    atlantic = list(
        "AC"=list(name = "Avalon Channel",
                  lat = c(46, 48, 48, 46, 46),
                  lon = c(-53, -53, -51.5, -51.5, -53)),
        "CLS"=list(name = "Central Labrador Sea",
                   lat = c(55.5, 60.1, 60.1, 55.5, 55.5),
                   lon = c(-53.7, -53.7, -48.8, -48.8, -53.7)),
        "CS_V01"=list(name = "Cabot Strait V01",
                      lat = c(46.9, 48, 48, 46.9, 46.9),
                      lon = c(-60.4, -60.4, -59, -59, -60.4)),
        "CS_V02"=list(name = "Cabot Strait V02",
                      lat = c(47, 47.758, 48, 48, 47.436, 47),
                      lon = c(-59.903, -60.73, -60.322, -59.783, -59.168, -59.903)),
        "CSS_V01"=list(name = "Central Scotian Shelf V01",
                       lat = c(43.33, 44.33, 44.33, 43.33, 43.33),
                       lon = c(-64, -64, -62, -62, -64)),
        "CSS_V02"=list(name = "Central Scotian Shelf V02",
                       lat = c(43.558137,43.984863,44.182952,43.472424,42.984401,42.984401,43.558137),
                       lon = c(-63.4708,-62.881918,-61.390084,-61.115273,-62.293037,-63.353024,-63.4708)),
        "ESS_V01"=list(name = "Eastern Scotian Shelf V01",
                       lat = c(44.2, 45.67, 45.67, 44.2, 44.2),
                       lon = c(-60, -60, -58, -58, -60)),
        "ESS_V02"=list(name = "Eastern Scotian Shelf V02",
                       lat = c(44.267643,44.098138,44.52098,44.940753,45.107804,44.267643),
                       lon = c(-58.013828,-60.722685,-60.722685,-59.741215,-58.053087,-58.013828)),
        "FP"=list(name = "Flemish Pass",
                  lat = c(46, 48, 48, 46, 46),
                  lon = c(-47.5, -47.5, -46, -46, -47.5)),
        "GB_V01"=list(name = "Georges Bank V01",
                      lat = c(41,42,42,41, 41),
                      lon = c(-68,-68, -66.5, -66.5, -68)),
        "GB_V02"=list(name = "Georges Bank V02",
                      lat = c(41.879311,41.585239,41.082205,40.754628,41.052493,41.28981,41.526261,41.879311),
                      lon = c(-67.278903,-66.925574,-67.514456,-68.652961,-68.692219,-68.378149,-68.378149,-67.278903)),
        "GS"=list(name = "Greenland Shelf",
                  lat = c(60.1, 60.7, 60.7, 60.1, 60.1),
                  lon = c(-48.8, -48.8, -48.1, -48.1, -48.8)),
        "HB"=list(name = "Hamilton Bank",
                  lat = c(53.5, 54.5, 54.5, 53.5, 53.5),
                  lon = c(-56, -56, -54, -54, -56)),
        "HIB"=list(name = "Hibernia",
                   lat = c(46, 47, 47, 46, 46),
                   lon = c(-51, -51, -48.75, -48.75, -51)),
        "HL2"=list(name="Halifax Line 2",
                   lat=c(44.17,44.37,44.37,44.17,44.17),
                   lon=c(-63.42,-63.42,-63.22,-63.22,-63.42)),
        "LAS"=list(name = "Labrador Shelf",
                   lat = c(53.6, 55.5, 55.5, 53.6, 53.6),
                   lon = c(-55.7, -55.7, -53.7, -53.7, -55.7)),
        "LS_V01"=list(name = "Lurcher Shoal V01",
                      lat = c(43, 44, 44, 43, 43),
                      lon = c(-66.7, -66.7, -66, -66, -66.7)),
        "LS_V02"=list(name = "Lurcher Shoal V02",
                      lat = c(43.128346,42.753382,42.984401,43.871369,44.013202,43.27195,43.128346),
                      lon = c(-65.865586,-66.218916,-66.847056,-66.886315,-66.454468,-66.297433,-65.865586)),
        "MS_V01"=list(name = "Magdalen Shallows V01",
                      lat = c(46.5, 48, 48, 46.5, 46.5),
                      lon = c(-64, -64, -61.5, -61.5, -64)),
        "MS_V02"=list(name = "Magdalen Shallows V02",
                      lat = c(46.579, 46.579, 46.82, 47.82, 47.82, 47.26, 47.1, 46.579),
                      lon = c(-61.9, -63.162, -63.766, -63.766, -61.606, -62.23, -61.9, -61.9)),
        "NEGSL_V01"=list(name = "Northeast Gulf of St. Lawrence V01",
                         lat = c(49, 50, 50, 49, 49),
                         lon = c(-61, -61, -58, -58, -61)),
        "NEGSL_V02"=list(name = "Northeast Gulf of St. Lawrence V02",
                         lat = c(49, 50, 50, 49, 49),
                         lon = c(-61, -61, -58, -58.73, -61)),
        "NENS"=list(name = "Northeast Newfoundland Shelf",
                    lat = c(48.5, 50, 50, 48.5, 48.5),
                    lon = c(-53, -53, -51, -51, -53)),
        "NWGSL_V01"=list(name = "Northwest Gulf of St. Lawrence V01",
                         lat = c(49.7, 50.3, 50.3, 49.7, 49.7),
                         lon = c(-67.0, -67.0, -64.5, -64.5, -67.0)),
        "NWGSL_V02"=list(name = "Northwest Gulf of St. Lawrence V02",
                         lat = c(49.7, 50.16, 50.16, 49.7, 49.7),
                         lon = c(-66.86, -65.93, -64.76, -64.76, -66.86)),
        "P5"=list(name="Prince5",
                  lat=c(44.83,45.03,45.03,44.83,44.83),
                  lon=c(-66.95,-66.95,-66.75,-66.75,-66.95)),
        "SES"=list(name = "Southeast Shoal",
                   lat = c(44, 46, 46, 44, 44),
                   lon = c(-52, -52, -50, -50, -52)),
        "SAB"=list(name = "St. Anthony Basin",
                   lat = c(50, 52, 52, 50, 50),
                   lon = c(-55, -55, -53, -53, -55)),
        "SPB"=list(name = "St. Pierre Bank",
                   lat = c(45.33, 46.33, 46.33, 45.33, 45.33),
                   lon = c(-56, -56, -54, -54, -56)),
        "WSS_V01"=list(name = "Western Scotian Shelf V01",
                       lat = c(42.5, 43.33, 43.33, 42.5, 42.5),
                       lon = c(-65.5, -65.5, -64.5, -64.5, -65.5)),
        "WSS_V02"=list(name = "Western Scotian Shelf V02",
                       lat = c(43.013218,43.443826,42.869001,42.666526,42.259577,42.608553,43.013218),
                       lon = c(-65.551516,-64.727082,-64.1382,-64.923376,-65.551516,-65.708551,-65.551516)))
    # Turn polygons into Simple feature collection of multipolygon objects
    allp <- names(atlantic)
    atlanticsf <- lapply(1:length(atlantic), function(i) {
        yi <- atlantic[[i]]
        coords <- matrix(unlist(yi[c("lon","lat")]),ncol=2)
        tibble::tibble(polygon=allp[i], name=yi$name, geometry=list(sf::st_multipolygon(x=list(list(coords)))))
    })
    atlanticsf <- sf::st_sf(do.call(dplyr::bind_rows, atlanticsf))
    sf::st_crs(atlanticsf) <- "EPSG:4326"
    if (!is.null(polys)) {atlanticsf <- dplyr::filter(atlanticsf, polygon %in% polys)}
    return(list(aslist=atlantic, assf=atlanticsf))
}
