% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polygons.R
\name{get_polygons}
\alias{get_polygons}
\title{Get predefined polygons from Northwest Atlantic}
\usage{
get_polygons(polys = NULL)
}
\arguments{
\item{polys}{Vector of polygon abbreviations. If NULL, all polygons will be returned.}
}
\value{
sf object containing the selected polygons
}
\description{
Given a polygon abbreviation (see Details), get the polygon as a Simple Features (sf) object for simplified use in extracting data from that region of interest. The polygons defined in this function are those used in regular reporting of the state of the Northwest Atlantic by the AZMP and AZOMP programs, and predefined in the PhytoFit app (https://cioosatlantic.ca/phytofit/).
}
\examples{
scotian_shelf <- get_polygons(c("ESS_V02","CSS_V02","WSS_V02"))
plot(scotian_shelf)
print(scotian_shelf)
}
