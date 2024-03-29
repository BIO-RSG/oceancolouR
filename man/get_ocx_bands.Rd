% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CHLA_OCX.R
\name{get_ocx_bands}
\alias{get_ocx_bands}
\title{Get OCX wavebands for a sensor}
\usage{
get_ocx_bands(sensor, use_443nm)
}
\arguments{
\item{sensor}{String, either modisaqua, seawifs, viirssnpp, landsat8, or olci}

\item{use_443nm}{Logical value, TRUE to make the 443nm band an option in the band ratio}
}
\value{
Named list of 2 character vectors, one for "green" waveband(s) and one for "blue"
}
\description{
Given a sensor name, get the wavebands typically used in the OCX algorithm.
}
