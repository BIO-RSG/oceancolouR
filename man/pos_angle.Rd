% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/math_funcs.R
\name{pos_angle}
\alias{pos_angle}
\title{Convert an angle to its equivalent value in the range 0 to 2*pi}
\usage{
pos_angle(x)
}
\arguments{
\item{x}{Angle (IN RADIANS)}
}
\value{
Corresponding angle between 0 and 2*pi (IN RADIANS)
}
\description{
Given an angle in radians, shift it to a value between 0 and 2*pi.
}
\details{
This function was originally included in the code for the ULaval primary production model.
}
\examples{
angle_rad <- -4.3*pi
pos_angle(angle_rad)

# starting with an angle in degrees: convert to radians, calculate positive angle, convert back
angle_deg <- -365
pos_angle(angle_deg*pi/180) * 180/pi

# multiple angles
pos_angle(c(-3, -2*pi, 4*pi, 0, 5*pi, pi/2))

}
