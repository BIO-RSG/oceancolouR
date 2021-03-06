% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics.R
\name{vector_errors}
\alias{vector_errors}
\title{Calculate error vectors}
\usage{
vector_errors(x, y)
}
\arguments{
\item{x}{Numeric vector of data.}

\item{y}{Numeric vector of data to compare to x, same length.}
}
\value{
Dataframe with columns representing error for each point. Types of error include: error, magnitude of error, percent error, magnitude of percent error, error of logged values, and magnitude of error of logged values.
}
\description{
Given vectors x and y of the same length, compute various forms of error between the two.
}
