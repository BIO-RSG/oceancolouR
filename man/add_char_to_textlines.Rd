% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatting.R
\name{add_char_to_textlines}
\alias{add_char_to_textlines}
\title{Add a string to the end of each line in a file.}
\usage{
add_char_to_textlines(file_in, file_out, ch = "  ")
}
\arguments{
\item{file_in}{Input filename}

\item{file_out}{Output filename}

\item{ch}{Character string to add to the end of each line}
}
\value{
TRUE if successful, FALSE if not
}
\description{
This can be used to add a double space to the end of each line of an Rmarkdown file, which creates a new line.
}
