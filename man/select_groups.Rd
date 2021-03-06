% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatting.R
\name{select_groups}
\alias{select_groups}
\title{Select groups in dplyr}
\usage{
select_groups(data, groups, ...)
}
\arguments{
\item{data}{Dataframe that has already been grouped using dplyr::group_by()}

\item{groups}{Numeric vector of group numbers to use as the indices of the groups to select}
}
\value{
Same format as input dataframe, with only the selected groups, in the order listed. Duplicate group IDs are removed.
}
\description{
This returns the selected groups in a dataframe by index number.
}
\examples{
# This grabs the first 2 groups listed.
mtcars \%>\%
group_by(cyl) \%>\%
select_groups(1:2)

}
