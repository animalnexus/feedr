% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{tz_offset}
\alias{tz_offset}
\title{Get timezone offset from UTC}
\usage{
tz_offset(tz, dst = FALSE, tz_name = FALSE)
}
\arguments{
\item{tz}{Character. Timezone to calculate offset from}

\item{dst}{Character. Whether or not to consider day-light-savings}

\item{tz_name}{Logical. Whether or not to return name of the timezone or
just the offset in hours.}
}
\description{
Get timezone offset from UTC
}
