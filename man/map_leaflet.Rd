% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maps.R
\name{map_leaflet}
\alias{map_leaflet}
\title{Map data using leaflet}
\usage{
map_leaflet(
  p = NULL,
  m = NULL,
  locs = NULL,
  summary = "none",
  p_scale = 1,
  m_scale = 1,
  p_title = "Time",
  m_title = "Path use",
  p_pal = c("yellow", "red"),
  m_pal = c("yellow", "red"),
  controls = TRUE,
  u.scale,
  p.scale,
  u.title,
  p.title,
  u.pal,
  p.pal
)
}
\arguments{
\item{p}{Dataframe. Regular or summarized presence data with columns
\code{logger_id} and, if already summarized, \code{amount}. It can also
contain \code{animal_id} for individual-based data, and lat/lon columns.}

\item{m}{Dataframe. Regular or summarized movement data with columns
\code{logger_id}, \code{move_path}, and, if already summarized,
\code{path_use}. It can also contain \code{animal_id} for individual-based
data, and lat/lon columns.}

\item{locs}{Dataframe. Lat and lon for each logger_id, required if lat and
lon not provided in either p or m.}

\item{summary}{Character. How the data should be summarized. If providing
summarized data, use "none" (default). "sum" calculates total movements and
total amount of time spent at a logger, "sum_indiv", averages the totals by
the number of individuals in the data, "indiv" calculates totals for each
individual.}

\item{p_scale, m_scale}{Numerical. Scaling constants to increase (> 1) or
decrease (< 1) the relative size of presence (p) and movements (m) data.}

\item{p_title, m_title}{Character. Titles for the legends of presence (p) and
movements (m) data.}

\item{p_pal, m_pal}{Character vectors. Colours used to construct gradients for
presence (p) and path (m) data.}

\item{controls}{Logical. Add controls to map (allows showing/hiding of
different layers)}

\item{u.scale, p.scale, u.title, p.title, u.pal, p.pal}{Depreciated.}
}
\value{
An interactive leaflet map with layers for presence, movement paths
and logger positions.
}
\description{
A visual summary of presence at and movements between loggers using leaflet
for R. This produces an interactive html map. This function can take
invidiual data as well as grand summaries (see Details and Examples).
}
\details{
The type of summary visualized is defined by the \code{summary}
argument: \code{summary = "sum"} calculates the total amount of time spent
at each logger (presence) or total number of movements between loggers
(movements). \code{summary = "sum_indiv"} averages these totals by the
number of individuals in the data set, resulting in an average total amount
of time per individual and an average amount of movements per individual
for, or between, each logger. \code{summary = "indiv"} calculates
individual totals. If the data is already summarized, use \code{summary = "none"}.
}
\examples{
v <- visits(finches)
p <- presence(v, bw = 15)
m <- move(v)

# Built in summaries
map_leaflet(p = p, m = m, summary = "sum")
map_leaflet(p = p, m = m, summary = "sum_indiv")
map_leaflet(p = p, m = m, summary = "indiv")

# Custom summaries
# Here, we average by the number of loggers (using dplyr)

library(dplyr)

p2 <- p \%>\%
  group_by(logger_id, lat, lon) \%>\%
  summarize(amount = sum(length) / logger_n[1])

m2 <- m \%>\%
  group_by(logger_id, move_path, lat, lon) \%>\%
  summarize(path_use = length(move_path) / logger_n[1])

map_leaflet(p = p2, m = m2)


}
