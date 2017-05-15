#' Convert data for use in two asnipe functions
#'
#' Converts raw RFID data in a format for easy use in either the \code{\link[asnipe]{gmmevents}}
#' or the \code{\link[asnipe]{get_associations_points_tw}} functions included in the asnipe
#' package.
#'
#' @param r Dataframe. Raw RFID dataset. Must have at least columns
#'   \code{animal_id}, \code{logger_id} and \code{time}. The column time should
#'   be formated as POSIXct (data/time). (Consider using
#'   \code{\link{load_format}} to format your dataframe)
#' @param fun Which asipe function should the data be formated for?
#'   \code{\link[asnipe]{gmmevents}} or
#'   \code{\link[asnipe]{get_associations_points_tw}}.
#' @param by_day Logical. The \code{\link[asnipe]{gmmevents}} function suggests that
#'   locations are split into sublocations defined by the scale at which one
#'   might see temporal gaps in the data (e.g., by day as diurnal species are
#'   not active at night). If \code{by_day = TRUE}, logger_ids (locations) will
#'   be split into unique locations by date (e.g., 2200 would become
#'   2200_2016-10-28, 2200_2016_10-29, etc.). See
#'   \code{\link[asnipe]{gmmevents}} for more details. Ignored if the
#'   \code{get_associations_points_tw} function is selected.
#' @param time_scale Character. The \code{\link[asnipe]{gmmevents}} function
#'   suggests that the time data should be at a scale relevant to the definition
#'   of group membership. For birds in flocks visiting feeders, seconds are
#'   appropriate. Values can be any specified by the
#'   \code{\link[base]{difftime}} function: "auto", "secs", "mins", "hours",
#'   "days", "weeks". Ignored if the \code{get_associations_points_tw} function
#'   is selected.
#'
#' @return If \code{fun = "gmmevents"}, a data frame with three columns, \code{time}, \code{identity}, and \code{location}. Time is converted to the number of seconds or minutes (etc., taken from \code{time_scale}) since the first observation.
#'
#' If \code{fun = "get_associations_points_tw"}, a data frame with four columns: \code{Date}, \code{Time}, \code{ID}, \code{Location}. Date is converted to count starting with 1 as the first day, time is converted to the number of seconds since the first observation.
#'
#' @seealso \link{asnipe} package and it's functions: \link[asnipe]{gmmevents} and \link[asnipe]{get_associations_points_tw}. \link{https://cran.r-project.org/package=asnipe}
#'
#' @examples
#'
#' a <- convert_asnipe(finches, fun = "gmmevents")
#'
#' \dontrun{
#' library(asnipe)
#' gmmevents(a$time, a$identity, a$location)
#' }
#'
#' a <- convert_asnipe(finches, fun = "get_associations_points_tw")
#'
#' \dontrun{
#' library(asnipe)
#' get_associations_points_bw(a)
#' }
#'
#' @import magrittr
#' @export
convert_asnipe <- function(r, fun = "gmmevents", by_day = TRUE, time_scale = "secs"){
  check_name(r, n = c('animal_id', 'logger_id', 'time'))
  check_time(r, n = "time", internal = FALSE)

  r <- dplyr::mutate(r, date = as.Date(time))

  if(!(time_scale %in% c("auto", "secs", "mins", "hours", "days", "weeks"))) stop("time_scale should be one of: 'auto', 'secs', 'mins', 'hours', 'days', or 'weeks'.")

  if(fun == "gmmevents"){
    if(by_day) r <- dplyr::mutate(r, logger_id = factor(paste0(logger_id, "_", date)))
    r <- dplyr::mutate(r, time = as.numeric(difftime(time, min(time, na.rm = TRUE), units = time_scale))) %>%
      dplyr::select(time, identity = animal_id, location = logger_id)
  } else if(fun == "get_associations_points_tw") {
    r <- r %>%
      dplyr::mutate(date = as.numeric(difftime(date, min(date, na.rm = TRUE), units = "day")) + 1,
                    time = as.numeric(difftime(time, min(time, na.rm = TRUE), units = "sec"))) %>%
      dplyr::select(Date = date, Time = time,
                    ID = animal_id, Location = logger_id)
  }
  return(r)
}


#' Convert displacements for use in aniDom functions
#'
#' @return
#'
#' @examples
#'
#' library(aniDom)
#'
#'
#'
#' @export
#'
convert_anidom <- function(){
  d <- disp(visits(finches_lg))

  e <- d$displacements %>%
    dplyr::select(animal_id, role, left, arrived) %>%
    tidyr::spread(key = role, value = animal_id)



  s <- elo_scores(winners = as.character(e$displacer), losers = as.character(e$displacee))
  plot_ranks(s)

}

convert_dominance <- function() {

}

convert_perc <- function() {

}



