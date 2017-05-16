#' Convert data for use in two asnipe functions
#'
#' Converts raw RFID data into a format for easy use by either the
#' \code{\link[asnipe]{gmmevents}} or the
#' \code{\link[asnipe]{get_associations_points_tw}} functions included in the
#' \link{asnipe} package.
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
#' @seealso \link{asnipe} package and it's functions: \link[asnipe]{gmmevents} and \link[asnipe]{get_associations_points_tw}. \url{https://cran.r-project.org/package=asnipe}
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
  return(as.data.frame(r))
}


#' Convert displacements for use by functions from the aniDom package
#'
#' Converts displacements data into a format for easy use by either the
#' \code{\link[aniDom]{elo_scores}}, the
#' \code{\link[aniDom]{estimate_uncertainty_by_repeatability}}, or the
#' \code{\link[aniDom]{estimate_uncertainty_by_splitting}} functions included in
#' the \link{aniDom} package.
#'
#' @param d Data frame or List. Either the specific displacements data frame which is
#'   returned as a list item from \code{disp()}, or the whole displacements list
#'   returned by \code{disp()}.
#'
#' @return A data frame listing winners and losers of all displacements sorted
#'   by time of the event.
#'
#' @seealso \link{aniDom} package and it's functions: \link[aniDom]{elo_scores}, \link[aniDom]{estimate_uncertainty_by_repeatability}, and \link[aniDom]{estimate_uncertainty_by_splitting}. \url{https://cran.r-project.org/package=aniDom}
#'
#' @examples
#'
#' # Calculate displacements
#' d <- disp(visits(finches_lg))
#'
#' # Format for use in aniDom's elo_scores
#' i <- convert_anidom(d)
#' i <- convert_anidom(d$displacements) # Equivalent
#'
#' # Use aniDom functions:
#'
#' library(aniDom)
#'
#' # Calculate elo_scores
#' s <- elo_scores(winners = i$winner, losers = i$loser)
#'
#' # Estimate repeatability
#' r1 <- estimate_uncertainty_by_repeatability(winners = i$winner, losers = i$loser)
#' r2 <- estimate_uncertainty_by_splitting(winners = i$winner, losers = i$loser, randomise = TRUE)
#'
#' @import magrittr
#' @export
convert_anidom <- function(d){

  # Function takes either the whole output of disp() or just the displacements
  if(!is.data.frame(d)) d <- d$displacements

  # Check for Correct formating
  check_name(d, c("animal_id","role"), type = "displacement")
  check_format(d)

  # Format
  d %>%
    dplyr::arrange(left) %>%
    dplyr::select(animal_id, role, left, arrived) %>%
    tidyr::spread(key = role, value = animal_id) %>%
    dplyr::mutate(displacer = as.character(displacer),
                  displacee = as.character(displacee)) %>%
    dplyr::select(winner = displacer, loser = displacee) %>%
    as.data.frame()
}


#' Convert displacements for use by functions from the Dominance package
#'
#' Converts displacements RFID data into a format for easy use by either the
#' \code{\link[Dominance]{ADI}} or the \code{\link[asnipe]{Sociogram}} functions
#' included in the \link{Dominance} package.
#'
#' @param d Data frame or List. Either the specific displacements data frame which is
#'   returned as a list item from \code{disp()}, or the whole displacements list
#'   returned by \code{disp()}.
#'
#' @return List of data frames to use in functions from the \link{Dominance}
#'   package. data_sheet contains all interactions: action.from/action.to
#'   represent individuals (code is matched to animal_id in the items data
#'   frame). action.from represent displacers (winners), action.to represent
#'   displacees (losers), kind.of.action is a dummy variable representing the
#'   action type (displacement). items is a data frame matching animal_ids to to
#'   the code used in the data_sheet. actions contain the one action type
#'   (displacements) and the classification and weighting (corresponds to
#'   action.from being a winner, and action.to being a loser). bytes is a dummy
#'   vector indicating that the action type "displacement" should be included in
#'   the calculation. See examples for specific application.
#'
#' @seealso \link{Dominance} package and it's functions: \link[Dominance]{ADI} and \link[Dominance]{Sociogram}. \url{https://cran.r-project.org/package=Dominance}
#'
#' @examples
#' # Calculate displacements
#' d <- disp(visits(finches_lg))
#'
#' # Format for use by Dominance package
#' i <- convert_dominance(d)
#' i <- convert_dominance(d$displacements) # Equivalent
#'
#' # Use Dominance package:
#' library(Dominance)
#'
#' # Calculate the Average Dominance Index
#' ADI(data_sheet = i$data_sheet, items = i$items, actions = i$actions, bytes = i$bytes)
#'
#' # Construct social network graphs
#' Sociogram(data_sheet = i$data_sheet, items = i$items, actions = i$actions, bits = i$bytes)
#'
#' @import magrittr
#' @export
convert_dominance <- function(d) {
  # Function takes either the whole output of disp() or just the displacements
  if(!is.data.frame(d)) d <- d$displacements

  # Check for Correct formating
  check_name(d, c("animal_id","role"), type = "displacement")
  check_format(d)

  d <- d %>%
    dplyr::arrange(left) %>%
    dplyr::mutate(animal_id = as.character(animal_id),
                  item.number = as.numeric(factor(d$animal_id)))

  items <- d %>%
    dplyr::select(Name = animal_id, item.number) %>%
    dplyr::distinct() %>%
    dplyr::arrange(item.number) %>%
    as.data.frame()

  data_sheet <- d %>%
    dplyr::select(item.number, role, left, arrived) %>%
    tidyr::spread(key = role, value = item.number) %>%
    dplyr::select(action.from = displacer, action.to = displacee) %>%
    dplyr::mutate(kind.of.action = 1) %>%
    as.data.frame()

  actions <- data.frame(name.of.action = "displacement", action.number = 1,
                        classification = 1, weighting = 1, stringsAsFactors = FALSE)

  bytes <- paste0(rep(1, nrow(actions)), collapse = "")

  return(list(data_sheet = data_sheet, items = items, actions = actions, bytes = bytes))
}



