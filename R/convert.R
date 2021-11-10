#' Convert data for use in two asnipe functions
#'
#' Converts raw RFID data into a format for easy use by either the
#' [asnipe::gmmevents()] or the
#' [asnipe::get_associations_points_tw()] functions included in the
#' [asnipe] package for calculating group membership.
#'
#' @param r Dataframe. Raw RFID dataset. Must have at least columns
#'   `animal_id`, `logger_id` and `time`. The column time should
#'   be formated as POSIXct (data/time). (Consider using
#'   [load_format()] to format your dataframe)
#' @param fun Which asipe function should the data be formated for?
#'   [asnipe::gmmevents()] or
#'   [asnipe::get_associations_points_tw()].
#' @param by_day Logical. The [asnipe::gmmevents()] function suggests that
#'   locations are split into sublocations defined by the scale at which one
#'   might see temporal gaps in the data (e.g., by day as diurnal species are
#'   not active at night). If `by_day = TRUE`, logger_ids (locations) will
#'   be split into unique locations by date (e.g., 2200 would become
#'   2200_2016-10-28, 2200_2016_10-29, etc.). See
#'   [asnipe::gmmevents()] for more details. Ignored if the
#'   `get_associations_points_tw` function is selected.
#' @param time_scale Character. The [asnipe::gmmevents()] function
#'   suggests that the time data should be at a scale relevant to the definition
#'   of group membership. For birds in flocks visiting feeders, seconds are
#'   appropriate. Values can be any specified by the
#'   [base::difftime()] function: "auto", "secs", "mins", "hours",
#'   "days", "weeks". Ignored if the `get_associations_points_tw` function
#'   is selected.
#'
#' @return If `fun = "gmmevents"`, a data frame with three columns, `time`, `identity`, and `location`. Time is converted to the number of seconds or minutes (etc., taken from `time_scale`) since the first observation.
#'
#' If `fun = "get_associations_points_tw"`, a data frame with four columns: `Date`, `Time`, `ID`, `Location`. Date is converted to count starting with 1 as the first day, time is converted to the number of seconds since the first observation.
#'
#' @seealso [asnipe] package and it's functions: [gmmevents][asnipe::gmmevents] and [get_associations_points_tw][asnipe::get_associations_points_tw]. <https://cran.r-project.org/package=asnipe>
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
#' [aniDom::elo_scores()], the
#' [aniDom::estimate_uncertainty_by_repeatability()], or the
#' [aniDom::estimate_uncertainty_by_splitting()] functions included in
#' the [aniDom] package for calculating dominance hierarchies from
#' Elo scores and assessing their robustness. Only includes individuals that
#' participated in at least one interaction.
#'
#' @param d Data frame or List. Either the specific displacements data frame which is
#'   returned as a list item from `disp()`, or the whole displacements list
#'   returned by `disp()`.
#'
#' @return A data frame listing winners and losers of all displacements sorted
#'   by time of the event.
#'
#' @seealso [aniDom] package and it's functions: [elo_scores][aniDom::elo_scores], [estimate_uncertainty_by_repeatability][aniDom::estimate_uncertainty_by_repeatability], and [estimate_uncertainty_by_splitting][aniDom::estimate_uncertainty_by_splitting]. <https://cran.r-project.org/package=aniDom>
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
#' \dontrun{
#' library(aniDom)
#'
#' # Calculate elo_scores
#' s <- elo_scores(winners = i$winner, losers = i$loser)
#'
#' # Estimate repeatability
#' r1 <- estimate_uncertainty_by_repeatability(winners = i$winner, losers = i$loser)
#' r2 <- estimate_uncertainty_by_splitting(winners = i$winner, losers = i$loser, randomise = TRUE)
#'
#' }
#'
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
#' [Dominance::ADI()] or the [Dominance::Sociogram()]
#' functions included in the [Dominance] package for calculating average
#' dominance indices and drawing sociograms. Only includes individuals that
#' participated in at least one interaction.
#'
#' @param d Data frame or List. Either the specific displacements data frame which is
#'   returned as a list item from `disp()`, or the whole displacements list
#'   returned by `disp()`.
#'
#' @return List of data frames to use in functions from the [Dominance]
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
#' @seealso [Dominance] package and it's functions: [ADI][Dominance::ADI] and [Sociogram][Dominance::Sociogram]. <https://cran.r-project.org/package=Dominance>
#'
#' @examples
#' # Calculate displacements
#' d <- disp(visits(finches_lg))
#'
#' # Format for use by Dominance package
#' i <- convert_dominance(d)
#' i <- convert_dominance(d$displacements) # Equivalent
#'
#' \dontrun{
#' # Use Dominance package:
#' library(Dominance)
#'
#' # Calculate the Average Dominance Index
#' ADI(data_sheet = i$data_sheet, items = i$items, actions = i$actions, bytes = i$bytes)
#'
#' # Construct social network graphs
#' Sociogram(data_sheet = i$data_sheet, items = i$items, actions = i$actions, bits = i$bytes)
#' }
#'
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

#' Convert displacements for use by the Perc package
#'
#' Converts displacements RFID data into a format for easy use by the
#' [Perc::as.conflictmat()] function included in the [Perc]
#' package. Can then be applied to internal Perc functions for calculating
#' dominance from perculation and conductance. Only includes individuals that
#' participated in at least one interaction.
#'
#' @param d Data frame or List. Either the specific interactions data frame which is
#'   returned as a list item from `disp()`, or the whole displacements list
#'   returned by `disp()`.
#'
#' @return A data frame of interactions for input into
#'   [Perc::as.conflictmat()]. See examples for specific application.
#'
#' @seealso [Perc] package and it's function [as.conflictmat][Perc::as.conflictmat].
#'   <https://cran.r-project.org/package=Perc>
#'
#' @examples
#' # Calculate displacements
#' d <- disp(visits(finches_lg))
#'
#' # Format for use by Perc package
#' i <- convert_perc(d)
#' i <- convert_perc(d$interactions) # Equivalent
#'
#' \dontrun{
#' # Use Perc package:
#' library(Perc)
#'
#' # Calculate ranks (adapted from Perc examples)
#' conflict_mat <- as.conflictmat(i, weighted = TRUE)
#' perm <- conductance(conflict_mat, 2)
#' simRankOrder(perm$p.hat, num = 10, kmax = 1000)
#'
#' }
#'
#' @export
convert_perc <- function(d){

  # Function takes either the whole output of disp() or just the displacements
  if(!is.data.frame(d)) d <- d$interactions

  # Check for Correct formating
  check_name(d, c("displacer", "displacee","n"), type = "displacement")
  check_format(d)

  d %>%
    dplyr::filter(n != 0) %>%
    dplyr::mutate(displacer = as.character(displacer), displacee = as.character(displacee)) %>%
    dplyr::rename(Initiator1 = displacer, Recipient1 = displacee, Freq = n) %>%
    as.data.frame()
}

#' Convert data for use by the activity package
#'
#' Converts raw RFID data into a format for easy use by the
#' [activity::fitact()] function included in the [activity]
#' package for modelling activity levels and daily activity patterns.
#'
#' @param r Dataframe. Raw RFID dataset. Must have at least columns
#'   `animal_id`, `logger_id` and `time`. The column time should
#'   be formated as POSIXct (data/time). (Consider using
#'   [load_format()] to format your dataframe)
#'
#' @return A list of vectors corresponding to each individual. See examples for
#'   specific application.
#'
#' @seealso [activity] package and it's function [fitact][activity::fitact].
#'   <https://cran.r-project.org/package=activity>
#'
#' @examples
#'
#' # Format for use by activity package
#' i <- convert_activity(finches_lg)
#'
#' \dontrun{
#' # Use activity package:
#' library(activity)
#'
#' # Calculate daily activity pattern for a single individual
#' a <- fitact(i[[1]], sample = "none")
#' plot(a)
#'
#' # Calculate daily activity pattern for all individuals
#' a <- lapply(i, fitact, sample = "none")
#' plot(a[[3]])
#' plot(a[["06200004F8"]])
#' # etc.
#' }
#'
#' @export
convert_activity <- function(r){
  # Check for Correct formatting
  check_name(r, n = c('animal_id', 'logger_id', 'time'))
  check_time(r, n = "time", internal = FALSE)

  t <- r %>%
    dplyr::mutate(midnight = lubridate::floor_date(time, "day"),
                  time_sec = as.numeric(difftime(time, midnight, units = "sec")),
                  time_sec = time_sec / (60*60*24),
                  time_rad = 2 * pi * time_sec) %>%
    dplyr::select("animal_id", "time_rad") %>%
    tidyr::nest(data = c(.data$time_rad)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(data = as.list(data))
  t2 <- as.list(t$data)
  names(t2) <- t$animal_id
  t2
}
