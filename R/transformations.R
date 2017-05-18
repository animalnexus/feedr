#' Visits
#'
#' Raw data from RFID loggers contain multiple reads per individual simply
#' because the individual sat there long enough. In \code{visits()} these reads
#' are collapsed into one visit.
#'
#' Visits are defined by three pieces of data:
#' \itemize{
#' \item How much time has passed between reads (\code{bw})
#' \item A change in identity between two successive reads (\code{animal_id})
#' \item A change in logger for a single individual (\code{logger_id})
#' }
#'
#' The function will return an error if impossible visits are detected (unless
#'   \code{allow_imp = TRUE}) . A visit is deemed impossible if a single animal
#'   travels between loggers in less time than specified by \code{bw}.
#'
#' @param r Dataframe. Contains raw reads from an RFID reader with colums
#'   \code{animal_id}, \code{logger_id}, \code{time}.
#' @param bw Numerical. The minimum interval, in seconds, between reads for two
#'   successive reads to be considered separate visits.
#' @param allow_imp Logical. Whether impossible visits should be allowed (see
#'   details).
#' @param bw_imp Numerical. The minimum number of seconds required to travel
#'   between loggers. If quicker, visits considered impossible.
#' @param na_rm Logical. Whether NA values should be automatically omited.
#'   Otherwise an error is returned.
#' @param pass Logical. Pass 'extra' columns through the function and append
#'   them to the output.
#' @param allow.imp,na.rm Depreciated.
#'
#' @return A data frame with visits specifying \code{animal_id} and
#'   \code{logger_id} as well as the \code{start} and \code{end} of the visit.
#'   Any extra columns that are unique at the level of animal_id or logger_id will
#'   also be passed through (i.e. age, sex, logger location, etc.).
#'
#' @examples
#' v <- visits(finches)
#' head(v)
#'
#' v <- visits(finches, bw = 30)
#' head(v)
#'
#' # Calculate across different experiments:
#' library(dplyr)
#' v <- chickadees %>%
#'   group_by(experiment) %>%
#'   do(visits(.))
#'
#' @import magrittr
#' @export
visits <- function(r, bw = 3, allow_imp = FALSE, bw_imp = 2, na_rm = FALSE, pass = TRUE, allow.imp, na.rm){
  if (!missing(allow.imp)) {
    warning("Argument allow.imp is deprecated; please use allow_imp instead.",
            call. = FALSE)
    allow_imp <- allow.imp
  }

  if (!missing(na.rm)) {
    warning("Argument na.rm is deprecated; please use na_rm instead.",
            call. = FALSE)
    na_rm <- na.rm
  }

  # Confirm that expected columns and formats are present
  check_name(r, n = c("animal_id", "logger_id", "time"), "raw RFID")
  check_time(r, n = "time", internal = FALSE)
  check_format(r)

  # Check for NAs, remove if specified by na_rm = TRUE
  if(any(is.na(r[, c("animal_id", "logger_id", "time")]))){
    if(na_rm == FALSE) stop("NAs found. To automatically remove NAs, specify 'na_rm = TRUE'.")
    if(na_rm == TRUE) r <- r[rowSums(is.na(r)) == 0,]
  }

  ## Make factors and get date
  r <- dplyr::mutate(r,
                     date = as.Date(time),
                     logger_id = factor(logger_id),
                     animal_id = factor(animal_id))

  # Grab unique extra cols
  if(pass == TRUE) extra <- keep_extra(r, n = "time")

  # Grab the timezone
  tz <- attr(r$time, "tzone")

  # Get spacing between visits, whether same animal or not, and whether same logger or not

  # Diff animal_id AT SAME LOGGER
  # Diff time < bw PER ID
  # Diff logger PER ID

  v <- r %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(diff_animal = dplyr::lead(logger_id) == logger_id & dplyr::lead(animal_id) != animal_id) %>%
    dplyr::group_by(animal_id) %>%
    dplyr::mutate(diff_time = difftime(dplyr::lead(time), time, units = "sec") > bw,
                  diff_logger = dplyr::lead(logger_id) != logger_id)

  # Check for impossible combos: where less than bw, still the same animal, but a different logger
  if(!allow_imp) {
    impos <- v %>%
      dplyr::mutate(diff_imp = difftime(dplyr::lead(time), time, units = "sec") < bw_imp,
                    diff_imp = diff_imp & diff_logger) %>%
      dplyr::arrange(animal_id) %>%
      dplyr::filter(diff_imp | dplyr::lag(diff_imp)) %>%
      unique()

    if(nrow(impos) > 0) {
      impos <- impos %>%
        dplyr::arrange(animal_id, time) %>%
        dplyr::select(animal_id, time, logger_id)

      rows <- nrow(impos)
      if(nrow(impos) > 6) {
        rows <- 6
      }
      stop("Impossible visits found (n = ", nrow(impos), "), no specification for how to handle:\n\nIndividual(s) detected at 2+ loggers within ", bw_imp, "s.\nDecrease the `bw_imp` argument, remove these reads, or\nallow impossible visits (allow_imp = TRUE) and try again.\n\nFirst 6 impossible visits:\n", paste0(utils::capture.output(as.data.frame(impos[1:rows, ])), collapse = "\n"), call. = FALSE)
    }
  }

  # End if
  # - for same animal:
  #     - next logger diff OR next time > bw  ==> diff_logger OR diff_bw
  #     - final obs
  # - next animal diff (at same logger)       ==> diff_animal

  # Start if
  # All PER ANIMAL (i.e. keep grouping)
  # - first obs is an include
  # - previous obs was an end

  # Start/End if (only one obs in visit)
  # - first obs is an end
  # - end, but previous obs was also an end

  v <- v %>%
    # Assign end points
    dplyr::mutate(new = "include") %>%
    dplyr::mutate(new = replace(new, diff_logger | diff_time | diff_animal, "end"),
                  new = replace(new, is.na(dplyr::lead(new)), "end")) %>%
    # Assign start or start-end points for each individual
    dplyr::mutate(new = replace(new, new == "include" &
                                  (is.na(dplyr::lag(new)) | dplyr::lag(new) == "end"), "start"),
                  new = replace(new, new == "end" &
                                  (is.na(dplyr::lag(new)) | dplyr::lag(new) == "end"), "start-end")) %>%
    dplyr::ungroup() %>%
    dplyr::filter(new != "include") %>%
    dplyr::mutate(start = as.POSIXct(NA, tz = tz),
                  end = as.POSIXct(NA, tz = tz),
                  start = replace(start, stringr::str_detect(new, "start"), time[stringr::str_detect(new, "start")]),
                  end = replace(end, stringr::str_detect(new, "end"), time[stringr::str_detect(new, "end")])) %>%
    dplyr::select(logger_id, animal_id, start, end) %>%
    tidyr::gather(variable, value, start, end) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::arrange(value) %>%
    dplyr::group_by(logger_id, animal_id, variable) %>%
    dplyr::mutate(n = 1:length(value)) %>%
    tidyr::spread(variable, value) %>%
    dplyr::select(-n) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(animal_n = length(unique(animal_id)),         # Get sample sizes
                 logger_n = length(unique(logger_id)),
                 date = as.Date(start))

  # Set timezone attributes
  attr(v$start, "tzone") <- tz
  attr(v$end, "tzone") <- tz

  # Order data frame
  v <- v %>%
    dplyr::select(animal_id, date, start, end, logger_id, animal_n, logger_n) %>%
    dplyr::arrange(animal_id, start)

  # Add in extra variables
  if(pass == TRUE) v <- merge_extra(v, extra)

  return(v)
}


#' Movements
#'
#' Turns visits to mulitple loggers into movements between loggers
#'
#' @param v Dataframe. A visits data frame (may contain multiple animal_ids). From
#'   the output of \code{visits}. Must contain columns \code{animal_id},
#'   \code{logger_id}, \code{start}, and \code{end}.
#' @param all Logical. Should all animal_ids be returned, even if the animal made no
#'   movements? If TRUE, a data frame with NAs for all columns except
#'   \code{animal_id} will be returned, if FALSE, an empty data frame will be
#'   returned.
#' @param pass Logical. Pass 'extra' columns through the function and append
#'   them to the output.
#'
#' @return A data frame of movements. These are defined as the bout of time from
#'   leaving one logger to arriving at a second one. Each movement bout consists
#'   of two rows of data containing:
#'   \itemize{
#'   \item ID of the animal (\code{animal_id})
#'   \item Date of event (\code{date})
#'   \item Time of event (\code{time})
#'   \item The ID of loggers involved (\code{logger_id})
#'   \item The movement path including direction (\code{move_dir})
#'   \item The movement path independent of direction (\code{move_path})
#'   \item The 'strength' of the connection (inverse of time taken to move
#'   between; \code{strength})
#'   \item Information on whether left/arrived (\code{direction})
#'   \item The ID of a single move event for a particular individual (\code{move_id})
#'   \item Any extra columns \code{pass}ed through
#'   }
#'
#' @examples
#'
#' v <- visits(finches)
#' m <- move(v)
#'
#' # Summarize (divide by 2 because 2 rows per event)
#' library(dplyr)
#'
#' m.totals <- m %>%
#'   group_by(animal_id, move_path) %>%
#'   summarize(n_path = length(move_path) /2)
#'
#' # Calculate across different experiments:
#'
#' v <- chickadees %>%
#'   group_by(experiment) %>%
#'   do(visits(.))
#'
#' m <- v %>%
#'   group_by(experiment) %>%
#'   do(move(.))
#'

#' @import magrittr
#' @export
move <- function(v, all = FALSE, pass = TRUE){

  # Check for correct formatting
  check_name(v, c("animal_id", "logger_id", "date", "start", "end"))
  check_time(v)
  check_format(v)

  # Get factor levels for whole dataset
  if(is.factor(v$animal_id)) animal_id <- levels(v$animal_id) else animal_id <- unique(v$animal_id)
  if(is.factor(v$logger_id)) logger_id <- levels(v$logger_id) else logger_id <- unique(v$logger_id)

  # Remove factors to allow silent joins between different levels
  v$animal_id <- as.character(v$animal_id)
  v$logger_id <- as.character(v$logger_id)

  # Get extra columns
  if(pass == TRUE) extra <- keep_extra(v, n = c("start", "end"))

  # Get movement options
  move_dir <- expand.grid(logger_left = logger_id, logger_arrived = logger_id)
  move_dir <- move_dir[move_dir$logger_left != move_dir$logger_arrived,]
  move_dir <- paste0(move_dir$logger_left, "_", move_dir$logger_arrived)
  move_path <- unique(sapply(move_dir, FUN = mp))

  # Apply individually to each animal
  m <- v %>%
    dplyr::group_by(animal_id) %>%
    dplyr::do(move_single(., move_dir = move_dir, move_path = move_path, all = all)) %>%
    dplyr::ungroup()

  if(nrow(m) > 0){
    # Order
    m <- m %>%
      dplyr::select(animal_id, date, time, logger_id, direction, move_id, move_dir, move_path, strength) %>%
      dplyr::arrange(animal_id, time)

    # Add in extra cols
    if(pass == TRUE) m <- merge_extra(m, extra)

    # Apply factors
    m$logger_id <- factor(m$logger_id, levels = logger_id)
  }
  m$animal_id <- factor(m$animal_id, levels = animal_id)

  return(m)
}

# Movement function for single animal
move_single <- function(v1, move_dir, move_path, all = FALSE){

  if(length(unique(v1$logger_id)) > 1) { # Only proceed if there are actual data!

    # If there are movements, calculate events
    v1 <- v1[, c("animal_id", "date", "start", "end", "logger_id")]
    v1 <- v1[order(v1$start),]
    diff <- v1$logger_id[-1] != v1$logger_id[-nrow(v1)]
    v1$arrived <- v1$left <- FALSE
    v1$left[c(diff, FALSE)] <- TRUE
    v1$arrived[c(FALSE, diff)] <- TRUE

    m <- v1 %>%
      dplyr::filter(left | arrived) %>%
      tidyr::gather(direction, value, left, arrived) %>%
      dplyr::filter(value) %>%
      tidyr::gather(type, time, start, end) %>%
      dplyr::filter((direction == "arrived" & type == "start") |
                    (direction == "left" & type == "end")) %>%
      dplyr::select(-value, -type) %>%
      dplyr::arrange(time) %>%
      dplyr::mutate(move_id = sort(rep(1:(length(animal_id)/2),2))) %>%
      dplyr::group_by(move_id) %>%
      dplyr::mutate(move_dir = factor(paste0(logger_id, collapse = "_"), levels = move_dir),
                    move_path = factor(paste0(sort(logger_id), collapse = "_"), levels = move_path),
                    strength = 1 / as.numeric(difftime(time[direction == "arrived"], time[direction == "left"], units = "hours"))) %>%
      dplyr::ungroup()
  } else if (all == TRUE) {
    # Create the movement data frame for animals that didn't move between loggers
    m <- tibble::data_frame(animal_id = v1$animal_id[1],
                            date = as.Date(NA),
                            time = as.POSIXct(NA),
                            logger_id = as.character(NA),
                            direction = as.character(NA),
                            move_id = as.numeric(NA),
                            move_dir = factor(NA, levels = move_dir),
                            move_path = factor(NA, levels = move_path),
                            strength = as.numeric(NA))
  } else {
    # If there are no movements and all == FALSE, return an empty data frame
    m <- tibble::data_frame()
  }
  return(m)
}

#' Presence
#'
#' Turns multiple visits at specific loggers into overall presence events.
#' Presence is different from a visit in that a visit is considered a specific
#' period of time in which the individual was in range of a logger to the
#' exclusion of others. In contrast, presence reflects a period of time where
#' the individual was making regular visits to the logger but not necessarily in
#' range at all times, or to the exclusion of other individuals. Presence can be
#' considered a less precise 'smoothing' of the data.
#'
#' The start and end of a period of presence is determined by either switching
#' loggers (when \code{bw = NULL}) or by both switching loggers and by a cutoff
#' time of \code{bw} minutes.
#'
#'
#' @param v Dataframe. A visits data frame from the output of \code{\link{visits}} (may
#'   contain more than one animal_id). Must contain columns \code{animal_id},
#'   \code{logger_id}, \code{start}, and \code{end}.
#' @param bw Numeric. The maximum number of minutes between visits for them to
#'   be considered the same event. When \code{bw} = NULL only
#'   visits to another logger are scored as a separate event.
#' @param pass Logical. Pass 'extra' columns through the function and append
#'   them to the output.
#'
#' @return A data frame of presence events. This data frame has the following
#'   columns:
#'   \itemize{
#'   \item ID of the animal (\code{animal_id})
#'   \item ID of the logger(\code{logger_id})
#'   \item Time of the start of the event (\code{start})
#'   \item Time of the end of the event (\code{end})
#'   }
#'
#' @examples
#'
#' v <- visits(finches)
#' p <- presence(v)
#'
#' # Summarize a movement dataframe (get total time present per logger per animal)
#' library(dplyr)
#' p_totals <- p %>%
#'     group_by(animal_id, logger_id) %>%
#'     summarize(length = sum(length))
#'
#' # Calculate across different experiments (expect warnings about unequal factor levels):
#' library(dplyr)
#'
#' v <- chickadees %>%
#'   group_by(experiment) %>%
#'   do(visits(.))
#'
#' p <- v %>%
#'   group_by(experiment) %>%
#'   do(presence(.))
#'
#'
#' @export

presence <- function(v, bw = 15, pass = TRUE){

  ## Check for correct formatting
  check_name(v, c("animal_id","logger_id", "date", "start", "end"))
  check_time(v)
  check_format(v)

  # Get factor levels for whole dataset
  if(is.factor(v$animal_id)) animal_id <- levels(v$animal_id) else animal_id <- unique(v$animal_id)
  if(is.factor(v$logger_id)) logger_id <- levels(v$logger_id) else logger_id <- unique(v$logger_id)

  # Remove factors for now
  v$animal_id <- as.character(v$animal_id)
  v$logger_id <- as.character(v$logger_id)

  # Keep extra cols
  if(pass == TRUE) extra <- keep_extra(v, n = c("start", "end"))

  # Calculate for each individual
  p <- v %>%
    dplyr::group_by(animal_id) %>%
    dplyr::do(presence_single(., bw = bw)) %>%
    dplyr::ungroup()

  # Get extra columns and add in
  if(pass == TRUE) p <- merge_extra(p, extra)
  p <- p[order(p$animal_id, p$start, p$logger_id), ]

  # Apply factors
  p$animal_id <- factor(p$animal_id, levels = animal_id)
  p$logger_id <- factor(p$logger_id, levels = logger_id)

  return(p)
}


# Calculate presence bouts for single animal
presence_single <- function(v1, bw = bw){

  v1 <- v1[order(v1$start),]
  v1$start_orig <- v1$start
  v1$end_orig <- v1$end
  diff_logger <- v1$logger_id[-1] != v1$logger_id[-nrow(v1)]
  v1$end <- v1$start <- FALSE

  if(!is.null(bw)){
    diff_time <- difftime(v1$start_orig[-1], v1$end_orig[-nrow(v1)], units = "min")
    v1$start[c(TRUE, (diff_time > bw | diff_logger))] <- TRUE
    v1$end[c((diff_time > bw | diff_logger), TRUE)] <- TRUE
  } else {
    v1$start[c(TRUE, diff_logger)] <- TRUE
    v1$end[c(diff_logger, TRUE)] <- TRUE
  }

  ## Create the presence data frame.
  p <- tibble::data_frame(animal_id = v1$animal_id[1],
                          date = as.Date(v1$start_orig[v1$start == TRUE]),
                          logger_id = v1$logger_id[v1$start == TRUE],
                          start = v1$start_orig[v1$start == TRUE],
                          end = v1$end_orig[v1$end == TRUE],
                          length = difftime(v1$end_orig[v1$end == TRUE], v1$start_orig[v1$start == TRUE], units = "mins"))

  return(p)
}


#' Displacements
#'
#' For an entire \code{visits} data frame, identifies displacement events.
#' Displacements are events when one animal leaves the logger right before the
#' arrival of another.
#'
#' The first and last visits on the record are automatically assumed to be
#' non-displacer and non-displacee, respectively.
#'
#' In some species displacements can be used to infer dominance. Displacements
#' can be passed to the \code{feedr} function \code{\link{dom}} to calculate
#' dominance matrices. Alternatively, the interaction data frame returned can be
#' passed directly to the \code{\link[Perc]{as.conflictmat}} function of the
#' \link{Perc} package to be transformed into a conflict matrix, ready for
#' analysis of dominance using percolation and conductance. Finally, the
#' displacements data frame can also be converted using the
#' \code{\link{convert_anidom}} function to a data frame for use by the
#' \link{aniDom} package's \link[aniDom]{elo_scores} function.
#'
#' @param v Dataframe. A visits data frame containing \strong{all} visits from
#'   \strong{all} animals. From the output of \code{visits}. Must contain columns
#'   \code{animal_id}, \code{logger_id}, \code{start}, and \code{end}.
#' @param bw Numeric. The maximum interval in seconds between visits by two
#'   different animals for the interaction to be considered a displacement.
#' @param pass Logical. Pass 'extra' columns through the function and append
#'   them to the output.
#'
#' @return A list with the following named items: \enumerate{ \item
#'   \code{displacements}: A data frame of individual displacement events,
#'   including the following columns: \itemize{ \item \code{logger_id}: ID of
#'   the logger at which the event occurred \item ID of the animal being displaced
#'   (\code{displacee}) \item ID of the animal doing the displacing
#'   (\code{displacer}) \item Time of the departure of the displacee
#'   (\code{left}) \item Time of the arrival of the displacer (\code{arrived}) }
#'
#'   \item \code{summaries}: A data frame of overall wins/lossess per
#'   individual, containing the following columns: \itemize{ \item ID of the
#'   animal (\code{animal_id}) \item No. of times the animal was displaced
#'   (\code{displacee}) \item No. of times the animal was a displacer
#'   (\code{displacer}) \item Proportion of wins (\code{p_win}) } \item
#'   \code{interactions}: A data frame of interaction summaries, containing the
#'   following columns: \itemize{ \item ID of the displacee (\code{displacee})
#'   \item ID of the displacer (\code{displacer}) \item No. of times this
#'   interaction occurred (\code{n}) } }
#'
#' @examples
#'
#' # Look at displacements for chickadees in experiment 2
#'  v <- visits(chickadees[chickadees$experiment == "exp2",])
#'  d <- disp(v)
#'
#'  # Look at displacement events:
#'  d[['displacements']] #or
#'  d$displacements
#'
#'  # Look at summaries (identical methods):
#'  d[['summaries']] #or
#'  d$summaries
#'
#'  # Look at interactions (identical methods):
#'  d[['interactions']] #or
#'  d$interactions
#'
#'  # Calculate across different experiments (expect warnings about unequal factor levels):
#' library(dplyr)
#'
#' v <- chickadees %>%
#'   group_by(experiment) %>%
#'   do(visits(.))
#'
#' d <- v %>%
#'   group_by(experiment) %>%
#'   do(data = disp(.))
#'
#' # Look at the data stored in the 2nd experiment:
#' d$data[d$experiment == "exp2"][[1]] #or
#' d[["data"]][[1]] #or
#' d$data[[1]]
#'
#' # Access the displacements from the 3rd experiment:
#' d$data[d$experiment == "exp3"][[1]]$displacements #or
#' d[["data"]][[2]]$displacements #or
#' d$data[[2]]$displacements
#'
#' @import magrittr
#' @export
disp <- function(v, bw = 5, pass = TRUE){

  ## Check for correct formatting
  check_name(v, c("animal_id", "logger_id", "date", "start", "end"))
  check_time(v)
  check_format(v)

  # Get factor levels for whole dataset
  if(is.factor(v$animal_id)) animal_id <- levels(v$animal_id) else animal_id <- unique(v$animal_id)
  if(is.factor(v$logger_id)) logger_id <- levels(v$logger_id) else logger_id <- unique(v$logger_id)

  v <- v[order(v$start), ]

  # Keep extra columns
  if(pass == TRUE) extra <- keep_extra(v, n = c("start", "end"))

  ## Define displacee and displacer by
  #  (a) whether subsequent visit was a different animal, AND
  #  (b) the arrival of the 2nd animal occurred within 'bw' seconds of the
  #      departure of the 1st
  #  (c) all of this occurs at the same logger
  diff_animal <- v$animal_id[-1] != v$animal_id[-nrow(v)]
  diff_time <- difftime(v$start[-1], v$end[-nrow(v)], units = "sec") < bw
  diff_logger <- v$logger_id[-1] == v$logger_id[-nrow(v)]

  d <- rbind(v[c(diff_animal & diff_time & diff_logger, FALSE), c("animal_id", "logger_id", "date", "start", "end")],
             v[c(FALSE, diff_animal & diff_time & diff_logger), c("animal_id", "logger_id", "date", "start", "end")])

  if(nrow(d) > 0) {
    d <- d[order(d$start), ]
    d$role <- c("displacee", "displacer")
    d <- d[order(d$role, d$start), ]

    d$left <- rep(v$end[c(diff_animal & diff_time & diff_logger, FALSE)], 2)
    d$arrived <- rep(v$start[c(FALSE, diff_animal & diff_time & diff_logger)], 2)

    d <- dplyr::select(d, animal_id, date, left, arrived, logger_id, role) %>%
      dplyr::arrange(left, logger_id, role)

    if(pass == TRUE) d <- merge_extra(d, extra)

    ## Summarize totals
    s <- d %>%
      dplyr::group_by(role, animal_id) %>%
      dplyr::summarize(n = length(animal_id)) %>%
      tidyr::complete(animal_id, role, fill = list("n" = 0)) %>%
      tidyr::spread(role, n) %>%
      dplyr::mutate(p_win = displacer / (displacee + displacer)) %>%
      dplyr::arrange(desc(p_win))

    ## Summarize interactions
    t <- d %>%
      dplyr::select(left, animal_id, role) %>%
      tidyr::spread(role, animal_id) %>%
      dplyr::group_by(displacer, displacee) %>%
      dplyr::summarize(n = length(displacee)) %>%
      dplyr::ungroup()

    t$displacee <- factor(t$displacee, levels = animal_id)
    t$displacer <- factor(t$displacer, levels = animal_id)

    t <- t %>%
      tidyr::complete(displacer, displacee, fill = list("n" = 0)) %>%
      dplyr::filter(displacee != displacer)

    t <- t[order(match(t$displacer, s$animal_id)),]  ##Sort according to the p_win value from s

    return(list("displacements" = d, "summaries" = s, "interactions" = t))
  } else {
    message("There are no displacement events with a bw = ", bw)
    return(list("displacements" = data.frame(), "summaries" = data.frame(), "interactions" = data.frame()))
  }
}

#' Dominance
#'
#' Takes output from \code{disp()} and calculates dominance hierarchies. Should
#' be considered experimental.
#'
#' @param d Data frame or List. Either the interactions data frame which is
#'   returned as a list item from \code{disp()}, or the whole displacements list
#'   returned by \code{disp()}.
#' @param tries Numeric. The maximum number of iterations to find the 'best guess'
#' @param omit_cutoff Numeric. Minimum number of interactions (sum of wins and
#'   losses) individuals must have (omitted otherwise).
#'
#' @return The best guess dominance hierarchies (there may be more than one).
#'
#' A list with the following named items:
#' \enumerate{
#'   \item \code{dominance}: A best guess at the dominance hierarchy (most to
#' least dominant) (one vector per 'best guess')
#'
#'   \item \code{reversals}: Which individuals show reversals (and with whom)? (i.e. A > B, B >
#'   C but C > A) (one data frame per 'best guess')
#'
#'   \item \code{interactions}: A matrix of dominance interactions. Displacers
#'   across the top, displacees down the side. Values are the numbers of wins
#'   (upper triangle) or losses (lower triangle) against the opposing
#'   individual. (one matrix per 'best guess')
#'
#'   }
#'
#' @examples
#'
#'  # Look at dominance for chickadees in experiment 2
#'  v <- visits(chickadees[chickadees$experiment == "exp2",])
#'  d <- disp(v)
#'  dm <- dom(d$interactions)
#'
#'  # But not necessary to specify interactions:
#'  dm <- dom(d)
#'
#'  # Calculate across different experiments (expect warnings about unequal factor levels):
#' library(dplyr)
#'
#' v <- chickadees %>%
#'   group_by(experiment) %>%
#'   do(visits(.))
#'
#' d <- v %>%
#'   group_by(experiment) %>%
#'   do(data = disp(.))
#'
#' dm <- d %>%
#'   group_by(experiment) %>%
#'   do(data = dom(.$data[[1]]))
#'
#' # Look at the dominance data stored in the 2nd experiment:
#' dm$data[dm$experiment == "exp2"][[1]] #or
#' dm[["data"]][[1]] #or
#' dm$data[[1]]
#'
#' # Look at the dominance matrices from 3nd experiment:
#' dm$data[dm$experiment == "exp3"][[1]]$matrices #or
#' dm[["data"]][[2]]$matrices #or
#' dm$data[[2]]$matrices
#'
#'
#' @export
dom <- function(d, tries = 50, omit_cutoff = 3){

  # Function takes either the whole output of disp() or just the dominance table
  if(!is.data.frame(d)) d <- d$interactions

  # Check for Correct formating
  check_name(d, c("displacer","displacee","n"), type = "displacement")
  check_format(d)

  # Start with best order
  o <- dplyr::left_join(dplyr::group_by(d, displacer) %>% dplyr::summarize(win = sum(n)),
                        dplyr::group_by(d, displacee) %>% dplyr::summarize(loss = sum(n)),
                        by = c("displacer" = "displacee")) %>%
    dplyr::mutate(win = replace(win, is.na(win), 0),
                  loss = replace(loss, is.na(loss), 0),
                  n = win + loss,
                  p_win = win / n) %>%
    dplyr::arrange(desc(p_win))

  # Check sample sizes and warn if low

  if(omit_cutoff > 0) {
    omit <- o$displacer[(o$win + o$loss) < omit_cutoff]
    o <- o[!(o$displacer %in% omit), ]
    d <- d[!(d$displacee %in% omit) & !(d$displacer %in% omit), ]
    if(length(omit) > 0) message("animal_ids with fewer than ", omit_cutoff, " interactions have been omitted: ", paste0(omit, collapse = ", "))
  }

  if((nrow(o) == 0 | nrow(d) == 0)) { # No individuals, return empty lists/dataframes
    message("No individuals remaining.")
    r <- list()
    m <- list()
    o_l <- vector()
  } else {

    o <- list(as.character(unique(o$displacer)))

    ## Setup the matrix
    dm <- tidyr::spread(d, displacee, n)
    dm <- as.matrix(dm[, -grep("^displacer$", names(dm))])
    rownames(dm) <- colnames(dm)

    ## Setup Loop
    try <- 0
    o_l <- o
    rev <- list()
    done <- FALSE  ## Are we done this iteration?
    prev <- vector()

    while(done == FALSE & try < tries){

      ## For each alternative dominance ranking (o_l)
      for(i in 1:length(o_l)){
        new_o <- o_l[[i]]
        temp <- dm

        ## Sort matrix by dominance hierarchy (new_o)
        temp <- temp[order(match(rownames(temp), new_o)), order(match(colnames(temp), new_o))]

        ## CHECK (TODO set to stop script if this doesn't work)
        all(is.na(diag(temp)))
        all(dimnames(temp)[[1]] == dimnames(temp)[[2]])

        ## Get upper and lower to compare
        upper <- temp
        upper[lower.tri(upper, diag = TRUE)] <- NA
        lower <- t(temp)
        lower[lower.tri(lower, diag = TRUE)] <- NA

        ## Get reversals
        if(length(which(upper < lower, arr.ind = TRUE)) > 0){
          rev[[length(rev) + 1]] <- which(upper < lower, arr.ind = TRUE)
        }
      }

      ## Keep only the orders with the fewest reversals
      if(length(rev) > 0){
        n <- sapply(rev, nrow)
        rev <- rev[n == min(n)]
        o_l <- o_l[n == min(n)]
      }

      # Compare with previous matrix, if the same, we're done
      if(identical(prev, o_l)) done <- TRUE else prev <- o_l

      if(length(rev) > 0 && length(rev[[1]]) > 0 && done == FALSE){
        ## Add the new reversal switches to our list of options and try again
        for(j in 1:length(rev)){
          for(i in 1:nrow(rev[[j]])){
            a <- rev[[j]][i,2]  ## Which individuals to move up
            b <- rev[[j]][i,1]  ## Where to move it
            o_l[[length(o_l) + 1]] <- c(new_o[1:(b - 1)], new_o[a], new_o[-c(1:(b - 1), a)])
          }
        }
      } else {
        done <- TRUE
        try <- try + 1
      }

      ## Loop controls
      if(done == FALSE){
        try <- try + 1
        o_l <- unique(o_l)
        rev <- list()
      }
    }

    message(paste0("Tried ",try," times. Found ",length(o_l), " 'best' matrix(ces), each with ",if(length(rev) > 0) nrow(rev[[1]]) else 0," reversal(s)"))

    m <- list()
    r <- list()
    for(i in 1:length(o_l)) {
      m[[length(m) + 1]] <- dm[order(match(rownames(dm), o_l[[i]])), order(match(colnames(dm), o_l[[i]]))]
      if(length(rev) > 0 && length(rev[[i]]) > 0) r[[length(r)+1]] <- data.frame(animal_id1 = o_l[[i]][rev[[i]][, 1]], animal_id2 = o_l[[i]][rev[[i]][, 2]])
    }
  }

  return(list(dominance = o_l, reversals = r, matrices = m))
}

#' Activity
#'
#' Calculate activity status (active vs. inactive) at a resolution of \code{res}
#' from \code{\link{presence}} data.
#'
#' A message will alert you to when the \code{res} is larger than the 50\% of
#' the presence bout lengths. This may result in missed activity, and it may be
#' better to choose a smaller \code{res}.
#'
#' The \code{missing} data frame should have columns \code{start} and \code{end}
#' corresponding to the start and end times of the missing data. Any activity between
#' those start/end times will be scored as unknown, regardless of the
#' \code{logger_id}. However, if \code{by_logger} is TRUE, \code{missing} may
#' also include the column \code{logger_id}. In this case, only activity for the
#' logger with the missing start/end times will be scored as unknown. If
#' \code{by_logger} is TRUE but \code{missing} does not contain the column
#' \code{logger_id}, all activity between the start and end times will be scored
#' as unknown, regardless of the logger. See examples.
#'
#' @param p Dataframe. A \code{\link{presence}} data frame (may contain multiple
#'   animal_ids).
#' @param res Numeric. The resolution over which to calculate activity in
#'   minutes.
#' @param by_logger Logical. Should the activity be calculated overall, or
#'   individually for each logger visited? If there is only one logger,
#'   by_logger will automatically revert to TRUE to enable passing of
#'   logger-related variables.
#' @param missing Data frame. (NOT AVAILABLE) If there are known times for a
#'   particular logger for which activity can't be recorded (i.e. times during
#'   which a logger was inactive).
#' @param sun Logical. Calculate sun rise/set? If by_logger = FALSE, returns
#'   median sun rise/set across all loggers for each day.
#' @param keep_all Logical. Keep all individuals, even ones with less than 24hrs of data.
#' @param pass Logical. Pass 'extra' columns through the function and append them to the output.
#' @param f Depreciated. Use \code{p}.
#'
#' @examples
#'
#' v <- visits(chickadees)
#' p <- presence(v)
#' a <- activity(p, res = 1)
#'
#' # By logger (may take a while)
#' \dontrun{
#' a <- activity(p, res = 1, by_logger = TRUE)
#'}
#'
#' @import magrittr
#' @export

activity <- function(p, res = 15, by_logger = FALSE, missing = NULL, sun = TRUE, keep_all = FALSE, pass = TRUE, f){

  if (!missing(f)) {
    warning("Argument f is deprecated; please use p instead.",
            call. = FALSE)
    p <- f
  }

  check_name(p, c("animal_id", "logger_id", "start", "end"), "presence")
  check_time(p, c("start", "end"))

  if(!is.null(missing)){
    message("missing argument not yet implemented")
    # if(!is.data.frame(missing)) {
    #   if(!is.character(missing) | length(missing) != 1) {
    #     stop("'missing' must be data frame or string with location of a csv file.")
    #   } else {
    #     missing <- utils::read.csv(missing)
    #   }
    # }

    #if(sum(names(missing) %in% c("start", "end")) != 2) stop("'missing' must have columns 'start' and 'end'.")

    #missing$start <- lubridate::parse_date_time(missing$start, orders = "%Y-%m-%d %H:%M:%S", truncated = 3, tz = tz)
    #missing$end <- lubridate::parse_date_time(missing$end, orders = "%Y-%m-%d %H:%M:%S", truncated = 3, tz = tz)

    # if(any(!lubridate::is.POSIXct(c(missing$start, missing$end)))) {
    #   stop("'missing' start or end cannot be converted to date/time, be sure it is in a standard date/time format (YYYY-MM-DD HH:MM:SS is best).")
    # }
  }

  # Get factor levels for whole dataset
  if(is.factor(p$animal_id)) animal_id <- levels(p$animal_id) else animal_id <- unique(p$animal_id)
  loggers <- unique(tibble::as_tibble(p[, names(p) %in% c("logger_id", "lat", "lon")]))

  # Keep extra cols
  if(pass) {
    if(by_logger == FALSE) only <- c("animal_id", "date") else only <- c("logger_id", "animal_id", "date")
    extra <- keep_extra(p, n = c("start", "end", "length"), only = only)
  }

  if(any(!lubridate::is.POSIXct(c(p$start, p$end)))) {
    stop("Cannot define start and end times of the presence data set, make sure this is the output from presence().")
  }


  # Apply individually to each animal
  a <- p %>%
    dplyr::group_by(animal_id) %>%
    dplyr::do(activity_single(., loggers = loggers, res = res, by_logger = by_logger, missing = missing, sun = sun, keep_all = keep_all)) %>%
    dplyr::ungroup()

  if(pass) a <- merge_extra(a, extra)

  a <- dplyr::arrange(a, animal_id, date, time, logger_id)

  # Apply factors
  a$animal_id <- factor(a$animal_id, levels = animal_id)
  a$logger_id <- factor(a$logger_id, levels = levels(loggers$logger_id))

  return(a)

}


activity_single <- function(p1, loggers, res = 15, by_logger = FALSE, missing = NULL, sun = TRUE, keep_all = FALSE){

  check_indiv(p1)

  if(nrow(p1) == 0) {
    message(paste0(p1$animal_id[1], ": Skipping. Individual has no data"))
    return(tibble::data_frame())
  } else {


    # Grab the timezone
    tz <- attr(p1$start, "tzone")

    start <- lubridate::floor_date(min(p1$start), "day")
    end <- lubridate::ceiling_date(max(p1$end), "day")

    # Calculate Activity only if > 24hrs of data
    if((max(p1$end) - min(p1$start)) < lubridate::dhours(24) & keep_all == FALSE) {
      message(paste0(p1$animal_id[1], ": Skipping. Individual has less than 24hrs of data"))
      return(tibble::data_frame())
    } else if (all(p1$length == 0))  {
      message(paste0(p1$animal_id[1], ": Skipping. All bouts are 0 min. Consider increasing 'bw' in presence()"))
      return(tibble::data_frame())
    } else {
      ## ACCOUNT FOR MISSING!!!

      # Check proportion of time active, warn if really low
      p_active <- as.numeric(sum(p1$length)) / as.numeric(difftime(max(p1$end), min(p1$start), units = "mins"))
      if(p_active < 0.05) message(paste0(p1$animal_id[1], ": Active less than 5% of the total time period..."))

      # Override by_logger if only one logger_id to keep extra columns
      #if(length(unique(p1$logger_id)) == 1) by_logger <- TRUE

      # Get activity
      prob <- round(length(p1$length[p1$length < res]) / nrow(p1) * 100, 2)
      if(prob > 50) {
        message(paste0(p1$animal_id[1], ": ", prob, "% of obs are shorter than 'res' (", res, " min). Median obs is ", round(median(p1$length), 2), " min."))
      }

      # Prep activity data frame
      res <- res * 60
      a <- tibble::data_frame(
        animal_id = p1$animal_id[1],
        time = seq(start, end, by = paste0(res, " sec")),
        activity_c = factor("inactive",
                            levels = c("active", "inactive", "unknown")))

      a$date <- as.Date(lubridate::floor_date(a$time, unit = "day"))

      # Get by individual only, or by individual for each logger
      if(by_logger == FALSE){
        a$logger_id <- NA
      } else {
        temp <- tibble::data_frame()
        for(i in levels(loggers$logger_id)){
          temp <- rbind(temp, cbind(a, logger_id = i))
        }
        a <- temp
      }

      # Fill with active/inactive
      for(p_id in unique(p1$logger_id)){
        p <- p1[p1$logger_id == p_id, ]
        for(i in 1:nrow(p)) {
          if(by_logger == FALSE) {
            a$activity_c[a$time >= p$start[i] & a$time <= p$end[i]] <- "active"
          } else {
            a$activity_c[a$logger_id == p_id & a$time >= p$start[i] & a$time <= p$end[i]] <- "active"
          }
        }
      }

      # if(!is.null(missing)) {
      #   for(i in 1:nrow(missing)){
      #     if(by_logger == FALSE){
      #       a$activity_c[a$time >= missing$start[i] & a$time <= missing$end[i]] <- "unknown"
      #     } else {
      #       a$activity_c[a$logger_id == missing$logger_id[i] & a$time >= missing$start[i] & a$time <= missing$end[i]] <- "unknown"
      #     }
      #   }
      # }

      # Create plotting column
      a$activity <- as.numeric(a$activity_c == "active")
      a$activity[a$activity_c == "unknown"] <- NA

      # Calculate sunrise/sunset times
      if(sun == TRUE) {
        if(!all(c("lat", "lon") %in% names(p1))) {
          message(paste0(p1$animal_id[1], ": Skipping sunrise/sunset, no lat/lon information"))
        } else {

          s <- expand.grid(logger_id = loggers$logger_id,
                           date = as.Date(seq(start, end, by = "1 day"))) %>%
            dplyr::left_join(unique(loggers[, c("logger_id", "lon", "lat")]), by = "logger_id")

          s <- dplyr::bind_cols(s, sun(s[, c("lon", "lat")], s$date, tz = tz))

          if(by_logger == TRUE) {
            a <- dplyr::left_join(a, s[, c("logger_id", "date", "rise", "set")],
                                  by = c("logger_id", "date"))
          } else {
            s <- s %>%
              dplyr::group_by(date) %>%
              dplyr::summarize(rise = median(rise),
                               set = median(set))
            a <- dplyr::left_join(a, s[, c("date", "rise", "set")], by = "date")
          }
        }
      }

      # Select
      n <- c("animal_id", "date", "time", "activity", "activity_c", "logger_id", "rise", "set")
      n <- n[n %in% names(a)]
      a <- dplyr::select_(a, .dots = n)

      return(a)
    }
  }
}

#' Daily activity
#'
#' Summarizes and averages activity data over a 24-hr period, generating a 24-hr
#' daily activity pattern for plotting. The resulting data set contains four
#' columns reflecting the proportions of time blocks scored as active, inactive,
#' unknonw, or total.
#'
#' Output dates are irrelevant, as the data is tied to times, not
#' dates. Therefore the dates are all assigned to 1970-01-01. When
#' plotting, omit the date part of the label to accurately portray time only.
#'
#' Resolution of the data is automatically detected as the same as that
#' specified in \code{activity()}.
#'
#'
#' @param a Data frame. Data from output of \code{activity()}.
#' @param pass Logical. Pass 'extra' columns through the function and append them to the output.
#'
#' @export

daily <- function(a, pass = TRUE){

  check_name(a, c("animal_id", "date", "time", "activity", "activity_c", "logger_id"), "activity")
  check_time(a, c("time"))

  # Get extra
  if(pass){
    if(all(is.na(a$logger_id))) only <- "animal_id" else only <- c("logger_id", "animal_id")
    extra <- keep_extra(a, c("time", "activity", "activity_c"), only = only)
  }

  a$time_c <- format(a$time, "%H:%M:%S")

  # Apply single function

  d <- a %>%
    dplyr::group_by(animal_id) %>%
    dplyr::do(daily_single(., pass = pass)) %>%
    dplyr::ungroup()

  if(pass) d <- merge_extra(d, extra)


  return(d)
}

daily_single <- function(a1, pass = TRUE){

  check_indiv(a1)

  # Grab the timezone
  tz <- attr(a1$time, "tzone")

  d <- a1 %>%
    dplyr::group_by(animal_id, logger_id, time_c) %>%
    dplyr::summarize(p_active = length(activity_c[activity_c == "active"]) / length(activity_c[activity_c != "unknown"]),
                     p_inactive = length(activity_c[activity_c == "inactive"]) / length(activity_c[activity_c != "unknown"]),
                     p_unknown = length(activity_c[activity_c == "unknown"]) / length(activity_c),
                     p_total = 1 - p_unknown)


  d$time <- as.POSIXct(paste0(lubridate::origin, " ", d$time_c), tz = tz)
  #lubridate::tz(d$time) <- "UTM"

  # Get sun/rise set if exist, and average
  if(all(c("rise", "set") %in% names(a1))) {
    sun <- unique(a1[, c("date", "logger_id", "rise", "set")])

    sun <- sun %>%
      dplyr::group_by(logger_id) %>%
      dplyr::summarize(rise = mean_clock(rise, origin = TRUE),
                       set = mean_clock(set, origin = TRUE))
    d <- merge(d, sun, by = "logger_id", all.x = TRUE, all.y = FALSE)
  }

  # Order
  n <- c("animal_id", "time", "time_c", "p_active", "p_inactive", "p_unknown", "p_total", "logger_id", "rise", "set")
  n <- n[n %in% names(d)]

  d <- dplyr::select_(d, .dots = n) %>%
    dplyr::arrange(animal_id, time)

  return(d)
}

#' Get sunrise/sunset times
#'
#' Calculate times of sunrise and sunset depending on the location and the date.
#'
#' @param loc Vector/Data frame. Longitude and Latitude coordinates for location
#'   of sun rise/set
#' @param date Vector. Date(s) to cacluate sun rise/set for.
#' @param tz Timezone of the dates.
#'
#' @export
sun <- function(loc, date, tz) {
  if(class(loc) == "numeric") loc <- matrix(loc, nrow = 1)
  if(class(loc) %in% c("data.frame", "matrix")) loc <- as.matrix(loc)
  date <- as.POSIXct(as.character(date), tz = tz)
  s <- data.frame(rise = maptools::sunriset(loc, date, direction = "sunrise", POSIXct.out = TRUE)$time,
                  set = maptools::sunriset(loc, date, direction = "sunset", POSIXct.out = TRUE)$time)

  return(s)
}
