#' 'Raw' data to 'visits' data
#'
#'
#' Raw data from RFID feeders contain multiple reads per individual simply
#' because the individual sat there long enough. In \code{visits()} these reads
#' are collapsed into one visit.
#'
#' Visits are defined by three pieces of data:
#' \itemize{
#' \item How much time has passed between reads (\code{bw})
#' \item A change in identity between two successive reads (\code{bird_id})
#' \item A change in feeder for a single individual (\code{feeder_id})
#' }
#'
#' The function will return an error if impossible visits are detected (unless
#'   \code{allow_imp = TRUE}) . A visit is deemed impossible if a single bird
#'   travels between feeders in less time than specified by \code{bw}.
#'
#' @param r Dataframe. Contains raw reads from an RFID reader with colums
#'   \code{bird_id}, \code{feeder_id}, \code{time}.
#' @param bw Numerical. The minimum interval, in seconds, between reads for two
#'   successive reads to be considered separate visits.
#' @param allow_imp Logical. Whether impossible visits should be allowed (see
#'   details).
#' @param na_rm Logical. Whether NA values should be automatically omited.
#'   Otherwise an error is returned.
#' @param pass Logical. Pass 'extra' columns through the function and append
#'   them to the output.
#' @param allow.imp,na.rm Depreciated.
#'
#' @return A data frame with visits specifying \code{bird_id} and
#'   \code{feeder_id} as well as the \code{start} and \code{end} of the visit.
#'   Any extra columns that are unique at the level of bird_id or feeder_id will
#'   also be passed through (i.e. age, sex, feeder location, etc.).
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

#' @import magrittr
#' @export
visits <- function(r, bw = 3, allow_imp = FALSE, na_rm = FALSE, pass = TRUE, allow.imp, na.rm){
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
  check_name(r, n = c("bird_id", "feeder_id", "time"), "raw RFID")
  check_time(r, n = "time", internal = FALSE)
  check_format(r)

  # Check for NAs, remove if specified by na_rm = TRUE
  if(any(is.na(r[, c("bird_id", "feeder_id", "time")]))){
    if(na_rm == FALSE) stop("NAs found. To automatically remove NAs, specify 'na_rm = TRUE'.")
    if(na_rm == TRUE) r <- r[rowSums(is.na(r)) == 0,]
  }

  ## Make factors and get date
  r <- dplyr::mutate(r,
                     date = as.Date(time),
                     feeder_id = factor(feeder_id),
                     bird_id = factor(bird_id))

  # Grab unique extra cols
  if(pass == TRUE) extra <- keep_extra(r, n = "time")

  # Grab the timezone
  tz <- attr(r$time, "tzone")

  # Get spacing between visits, whether same bird or not, and whether same feeder or not
  r <- r[order(r$time),]
  diff_time <- (r$time[-1] - r$time[-nrow(r)]) > bw
  diff_bird <- r$bird_id[-nrow(r)] != r$bird_id[-1]
  diff_feeder <- r$feeder_id[-nrow(r)] != r$feeder_id[-1]

  # Check for impossible combos: where less than bw, still the same bird, but a different feeder
  if(!allow_imp) {
    impos <- which(rowSums(matrix(c(!diff_time, !diff_bird, diff_feeder), ncol = 3)) > 2)
    impos <- r[unique(c(impos, impos + 1)), ]
    if(nrow(impos) > 0) {
      impos <- impos[order(impos$bird_id, impos$time), ]
      end <- "hellow"
      rows <- nrow(impos)
      if(nrow(impos) > 5) {
        rows <- 5
        end <- "\n..."
      }
      stop("Impossible visits found, no specification for how to handle.\nTime between reads is less than 'bw' (", bw, "s) for a single individual, yet the\nreads occur at different feeders. You should fix, remove or\nallow (allow_imp = TRUE) these rows and try again.\n\n", paste0(capture.output(impos[1:rows, ]), collapse = "\n"))
    }
  }
  # Start if
  # - time before is greater than 'bw' OR
  # - bird before is not the same OR
  # - feeder before is not the same
  # End if
  # - time after is great than 'bw' OR
  # - bird after is not the same OR
  # - feeder after is not the same

  new_visit <- apply(cbind(diff_time, diff_bird, diff_feeder), 1, any)
  r$end <- r$start <- as.POSIXct(NA, tz = tz)
  r$start[c(TRUE, new_visit)] <- r$time[c(TRUE, new_visit)]
  r$end[c(new_visit, TRUE)] <- r$time[c(new_visit, TRUE)]

  # Get visits
  v <- r %>%
    dplyr::filter(!(is.na(start) & is.na(end))) %>%
    dplyr::select(feeder_id, bird_id, start, end) %>%
    tidyr::gather(variable, value, start, end) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::arrange(value) %>%
    dplyr::group_by(feeder_id, bird_id, variable) %>%
    dplyr::mutate(n = 1:length(value)) %>%
    tidyr::spread(variable, value) %>%
    dplyr::select(-n) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(bird_n = length(unique(bird_id)),         # Get sample sizes
                  feeder_n = length(unique(feeder_id)),
                  date = as.Date(start))

  # Set timezone attributes
  attr(v$start, "tzone") <- tz
  attr(v$end, "tzone") <- tz

  # Order data frame
  v <- v %>%
    dplyr::select(bird_id, date, start, end, feeder_id, bird_n, feeder_n) %>%
    dplyr::arrange(bird_id, start)

  # Add in extra variables
  if(pass == TRUE) v <- merge_extra(v, extra)

  return(v)
}


#' 'Visits' to 'movements'
#'
#' Turns visits to mulitple feeders into movements between feeders
#'
#' @param v Dataframe. A visits data frame (may contain multiple bird_ids). From
#'   the output of \code{visits}. Must contain columns \code{bird_id},
#'   \code{feeder_id}, \code{start}, and \code{end}.
#' @param all Logical. Should all bird_ids be returned, even if the bird made no
#'   movements? If TRUE, a data frame with NAs for all columns except
#'   \code{bird_id} will be returned, if FALSE, an empty data frame will be
#'   returned.
#' @param pass Logical. Pass 'extra' columns through the function and append
#'   them to the output.
#'
#' @return A data frame of movements. These are defined as the bout of time from
#'   leaving one feeder to arriving at a second one. Each movement bout consists
#'   of two rows of data containing:
#'   \itemize{
#'   \item ID of the bird (\code{bird_id})
#'   \item Date of event (\code{date})
#'   \item Time of event (\code{time})
#'   \item The ID of feeders involved (\code{feeder_id})
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
#'   group_by(bird_id, move_path) %>%
#'   summarize(n_path = length(move_path) /2)
#'
#' # Calculate across different experiments:
#' library(dplyr)
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
  check_name(v, c("bird_id", "feeder_id", "date", "start", "end"))
  check_time(v)
  check_format(v)

  # Get extra columns
  if(pass == TRUE) extra <- keep_extra(v, n = c("start", "end"))

  # Get factor levels for whole dataset
  if(is.factor(v$bird_id)) bird_id <- levels(v$bird_id) else bird_id <- unique(v$bird_id)
  if(is.factor(v$feeder_id)) feeder_id <- levels(v$feeder_id) else feeder_id <- unique(v$feeder_id)

  # Get movement options
  move_dir <- expand.grid(feeder_left = feeder_id, feeder_arrived = feeder_id)
  move_dir <- move_dir[move_dir$feeder_left != move_dir$feeder_arrived,]
  move_dir <- paste0(move_dir$feeder_left, "_", move_dir$feeder_arrived)
  move_path <- unique(sapply(move_dir, FUN = mp))

  # Apply individually to each bird
  m <- v %>%
    dplyr::group_by(bird_id) %>%
    dplyr::do(move_single(., move_dir = move_dir, move_path = move_path, all = all)) %>%
    dplyr::ungroup()

  if(nrow(m) > 0){
    # Order
    m <- m %>%
      dplyr::select(bird_id, date, time, feeder_id, direction, move_id, move_dir, move_path, strength) %>%
      dplyr::arrange(bird_id, time)

    # Add in extra cols
    if(pass == TRUE) m <- merge_extra(m, extra)

    # Apply factors
    m$bird_id <- factor(m$bird_id, levels = bird_id)
    m$feeder_id <- factor(m$feeder_id, levels = feeder_id)
  }
  return(m)
}

# Movement function for single bird
move_single <- function(v1, move_dir, move_path, all = FALSE){

  if(length(unique(v1$feeder_id)) > 1) { # Only proceed if there are actual data!

    # If there are movements, calculate events
    v1 <- v1[, c("bird_id", "date", "start", "end", "feeder_id")]
    v1 <- v1[order(v1$start),]
    diff <- v1$feeder_id[-1] != v1$feeder_id[-nrow(v1)]
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
      dplyr::mutate(move_id = sort(rep(1:(length(bird_id)/2),2))) %>%
      dplyr::group_by(move_id) %>%
      dplyr::mutate(move_dir = paste0(feeder_id, collapse = "_"),
                    move_path = factor(paste0(sort(feeder_id), collapse = "_"), levels = move_path),
                    strength = 1 / as.numeric(difftime(time[direction == "arrived"], time[direction == "left"], units = "hours"))) %>%
      dplyr::ungroup()
  } else if (all == TRUE) {
    # Create the movement data frame for birds that didn't move between feeders
    m <- tibble::data_frame(bird_id = v1$bird_id[1],
                            date = as.Date(NA),
                            time = as.POSIXct(NA),
                            feeder_id = NA,
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

#' 'Visits' to 'feeding bouts'
#'
#' Turns visits at mulitple feeders into feeding bouts. Feeding bouts are
#' separated by switching feeders (when \code{bw = NULL}). If \code{bw} is not
#' \code{NULL}, they are separated by switching feeders or by \code{bw} minutes.
#'
#'
#' @param v Dataframe. A visits data frame from the output of \code{visits} (may
#'   contain more than one bird_id). Must contain columns \code{bird_id},
#'   \code{feeder_id}, \code{start}, and \code{end}.
#' @param bw Numeric. The minimum number of minutes between visits for two
#'   successive visits to be considered separate feeding bouts. When \code{bw} =
#'   NULL only visits to another feeder are scored as a separate feeding bout.
#' @param pass Logical. Pass 'extra' columns through the function and append
#'   them to the output.
#'
#' @return A data frame of feeding bouts. A feeding bout is defined as the bout
#'   of time spent making regular visits to a feeder, in which each visit
#'   occurrred within some cutoff time of the last. This data frame has the
#'   following columns:
#'   \itemize{
#'   \item ID of the bird (\code{bird_id})
#'   \item ID of the feeder(\code{feeder_id})
#'   \item Time of the start of the feeding bout (\code{feed_start})
#'   \item Time of the end of the feeding bout (\code{feed_end})
#'   }
#'
#' @examples
#'
#' v <- visits(finches)
#' f <- feeding(v)
#'
#' # Summarize a movement dataframe (get total time spent feeding)
#' library(dplyr)
#' f.totals <- f %>%
#'     group_by(bird_id, feeder_id) %>%
#'     summarize(feed_length = sum(feed_length))
#'
#' # Calculate across different experiments (expect warnings about unequal factor levels):
#' library(dplyr)
#'
#' v <- chickadees %>%
#'   group_by(experiment) %>%
#'   do(visits(.))
#'
#' f <- v %>%
#'   group_by(experiment) %>%
#'   do(feeding(.))
#'
#'
#' @export

feeding <- function(v, bw = 15, pass = TRUE){

  ## Check for correct formatting
  check_name(v, c("bird_id","feeder_id", "date", "start","end"))
  check_time(v)
  check_format(v)

  # Remove factors for now
  v$bird_id <- as.character(v$bird_id)
  v$feeder_id <- as.character(v$feeder_id)

  # Keep extra cols
  if(pass == TRUE) extra <- keep_extra(v, n = c("start", "end"))

  # Get factor levels for whole dataset
  if(is.factor(v$bird_id)) bird_id <- levels(v$bird_id) else bird_id <- unique(v$bird_id)
  if(is.factor(v$feeder_id)) feeder_id <- levels(v$feeder_id) else feeder_id <- unique(v$feeder_id)

  # Calculate for each individual
  f <- v %>%
    dplyr::group_by(bird_id) %>%
    dplyr::do(feeding_single(., bw = bw)) %>%
    dplyr::ungroup()

  # Get extra columns and add in
  if(pass == TRUE) f <- merge_extra(f, extra)
  f <- f[order(f$bird_id, f$feed_start, f$feeder_id), ]

  # Apply factors
  f$bird_id <- factor(f$bird_id, levels = bird_id)
  f$feeder_id <- factor(f$feeder_id, levels = feeder_id)

  return(f)
}


# Calculate feeding bouts for single bird
feeding_single <- function(v1, bw = 15){

  v1 <- v1[order(v1$start),]
  feeder_diff <- v1$feeder_id[-1] != v1$feeder_id[-nrow(v1)]
  v1$feed_end <- v1$feed_start <- FALSE

  if(!is.null(bw)){
    time_diff <- v1$start[-1] - v1$end[-nrow(v1)]
    v1$feed_start[c(TRUE, (time_diff > bw*60 | feeder_diff))] <- TRUE
    v1$feed_end[c((time_diff > bw*60 | feeder_diff), TRUE)] <- TRUE
  } else {
    v1$feed_start[c(TRUE, feeder_diff)] <- TRUE
    v1$feed_end[c(feeder_diff, TRUE)] <- TRUE
  }

  ## Create the feeding data frame.
  f <- tibble::data_frame(bird_id = v1$bird_id[1],
                          date = as.Date(v1$start[v1$feed_start == TRUE]),
                          feeder_id = v1$feeder_id[v1$feed_start == TRUE],
                          feed_start = v1$start[v1$feed_start == TRUE],
                          feed_end = v1$end[v1$feed_end == TRUE],
                          feed_length = difftime(v1$end[v1$feed_end == TRUE], v1$start[v1$feed_start == TRUE], units = "mins"))

  return(f)
}


#' 'Visits' to 'displacements'
#'
#' For an entire \code{visits} data frame, identifies displacement events.
#' Displacements are events when one bird is forced to leave the feeder due to
#' the imminent arrival of a more dominant bird.
#'
#' The first and last visits on the record are automatically assumed to be non
#' displacer and non-displacee, respectively.
#'
#' @param v Dataframe. A visits data frame containing \strong{all} visits from
#'   \strong{all} birds. From the output of \code{visits}. Must contain columns
#'   \code{bird_id}, \code{feeder_id}, \code{start}, and \code{end}.
#' @param bw Numeric. The maximum interval in seconds between visits by two
#'   different birds for the interaction to be considered a displacement.
#' @param pass Logical. Pass 'extra' columns through the function and append
#'   them to the output.
#'
#' @return A list with the following named items: \enumerate{ \item
#'   \code{displacements}: A data frame of individual displacement events,
#'   including the following columns: \itemize{ \item \code{feeder_id}: ID of
#'   the feeder at which the event occurred \item ID of the bird being displaced
#'   (\code{displacee}) \item ID of the bird doing the displacing
#'   (\code{displacer}) \item Time of the departure of the displacee
#'   (\code{left}) \item Time of the arrival of the displacer (\code{arrived}) }
#'
#'   \item \code{summaries}: A data frame of overall wins/lossess per
#'   individual, containing the following columns: \itemize{ \item ID of the
#'   bird (\code{bird_id}) \item No. of times the bird was displaced
#'   (\code{displacee}) \item No. of times the bird was a displacer
#'   (\code{displacer}) \item Proportion of wins (\code{p_win}) } \item
#'   \code{interactions}: A data frame of interaction summaries, containing the
#'   following columns: \itemize{ \item ID of the displacee (\code{displacee})
#'   \item ID of the displacer (\code{displacer}) \item No. of times this
#'   interaction occurred (\code{n}) } }
#'
#' @examples
#'  v <- visits(chickadees)
#'  d <- disp(v)
#'
#'  # Look at displacement events (identical methods):
#'  d[['displacements']][1:5,]
#'  d$displacements[1:5,]
#'
#'  # Look at summaries (identical methods):
#'  d[['summaries']][1:10,]
#'  d$summaries[1:10,]
#'
#'  # Look at interactions (identical methods):
#'  d[['interactions']][1:10,]
#'  d$interactions[1:10,]
#' @import magrittr
#' @export
disp <- function(v, bw = 5, pass = TRUE){

  ## Check for correct formatting
  check_name(v, c("bird_id", "feeder_id", "date", "start", "end"))
  check_time(v)
  check_format(v)

  # Get factor levels for whole dataset
  if(is.factor(v$bird_id)) bird_id <- levels(v$bird_id) else bird_id <- unique(v$bird_id)
  if(is.factor(v$feeder_id)) feeder_id <- levels(v$feeder_id) else feeder_id <- unique(v$feeder_id)

  v <- v[order(v$start), ]

  # Keep extra columns
  if(pass == TRUE) extra <- keep_extra(v, n = c("start", "end"))

  ## Define displacee and displacer by
  #  (a) whether subsequent visit was a different bird, AND
  #  (b) the arrival of the 2nd bird occurred within 'bw' seconds of the
  #      departure of the 1st
  #  (c) all of this occurs at the same feeder
  bird_diff <- v$bird_id[-1] != v$bird_id[-nrow(v)]
  time_diff <- (v$start[-1] - v$end[-nrow(v)]) < bw
  feeder_diff <- v$feeder_id[-1] == v$feeder_id[-nrow(v)]

  d <- rbind(v[c(bird_diff & time_diff & feeder_diff, FALSE), c("bird_id", "feeder_id", "date", "start", "end")],
             v[c(FALSE, bird_diff & time_diff & feeder_diff), c("bird_id", "feeder_id", "date", "start", "end")])

  if(nrow(d) == 0) stop(paste0("There are no displacement events with a bw = ", bw, ", stopping now"))

  d <- d[order(d$start), ]
  d$role <- c("displacee", "displacer")
  d <- d[order(d$role, d$start), ]

  d$left <- rep(v$end[c(bird_diff & time_diff & feeder_diff, FALSE)], 2)
  d$arrived <- rep(v$start[c(FALSE, bird_diff & time_diff & feeder_diff)], 2)

  d <- dplyr::select(d, bird_id, date, left, arrived, feeder_id, role) %>%
    dplyr::arrange(left, feeder_id, role)

  if(pass == TRUE) d <- merge_extra(d, extra)

  ## Summarize totals
  s <- d %>%
    dplyr::group_by(role, bird_id) %>%
    dplyr::summarize(n = length(bird_id)) %>%
    tidyr::complete(bird_id, role, fill = list("n" = 0)) %>%
    tidyr::spread(role, n) %>%
    dplyr::mutate(p_win = displacer / (displacee + displacer)) %>%
    dplyr::arrange(desc(p_win))

  ## Summarize interactions
  t <- d %>%
    dplyr::select(left, bird_id, role) %>%
    tidyr::spread(role, bird_id) %>%
    dplyr::group_by(displacer, displacee) %>%
    dplyr::summarize(n = length(displacee)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(displacer, displacee, fill = list("n" = 0)) %>%
    dplyr::filter(displacee != displacer)

  t <- t[order(match(t$displacer,s$bird_id)),]  ##Sort according to the p_win value from s

  return(list("displacements" = d, "summaries" = s, "interactions" = t))
}

#' 'displacements' to 'dominance'
#'
#' @param d Data frame or List. Either the interactions data frame which is
#'   returned as a list item from \code{disp()}, or the whole displacements list
#'   returned by \code{disp()}.
#' @param tries Numeric. The maximum number of iterations to find the 'best guess'
#' @param omit_zero Logical. Should individuals with 0 interactions (sum of wins
#'   and losses) be omitted?
#'
#' @export
dom <- function(d, tries = 50, omit_zero = TRUE){

  # Function takes either the whole output of disp() or just the dominance table
  if(!is.data.frame(d)) d <- d$interactions

  # Check for Correct formating
  check_name(d, c("displacer","displacee","n"), type = "displacement")
  check_format(d)

  # Start with best order
  o <- dplyr::left_join(dplyr::group_by(d, displacer) %>% dplyr::summarize(win = sum(n)),
                        dplyr::group_by(d, displacee) %>% dplyr::summarize(loss = sum(n)),
                        by = c("displacer" = "displacee")) %>%
    dplyr::mutate(p_win = win / (win + loss)) %>%
    dplyr::arrange(desc(p_win))

  # Check sample sizes and warn if low

  if(omit_zero == TRUE) {
    omit <- o$displacer[(o$win + o$loss) == 0]
    o <- o[!(o$displacer %in% omit), ]
    d <- d[!(d$displacee %in% omit) & !(d$displacer %in% omit), ]
    if(length(omit) > 0) message("bird_ids with zero interactions have been omitted: ", paste0(omit, collapse = ", "))
  }

  if(nrow(o[(o$win + o$loss) <= 2, ]) / nrow(o) > 0.5) message("More than 50% of your interactions (", round(nrow(d[d$n == 0, ]) / nrow(d)*100), "%) have 2 or fewer observations, this matrix may be founded on too little data.")

  o <- list(as.character(unique(o$displacer)))

  ## Setup the matrix
  d <- tidyr::spread(d, displacee, n)
  d <- as.matrix(d[,-grep("^displacer$", names(d))])
  rownames(d) <- colnames(d)

  ## Setup Loop
  try <- 0
  o_l <- o
  rev <- list()
  done <- FALSE  ## Are we done this iteration?
  prev <- vector()

  while(done == FALSE & try < tries){

    for(i in 1:length(o_l)){
      new_o <- o_l[[i]]
      temp <- d

      ## Sort matrix by dominance hierarchy (new_o)
      temp <- temp[order(match(rownames(temp),new_o)),order(match(colnames(temp),new_o))]

      ## CHECK (TODO set to stop script if this doesn't work)
      all(diag(temp)==0)
      all(dimnames(temp)[[1]] == dimnames(temp)[[2]])

      ## Get upper and lower to compare
      upper <- temp
      upper[lower.tri(upper, diag = TRUE)] <- NA
      lower <- t(temp)
      lower[lower.tri(lower, diag = TRUE)] <- NA

      ## Get reversals
      if(length(which(upper < lower, arr.ind = TRUE)) > 0){
        rev[[length(rev)+1]] <- which(upper < lower, arr.ind = TRUE)
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
          o_l[[length(o_l)+1]] <- c(new_o[1:(b-1)], new_o[a], new_o[-c(1:(b-1), a)])
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
  #print(paste0("Started with: ",paste0(unlist(o), collapse = ", ")))
  message(paste0("Tried ",try," times. Found ",length(o_l), " 'best' matrix(ces), with ",if(length(rev) > 0) nrow(rev[[1]]) else 0," reversal(s) per matrix"))

  m <- list()
  r <- list()
  for(i in 1:length(o_l)) {
    m[[length(m) + 1]] <- d[order(match(rownames(d), o_l[[i]])), order(match(colnames(d), o_l[[i]]))]
    if(length(rev) >0 && length(rev[[i]]) > 0) r[[length(r)+1]] <- c(rownames(m[[i]])[rev[[i]][1]], colnames(m[[i]])[rev[[i]][2]])
  }

  return(list(dominance = o_l, reversals = r, matrices = m))
}


