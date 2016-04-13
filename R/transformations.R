# Grab extra columns unique only
keep.extra <- function(d, n, only = c("bird_id", "feeder_id")){

  d <- unique(d[, setdiff(names(d), n)])

  # If loc present, deconstruct
  if(any(names(d) == "loc")){
    d$lon <- as.numeric(gsub("\\(([-0-9.]+),[-0-9.]+\\)", "\\1", d$loc))
    d$lat <- as.numeric(gsub("\\([-0-9.]+,([-0-9.]+)\\)", "\\1", d$loc))
    d <- d[, names(d) != "loc",]
  }

  if("bird_id" %in% only) bird_id <- names(which(lapply(d[, !(names(d) %in% c("bird_id", "feeder_id"))], FUN = function(x) nrow(unique(cbind(d$bird_id, x)))) == length(unique(d$bird_id)))) else bird_id <- NULL
  if("feeder_id" %in% only) feeder_id <- names(which(lapply(d[, !(names(d) %in% c("bird_id", "feeder_id"))], FUN = function(x) nrow(unique(cbind(d$feeder_id, x)))) == length(unique(d$feeder_id)))) else feeder_id <- NULL

  if(all(c("feeder_id", "bird_id") %in% only)) {
    both <- intersect(bird_id, feeder_id)
    feeder_id <- setdiff(setdiff(feeder_id, both), bird_id)
    bird_id <- setdiff(setdiff(bird_id, both), feeder_id)
  } else {
    both <- NULL
  }

  if(length(both) > 0) both <- unique(d[, c("bird_id", "feeder_id", both)]) else both <- NULL
  if(length(bird_id) > 0) bird_id <- unique(d[, c("bird_id", bird_id)]) else bird_id <- NULL
  if(length(feeder_id) > 0) feeder_id <- unique(d[, c("feeder_id", feeder_id)]) else feeder_id <- NULL
  return(list(both = both, bird_id = bird_id, feeder_id = feeder_id))
}

merge.extra <- function(d, extra, only = NULL) {
  if(!is.null(extra$both)) d <- merge(d, extra$both, by = c("bird_id", "feeder_id"), all.x = TRUE, all.y = FALSE)
  if(!is.null(extra$bird_id)) d <- merge(d, extra$bird_id, by = "bird_id", all.x = TRUE, all.y = FALSE)
  if(!is.null(extra$feeder_id)) d <- merge(d, extra$feeder_id, by = "feeder_id", all.x = TRUE, all.y = FALSE)
  return(d)
}

# ----------------------------------
# visits()
# ----------------------------------
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
#'   \code{allow.imp = TRUE}) . A visit is deemed impossible if a single bird
#'   travels between feeders in less time than specified by \code{bw}.
#'
#' @param r Dataframe. Contains raw reads from an RFID reader with colums
#'   \code{bird_id}, \code{feeder_id}, \code{time}.
#' @param bw Numerical. The minimum interval, in seconds, between reads for two
#'   successive reads to be considered separate visits.
#' @param allow.imp Logical. Whether impossible visits should be allowed (see
#'   details).
#' @param na.rm Logical. Whether NA values should be automatically omited.
#'   Otherwise an error is returned.
#' @param pass Logical. Pass 'extra' columns through the function and append
#'   them to the output.
#'
#' @return A data frame with visits specifying \code{bird_id} and
#'   \code{feeder_id} as well as the \code{start} and \code{end} of the visit.
#'   Any extra columns that are unique at the level of bird_id or feeder_id will
#'   also be passed through (i.e. age, sex, feeder location, etc.).
#'
#' @examples
#' \dontrun{
#' r <- get.data(start = "2016-03-06", end = "2016-03-08")
#' v <- visits(r)
#' }

#' @export
visits <- function(r, bw = 3, allow.imp = FALSE, na.rm = FALSE, pass = TRUE){

  # Confirm that expected columns and formats are present
  check.name(r, n = c("bird_id", "feeder_id", "time"))
  check.time(r, n = "time", internal = FALSE)
  check.format(r)

  # Check for NAs, remove if specified by na.rm = TRUE
  if(any(is.na(r))) {
    if(na.rm == FALSE) stop("NAs found. To automatically remove NAs, specify 'na.rm = TRUE'.")
    if(na.rm == TRUE) r <- r[rowSums(is.na(r)) == 0,]
  }

  # Grab unique extra cols
  if(pass == TRUE) extra <- keep.extra(r, n = "time")

  # Grab the timezone
  tz <- attr(r$time, "tzone")

  # Get spacing between visits, whether same bird or not, and whether same feeder or not
  r <- r[order(r$time),]
  diff.time <- (r$time[-1] - r$time[-nrow(r)]) > bw
  diff.bird <- r$bird_id[-nrow(r)] != r$bird_id[-1]
  diff.feeder <- r$feeder_id[-nrow(r)] != r$feeder_id[-1]

  # Check for impossible combos: where less than bw, still the same bird, but a different feeder
  impos <- which(rowSums(matrix(c(!diff.time, !diff.bird, diff.feeder), ncol = 3)) > 2)
  impos <- r[unique(c(impos,impos+1)),]
  if(nrow(impos) > 0 & allow.imp == FALSE) {
      print(paste0("Potentially impossible reads found: Time between reads is less than 'bw' (", bw, "s) for a single individual, yet the reads occur at different feeders. You should fix, remove or allow (allow.imp = TRUE) these rows and try again."))
      print(impos[order(impos$bird_id, impos$time),])
      stop("Impossible visits found, no specification for how to handle.")
  }

  # Start if
  # - time before is greater than 'bw' OR
  # - bird before is not the same OR
  # - feeder before is not the same
  # End if
  # - time after is great than 'bw' OR
  # - bird after is not the same OR
  # - feeder after is not the same

  new_visit <- apply(cbind(diff.time, diff.bird, diff.feeder), 1, any)
  r$end <- r$start <- as.POSIXct(NA)
  r$start[c(TRUE, new_visit)] <- r$time[c(TRUE, new_visit)]
  r$end[c(new_visit, TRUE)] <- r$time[c(new_visit, TRUE)]

  # Get visits
  v <- r[!(is.na(r$start) & is.na(r$end)),c("feeder_id","bird_id","start","end")]
  v <- reshape2::melt(v, measure.vars = c("start","end"))
  v <- v[!is.na(v$value),]
  v <- plyr::ddply(v[order(v$value),], c("feeder_id", "bird_id", "variable"), transform, n = 1:length(value))
  v$value <- as.character(v$value)  ## To overcome a bug in dcast which has issues dealing with times
  v <- reshape2::dcast(v, ... ~ variable, value.var = "value")
  v$start <- as.POSIXct(v$start, tz = tz)
  v$end <- as.POSIXct(v$end, tz = tz)
  v <- v[, -grep("^n$",names(v))]

  # Get sample sizes
  v$bird_n <- length(unique(v$bird_id))
  v$feeder_n <- length(unique(v$feeder_id))

  # Add in extra
  if(pass == TRUE) v <- merge.extra(v, extra)

  # Order and format data frame
  v <- col.order(v, c("bird_id", "start", "end", "feeder_id", "bird_n", "feeder_n"))
  v <- v[order(v$bird_id, v$start), ]
  v$feeder_id <- factor(v$feeder_id)
  v$bird_id <- factor(v$bird_id)

  return(v)
}


# ----------------------------------
# move
# ----------------------------------

#' 'Visits' to 'movements' for a single bird_id
#'
#' For a single \code{bird_id}, turns visits to mulitple feeders into movements
#' between feeders
#'
#' @param v1 Dataframe. A visits data frame containing only \strong{one} unique
#'   bird id. From the output of \code{visits}. Must contain columns
#'   \code{bird_id}, \code{feeder_id}, \code{start}, and \code{end}.
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
#' \itemize{
#' \item ID of the bird (\code{bird_id})
#' \item Time of event (\code{time})
#' \item The ID of feeders involved (\code{feeder_id})
#' \item The movement path including direction (\code{move_dir})
#' \item The movement path independent of direction (\code{move_path})
#' \item The 'strength' of the connection (inverse of time taken to move
#' between; \code{strength})
#' \item Information on whether left/arrived (\code{direction})
#' \item Any extra columns \code{pass}ed through
#' }
#'
#' @examples
#'
#' v <- visits(finches)
#'
#' # One bird at a time:
#' m <- move(v[v$bird_id == "0620000514",])
#'
#' # Split a data frame by \code{bird_id} with \code{plyr}, then
#' # apply \code{move}.
#' library(plyr)
#' m <- ddply(v, c("bird_id"), move)
#'
#' # Summarize a movement dataframe (divide by 2 because 2 rows for each event)
#' library(plyr)
#' m.totals <- ddply(m, c("bird_id", "move_path"), summarise,
#'                   n_path = length(move_path) / 2)

#' @export
move <- function(v1, all = FALSE, pass = TRUE){

  # Check for correct formatting
  check.name(v1, c("bird_id", "feeder_id", "start", "end"))
  check.time(v1)
  check.indiv(v1)
  check.format(v1)

  # Get factor levels
  bird_id <- levels(v1$bird_id)
  feeder_id <- levels(v1$feeder_id)

  # Get movement options
  move_dir <- expand.grid(feeder_left = feeder_id, feeder_arrived = feeder_id)
  move_dir <- move_dir[move_dir$feeder_left != move_dir$feeder_arrived,]
  move_dir <- paste0(move_dir$feeder_left, "_", move_dir$feeder_arrived)
  move_path <- unique(sapply(move_dir, FUN = mp))

  # Get extra columns
  if(pass == TRUE) extra <- keep.extra(v1, n = c("start", "end"))

  if(length(unique(v1$feeder_id)) > 1) { # Only proceed if there are actual data!

    # If there are movements, calculate events
    v1 <- v1[, c("bird_id", "start", "end", "feeder_id")]
    v1 <- v1[order(v1$start),]
    diff <- v1$feeder_id[-1] != v1$feeder_id[-nrow(v1)]
    v1$arrived <- v1$left <- FALSE
    v1$left[c(diff, FALSE)] <- TRUE
    v1$arrived[c(FALSE, diff)] <- TRUE

    m <- reshape2::melt(v1[v1$left | v1$arrived,], measure.vars = c("left", "arrived"), variable.name = "direction")
    m <- m[m$value, ]
    m <- reshape2::melt(m, measure.vars = c("start", "end"), variable = "type", value.name = "time")
    m <- m[(m$direction == "left" & m$type == "end") | (m$direction == "arrived" & m$type == "start"), !(names(m) %in% c("value", "type"))]
    m <- m[order(m$direction, m$time),]

    m$move_dir <- rep(factor(paste0(m$feeder_id[m$direction == "left"], "_", m$feeder_id[m$direction == "arrived"]), levels = move_dir), 2)
    m$strength <- 1 / as.numeric(difftime(m$time[m$direction == "arrived"], m$time[m$direction == "left"], units = "hours"))
    m$move_path <- sapply(m$move_dir, FUN = mp)
    m$move_path <- factor(m$move_path, levels = move_path)


    # Add in extra cols
    if(pass == TRUE) m <- merge.extra(m, extra)

    # Order
    m <- m[order(m$bird_id, m$time),]
    m <- col.order(m, c("bird_id", "time", "feeder_id", "move_dir", "move_path", "strength"))

  } else if (all == TRUE) {
    # Create the movement data frame for birds that didn't move between feeders
    m <- data.frame(bird_id = factor(v1$bird_id[1], levels = bird_id),
                    time = as.POSIXct(NA),
                    feeder_id = factor(NA, levels = feeder_id),
                    move_dir = factor(NA, levels = move_dir),
                    move_path = factor(NA, levels = move_path),
                    strength = as.numeric(NA))

    # Add in extra cols
    if(pass == TRUE) m <- merge.extra(m, extra)

    # Order
    m <- col.order(m, c("bird_id", "time", "feeder_id", "move_dir", "move_path", "strength"))

  } else {
    # If there are no movements and all == FALSE, return an empty data.frame
    m <- data.frame()
  }
  return(m)
}

# ----------------------------------
# feeding
# ----------------------------------

#' 'Visits' to 'feeding bouts' for a single bird_id
#'
#' For a single \code{bird_id}, turns visits at mulitple feeders into feeding
#' bouts. Feeding bouts are separated by switching feeders (when \code{bw =
#' NULL}). If \code{bw} is not \code{NULL}, they are separated by switching
#' feeders or by \code{bw} minutes.
#'
#'
#' @param v1 Dataframe. A visits data frame containing only \strong{one} unique
#'   bird id. From the output of \code{visits}. Must contain columns
#'   \code{bird_id}, \code{feeder_id}, \code{start}, and \code{end}.
#' @param bw Numeric. The minimum number of minutes between visits for two
#'   successive visits to be considered separate feeding bouts. When \code{bw} =
#'   NULL only visits to another feeder are scored as a separate feeding bout.
#' @param pass Logical. Pass 'extra' columns through the function and append
#'   them to the output.
#'
#' @return A data frame of feeding bouts. A feeding bout is defined as the bout
#'   of time spent making regular visits to a feeder, in which each visit
#'   occurrred within some cutoff time of the last. This data frame has the
#'   following columns: \itemize{ \item ID of the bird (\code{bird_id}) \item ID
#'   of the feeder(\code{feeder_id}) \item Time of the start of the feeding bout
#'   (\code{feed.start}) \item Time of the end of the feeding bout
#'   (\code{feed.end}) }
#'
#' @examples
#'
#'  v <- visits(finches)
#'
#'  # One bird at a time:
#'  f <- feeding(v[v$bird_id == "0620000514",])
#'
#'  # Split a data frame by \code{bird_id} with \code{plyr}, then
#'  # apply \code{move}.
#'  library(plyr)
#'  f <- ddply(v, c("bird_id"), feeding)
#'
#'  # Summarize a movement dataframe
#'  library(plyr)
#'  f.totals <- ddply(f, c("bird_id", "feeder_id"), summarise,
#'                    feed_length = sum(feed_length))
#'

#' @export
feeding <- function(v1, bw = 15, pass = TRUE){

  ## Check for correct formatting
  check.name(v1, c("bird_id","feeder_id", "start","end"))
  check.time(v1)
  check.indiv(v1)
  check.format(v1)

  # Get factor levels
  bird_id <- levels(v1$bird_id)
  feeder_id <- levels(v1$feeder_id)

  # Keep extra cols
  if(pass == TRUE) extra <- keep.extra(v1, n = c("start", "end"))

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
  f <- data.frame(bird_id = factor(v1$bird_id[1], levels = bird_id),
                  feeder_id = factor(v1$feeder_id[v1$feed_start == TRUE], levels = feeder_id),
                  feed_start = v1$start[v1$feed_start == TRUE],
                  feed_end = v1$end[v1$feed_end == TRUE],
                  feed_length = difftime(v1$end[v1$feed_end == TRUE], v1$start[v1$feed_start == TRUE], units = "mins"))

  # Get extra columns and add in
  if(pass == TRUE) f <- merge.extra(f, extra)
  f <- f[order(f$bird_id, f$feed_start), ]

  return(f)
}


# ----------------------------------
# disp
# ----------------------------------

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

#' @export
disp <- function(v, bw = 5, pass = TRUE){

  ## Check for correct formatting
  check.name(v, c("bird_id", "feeder_id", "start", "end"))
  check.time(v)
  check.format(v)

  bird_id <- levels(v$bird_id)
  feeder_id <- levels(v$feeder_id)

  v <- v[order(v$start), ]

  # Keep extra columns
  if(pass == TRUE) extra <- keep.extra(v, n = c("start", "end"))

  ## Define displacee and displacer by
  #  (a) whether subsequent visit was a different bird, AND
  #  (b) the arrival of the 2nd bird occurred within 'bw' seconds of the
  #      departure of the 1st
  #  (c) all of this occurs at the same feeder
  bird.diff <- v$bird_id[-1] != v$bird_id[-nrow(v)]
  time.diff <- (v$start[-1] - v$end[-nrow(v)]) < bw
  feeder.diff <- v$feeder_id[-1] == v$feeder_id[-nrow(v)]

  d <- rbind(v[c(bird.diff & time.diff & feeder.diff, FALSE), c("bird_id", "feeder_id", "start", "end")],
             v[c(FALSE, bird.diff & time.diff & feeder.diff), c("bird_id", "feeder_id", "start", "end")])
  d <- d[order(d$start), ]
  d$role <- c("displacee", "displacer")
  d <- d[order(d$role, d$start), ]

  d$left <- rep(v$end[c(bird.diff & time.diff & feeder.diff, FALSE)], 2)
  d$arrived <- rep(v$start[c(FALSE, bird.diff & time.diff & feeder.diff)], 2)

  d <- d[, !(names(d) %in% c("start", "end"))]
  if(nrow(d) == 0) stop(paste0("There are no displacement events with a bw = ", bw, ", stopping now"))

  if(pass == TRUE) d <- merge.extra(d, extra)
  d <- col.order(d, c("bird_id", "left", "arrived", "feeder_id", "role"))
  d <- d[order(d$left), ]

  ## Summarize totals
  s <- plyr::ddply(d, c("role", "bird_id"), plyr::summarise,
                   n = length(bird_id), .drop = FALSE)
  s <- reshape2::dcast(s, ... ~ role, value.var = "n")
  s$p_win <- s$displacer / (s$displacee + s$displacer)
  s <- s[order(s$p_win, decreasing = TRUE),]

  ## Summarize interactions
  t <- d
  t$interaction <- paste(t$bird_id[t$role == "displacer"], t$bird_id[t$role == "displacee"], sep = "_")
  t <- plyr::ddply(t, "interaction", plyr::summarise,
                   n = length(interaction), .drop = FALSE)
  t[, c("displacer", "displacee")] <- stringr::str_extract_all(t$interaction, paste0(bird_id, collapse = "|"), simplify = TRUE)
  t <- t[order(match(t$displacer,s$bird_id)),]  ##Sort according to the p_win value from s

  return(list("displacements" = d, "summaries" = s, "interactions" = t[, names(t) != "interaction",]))
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
  check.name(d, c("displacer","displacee","n"))
  check.format(d)

  # Start with best order
  o <- merge(plyr::ddply(d, c("displacer"), plyr::summarise, win = sum(n)),
             plyr::ddply(d, c("displacee"), plyr::summarise, loss = sum(n)),
             by.x = "displacer", by.y = "displacee")
  o$p.win <- o$win / (o$win + o$loss)
  o <- o[order(o$p.win,decreasing = TRUE), ]

  # Check sample sizes and warn if low

  if(omit_zero == TRUE) {
    omit <- o$displacer[(o$win + o$loss) == 0]
    o <- o[!(o$displacer %in% omit), ]
    d <- d[!(d$displacee %in% omit) & !(d$displacer %in% omit), ]
    message("bird_ids with zero interactions have been omitted: ", paste0(omit, collapse = ", "))
  }

  if(nrow(o[(o$win + o$loss) <= 2, ]) / nrow(o) > 0.5) message("More than 50% of your interactions (", round(nrow(d[d$n == 0, ]) / nrow(d)*100), "%) have 2 or fewer observations, this matrix may be founded on too little data.")

  o <- list(as.character(unique(o$displacer)))

  ## Setup the matrix
  d <- reshape2::dcast(d, displacer ~ displacee, value.var = "n")
  row.names(d) <- d$displacer
  d <- as.matrix(d[,-grep("^displacer$", names(d))])

  ## Setup Loop
  try <- 0
  o.l <- o
  rev <- list()
  done <- FALSE  ## Are we done this iteration?
  prev <- vector()

  while(done == FALSE & try < tries){

    for(i in 1:length(o.l)){
      new.o <- o.l[[i]]
      temp <- d

      ## Sort matrix by dominance hierarchy (new.o)
      temp <- temp[order(match(rownames(temp),new.o)),order(match(colnames(temp),new.o))]

      ## CHECK (TODO set to stop script if this doesn't work)
      all(diag(temp)==0)
      all(dimnames(temp)[[1]] == dimnames(temp)[[2]])

      ## Get upper and lower to compare
      upper <- temp
      upper[lower.tri(upper, diag = TRUE)] <- NA
      lower <- t(temp)
      lower[lower.tri(lower, diag = TRUE)] <- NA

      ## Get reversals
      rev[[length(rev)+1]] <- which(upper < lower, arr.ind = TRUE)
    }

    ## Keep only the orders with the fewest reversals
    if(length(rev) > 0){
      n <- sapply(rev, nrow)
      rev <- rev[n == min(n)]
      o.l <- o.l[n == min(n)]
    }

    # Compare with previous matrix, if the same, we're done
    if(identical(prev, o.l)) done <- TRUE else prev <- o.l

    if(length(rev) > 0 & length(rev[[1]]) > 0 & done == FALSE){
      ## Add the new reversal switches to our list of options and try again
      for(j in 1:length(rev)){
        for(i in 1:nrow(rev[[j]])){
          a <- rev[[j]][i,2]  ## Which individuals to move up
          b <- rev[[j]][i,1]  ## Where to move it
          o.l[[length(o.l)+1]] <- c(new.o[1:(b-1)], new.o[a], new.o[-c(1:(b-1), a)])
        }
      }
    } else done <- TRUE

    ## Loop controls
    if(done == FALSE){
      try <- try + 1
      o.l <- unique(o.l)
      rev <- list()
    }
  }
  #print(paste0("Started with: ",paste0(unlist(o), collapse = ", ")))
  message(paste0("Tried ",try," times. Found ",length(o.l), " 'best' matrices, with ",nrow(rev[[1]])," reversal(s) per matrix"))

  m <- list()
  r <- list()
  for(i in 1:length(o.l)) {
    m[[length(m)+1]] <- d[order(match(rownames(d),o.l[[i]])),order(match(colnames(d),o.l[[i]]))]
    if(length(rev[[i]]) > 0) r[[length(r)+1]] <- c(rownames(m[[i]])[rev[[i]][1]], colnames(m[[i]])[rev[[i]][2]])
  }

  return(list(dominance = o.l, reversals = r, matrices = m))
}


