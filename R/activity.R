#' Calculate activity from feeding data
#'
#' This function will take feeding bouts and calculate activity status (active
#' vs. inactive) at a resolution of \code{res}.
#'
#' A message will alert you to when the \code{res} is larger than the 50% of the
#' feeding bouts. This may result in missed feeding bouts, and it may be better
#' to choose a smaller \code{res}.
#'
#' The \code{missing} data frame should have columns \code{start} and \code{end}
#' corresponding to the start and end times of the missing data. Any activity between
#' those start/end times will be scored as unknown, regardless of the
#' \code{feeder_id}. However, if \code{by_feeder} is TRUE, \code{missing} may
#' also include the column \code{feeder_id}. In this case, only activity for the
#' feeder with the missing start/end times will be scored as unknown. If
#' \code{by_feeder} is TRUE but \code{missing} does not contain the column
#' \code{feeder_id}, all activity between the start and end times will be scored
#' as unknown, regardless of the feeder. See examples.
#'
#' @param f1 Data frame. A feeding bout data frame for an individual
#'   \code{bird_id}
#' @param res Character. The resolution over which to calculate activity. Should
#'   be in the format of "15 min" or "1 hour", etc.
#' @param missing Data frame. If there are known times for a particular feeder
#'   for which activity can't be recorded (i.e. times during which a feeder was
#'   inactive).
#'
#' @export

activity <- function(f1, res = 15, by_feeder = FALSE, missing = NULL){

  check.name(f1, c("bird_id", "feeder_id", "feed_start", "feed_end"))
  check.time(f1, c("feed_start", "feed_end"))
  check.indiv(f1)

  tz <- lubridate::tz(f1$feed_start)

  if(!is.null(missing)){
    if(!is.data.frame(missing)) {
      if(!is.character(missing) | length(missing) != 1) {
        stop("'missing' must be data frame or string with location of a csv file.")
      } else {
        missing <- read.csv(missing)
      }
    }

    if(sum(names(missing) %in% c("start", "end")) != 2) stop("'missing' must have columns 'start' and 'end'.")

    missing$start <- lubridate::parse_date_time(missing$start, orders = "%Y-%m-%d %H:%M:%S", truncated = 3, tz = tz)
    missing$end <- lubridate::parse_date_time(missing$end, orders = "%Y-%m-%d %H:%M:%S", truncated = 3, tz = tz)

    if(any(!lubridate::is.POSIXct(c(missing$start, missing$end)))) {
      stop("'missing' start or end cannot be converted to date/time, be sure it is in a standard date/time format (YYYY-MM-DD HH:MM:SS is best).")
    }
  }

  start <- lubridate::floor_date(min(f1$feed_start), "day")
  end <- lubridate::ceiling_date(max(f1$feed_end), "day")

  if(any(!lubridate::is.POSIXct(c(start, end)))) {
    stop("Cannot define start and end times of the feeding data set (f1), make sure this is the output from feeding().")
  }

  # Calculate Activity only if > 24hrs of data
  if((max(f1$feed_end) - min(f1$feed_start)) < lubridate::dhours(24)) {
    message(paste0(f1$bird_id[1], ": Individual has less than 24hrs of data, skipping..."))
  } else {
    prob <- round(length(f1$feed_length[f1$feed_length < res]) / nrow(f1) * 100, 2)
    if(prob > 50) {
      message(paste0(f1$bird_id[1], ": ", prob, "% of obs are shorter than 'res' (", res, " min). Median obs is ", round(median(f1$feed_length), 2), " min."))
    }

    # Prep activity data frame
    res <- res * 60
    a <- data.frame(time = seq(start, end, by = paste0(res, " sec")),
                  activity = factor("inactive",
                                    levels = c("active", "inactive", "unknown")))

    # Get by individual only, or by individual for each feeder
    if(by_feeder == FALSE){
      a$feeder_id = factor(NA, levels = levels(f1$feeder_id))
    } else {
      temp <- data.frame()
      for(i in levels(f1$feeder_id)){
        temp <- rbind(temp, cbind(a, feeder_id = factor(i, levels = levels(f1$feeder_id))))
      }
      a <- temp
    }

    # Fill with active/inactive
    for(f.id in levels(f1$feeder_id)){
      f <- f1[f1$feeder_id == f.id, ]
      for(i in 1:nrow(f)) {
        if(by_feeder == FALSE) {
          a$activity[a$time >= f$feed_start[i] & a$time <= f$feed_end[i]] <- "active"
        } else {
          a$activity[a$feeder_id == f.id & a$time >= f$feed_start[i] & a$time <= f$feed_end[i]] <- "active"
        }
      }
    }

    if(!is.null(missing)) {
      for(i in 1:nrow(missing)){
        if(by_feeder == FALSE){
          a$activity[a$time >= missing$start[i] & a$time <= missing$end[i]] <- "unknown"
        } else {
          a$activity[a$feeder_id == missing$feeder_id[i] & a$time >= missing$start[i] & a$time <= missing$end[i]] <- "unknown"
        }
      }
    }
    return(a)
  }
}

#' Create daily activity data
#'
#' Summarizes and averages activity data over a 24-hr period, generating a 24-hr
#' daily activity pattern for plotting. The resulting data set contains four
#' columns reflecting the proportions of time blocks scored as active, inactive,
#' unknonw, or total.
#'
#' Output timezone and dates are irrelevant, as the data is tied to times, not
#' dates. Therefore timezone is UTC and the dates are on 1970-01-01. When
#' plotting, omit the date part of the label to accurately portray time only.
#'
#' Resolution of the data is automatically detected as the same as that
#' specified in \code{activity()}.
#'
#'
#' @param a1 Data frame. Data from output of \code{activity()}.
#'
#' @export
daily <- function(a1){
  check.name(a1, c("bird_id", "time", "activity", "feeder_id"))
  check.time(a1, c("time"))
  check.indiv(a1)

  a1$time_c <- format(a1$time, "%H:%M:%S")

  d <- plyr::ddply(a1, c("bird_id", "feeder_id", "time_c"), plyr::summarise,
                   p_active = length(activity[activity == "active"]) / length(activity[activity != "unknown"]),
                   p_inactive = length(activity[activity == "inactive"]) / length(activity[activity != "unknown"]),
                   p_unknown = length(activity[activity == "unknown"]) / length(activity),
                   p_total = 1 - p_unknown)

  d$time <- as.POSIXct(paste0(lubridate::origin, " ", d$time_c))
  lubridate::tz(d$time) <- "UTM"

  d <- col.order(d, c("bird_id", "time", "time_c", "feeder_id"))

  return(d)
}
