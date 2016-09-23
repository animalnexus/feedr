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
#' @param f Dataframe. A feeding bout data frame (may contain multiple bird_ids). From
#'   the output of \code{feeding}. Must contain columns \code{bird_id},
#'   \code{feeder_id}, \code{feed_start}, and \code{feed_end}.
#' @param res Character. The resolution over which to calculate activity. Should
#'   be in the format of "15 min" or "1 hour", etc.
#' @param by_feeder Logical. Should the activity be calculated overall, or
#'   individually for each feeder visited? If there is only one feeder,
#'   by_feeder will automatically revert to TRUE to enable passing of
#'   feeder-related variables.
#' @param missing Data frame. (NOT AVAILABLE) If there are known times for a
#'   particular feeder for which activity can't be recorded (i.e. times during
#'   which a feeder was inactive).
#' @param sun Logical. Calculate sun rise/set? If by_feeder = FALSE, returns
#'   median sun rise/set across all feeders for each day.
#' @param keep_all Logical. Keep all individuals, even ones with less than 24hrs of data.
#' @param pass Logical. Pass 'extra' columns through the function and append them to the output.
#'
#' @import magrittr
#' @export

activity <- function(f, res = 15, by_feeder = FALSE, missing = NULL, sun = TRUE, keep_all = FALSE, pass = TRUE){

  check_name(f, c("bird_id", "feeder_id", "feed_start", "feed_end"), "feeding")
  check_time(f, c("feed_start", "feed_end"))

  if(!is.null(missing)){
    message("missing argument not yet implemented")
    # if(!is.data.frame(missing)) {
    #   if(!is.character(missing) | length(missing) != 1) {
    #     stop("'missing' must be data frame or string with location of a csv file.")
    #   } else {
    #     missing <- read.csv(missing)
    #   }
    # }

    #if(sum(names(missing) %in% c("start", "end")) != 2) stop("'missing' must have columns 'start' and 'end'.")

    #missing$start <- lubridate::parse_date_time(missing$start, orders = "%Y-%m-%d %H:%M:%S", truncated = 3, tz = tz)
    #missing$end <- lubridate::parse_date_time(missing$end, orders = "%Y-%m-%d %H:%M:%S", truncated = 3, tz = tz)

    # if(any(!lubridate::is.POSIXct(c(missing$start, missing$end)))) {
    #   stop("'missing' start or end cannot be converted to date/time, be sure it is in a standard date/time format (YYYY-MM-DD HH:MM:SS is best).")
    # }
  }

  # Keep extra cols
  if(pass) {
    if(by_feeder == FALSE) only <- c("bird_id", "date") else only <- c("feeder_id", "bird_id", "date")
    extra <- keep_extra(f, n = c("feed_start", "feed_end", "feed_length"), only = only)
  }

  if(any(!lubridate::is.POSIXct(c(f$feed_start, f$feed_end)))) {
    stop("Cannot define start and end times of the feeding data set, make sure this is the output from feeding().")
  }


  # Apply individually to each bird
  a <- f %>%
    dplyr::group_by(bird_id) %>%
    dplyr::do(activity_single(., res = res, by_feeder = by_feeder, missing = missing, sun = sun, keep_all = keep_all)) %>%
    dplyr::ungroup()

  if(pass) a <- merge_extra(a, extra)

  a <- dplyr::arrange(a, bird_id, date, time, feeder_id)
  return(a)

}


activity_single <- function(f1, res = 15, by_feeder = FALSE, missing = NULL, sun = TRUE, keep_all = FALSE){

  check_indiv(f1)

  if(nrow(f1) == 0) {
    message(paste0(f1$bird_id[1], ": Skipping. Individual has no data"))
    return(data.frame())
  } else {

  start <- lubridate::floor_date(min(f1$feed_start), "day")
  end <- lubridate::ceiling_date(max(f1$feed_end), "day")

  # Calculate Activity only if > 24hrs of data
  if((max(f1$feed_end) - min(f1$feed_start)) < lubridate::dhours(24) & keep_all == FALSE) {
    message(paste0(f1$bird_id[1], ": Skipping. Individual has less than 24hrs of data"))
    return(data.frame())
  } else if (all(f1$feed_length == 0))  {
    message(paste0(f1$bird_id[1], ": Skipping. All feeding bouts are 0 min. Consider increasing 'bw' in feeding()"))
    return(data.frame())
  } else {
    ## ACCOUNT FOR MISSING!!!

    # Check proportion of time active, warn if really low
    p_active <- as.numeric(sum(f1$feed_length)) / as.numeric(difftime(max(f1$feed_end), min(f1$feed_start), units = "mins"))
    if(p_active < 0.05) message(paste0(f1$bird_id[1], ": Active less than 5% of the total time period..."))

    # Override by_feeder if only one feeder_id to keep extra columns
    #if(length(unique(f1$feeder_id)) == 1) by_feeder <- TRUE

    # Get activity
    prob <- round(length(f1$feed_length[f1$feed_length < res]) / nrow(f1) * 100, 2)
    if(prob > 50) {
      message(paste0(f1$bird_id[1], ": ", prob, "% of obs are shorter than 'res' (", res, " min). Median obs is ", round(median(f1$feed_length), 2), " min."))
    }

    # Prep activity data frame
    res <- res * 60
    a <- data.frame(
      bird_id = factor(f1$bird_id[1], levels = levels(f1$bird_id)),
      time = seq(start, end, by = paste0(res, " sec")),
      activity_c = factor("inactive",
                          levels = c("active", "inactive", "unknown")))

    a$date <- as.Date(lubridate::floor_date(a$time, unit = "day"))

    # Get by individual only, or by individual for each feeder
    if(by_feeder == FALSE){
      a$feeder_id = factor(NA, levels = levels(f1$feeder_id))
    } else {
      temp <- data.frame()
      for(i in unique(f1$feeder_id)){
        temp <- rbind(temp, cbind(a, feeder_id = factor(i, levels = levels(f1$feeder_id))))
      }
      a <- temp
    }

    # Fill with active/inactive
    for(f_id in levels(f1$feeder_id)){
      f <- f1[f1$feeder_id == f_id, ]
      for(i in 1:nrow(f)) {
        if(by_feeder == FALSE) {
          a$activity_c[a$time >= f$feed_start[i] & a$time <= f$feed_end[i]] <- "active"
        } else {
          a$activity_c[a$feeder_id == f_id & a$time >= f$feed_start[i] & a$time <= f$feed_end[i]] <- "active"
        }
      }
    }

    # if(!is.null(missing)) {
    #   for(i in 1:nrow(missing)){
    #     if(by_feeder == FALSE){
    #       a$activity_c[a$time >= missing$start[i] & a$time <= missing$end[i]] <- "unknown"
    #     } else {
    #       a$activity_c[a$feeder_id == missing$feeder_id[i] & a$time >= missing$start[i] & a$time <= missing$end[i]] <- "unknown"
    #     }
    #   }
    # }

    # Create plotting column
    a$activity <- as.numeric(a$activity == "active")

    # Calculate sunrise/sunset times
    if(sun == TRUE) {
      if(!all(c("lat", "lon") %in% names(f1))) {
        message(paste0(f1$bird_id, ": Skipping sunrise/sunset, no lat/lon information"))
      } else {

        s <- expand.grid(feeder_id = unique(f1$feeder_id), date = as.Date(seq(start, end, by = "1 day"))) %>%
          dplyr::left_join(unique(f1[, c("feeder_id", "lon", "lat")]), by = "feeder_id")
        s <- dplyr::bind_cols(s, sun(s[, c("lon", "lat")], s$date))

        if(by_feeder == TRUE) {
          a <- dplyr::left_join(a, s[, c("feeder_id", "date", "rise", "set")],
                                by = c("feeder_id", "date"))
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
    n <- c("bird_id", "date", "time", "activity", "activity_c", "feeder_id", "rise", "set")
    n <- n[n %in% names(a)]
    a <- dplyr::select_(a, .dots = n)

    return(a)
  }
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
#' @param a Data frame. Data from output of \code{activity()}.
#' @param pass Logical. Pass 'extra' columns through the function and append them to the output.
#'
#' @export

daily <- function(a, pass = TRUE){

  check_name(a, c("bird_id", "date", "time", "activity", "activity_c", "feeder_id"), "activity")
  check_time(a, c("time"))

    # Get extra
  if(pass){
    if(all(is.na(a$feeder_id))) only <- "bird_id" else only <- c("feeder_id", "bird_id")
    extra <- keep_extra(a, c("time", "activity", "activity_c"), only = only)
  }

  a$time_c <- format(a$time, "%H:%M:%S")

  # Apply single function

  d <- a %>%
    dplyr::group_by(bird_id) %>%
    dplyr::do(daily_single(., pass = pass)) %>%
    dplyr::ungroup()

  if(pass) d <- merge_extra(d, extra)


  return(d)
}

daily_single <- function(a1, pass = TRUE){

  check_indiv(a1)

  d <- a1 %>%
    dplyr::group_by(bird_id, feeder_id, time_c) %>%
    dplyr::summarize(p_active = length(activity_c[activity_c == "active"]) / length(activity_c[activity_c != "unknown"]),
                     p_inactive = length(activity_c[activity_c == "inactive"]) / length(activity_c[activity_c != "unknown"]),
                     p_unknown = length(activity_c[activity_c == "unknown"]) / length(activity_c),
                     p_total = 1 - p_unknown)


  d$time <- as.POSIXct(paste0(lubridate::origin, " ", d$time_c))
  lubridate::tz(d$time) <- "UTM"

  # Get sun/rise set if exist, and average
  if(all(c("rise", "set") %in% names(a1))) {
    sun <- unique(a1[, c("date", "feeder_id", "rise", "set")])

    sun <- sun %>%
      dplyr::group_by(feeder_id) %>%
      dplyr::summarize(rise = mean_clock(rise, origin = TRUE),
                       set = mean_clock(set, origin = TRUE))
    d <- merge(d, sun, by = "feeder_id", all.x = TRUE, all.y = FALSE)
  }

  # Order
  n <- c("bird_id", "time", "time_c", "p_active", "p_inactive", "p_unknown", "p_total", "feeder_id", "rise", "set")
  n <- n[n %in% names(d)]

  d <- dplyr::select_(d, .dots = n) %>%
    dplyr::arrange(bird_id, time)

  return(d)
}

#' Get sunrise/sunset times
#' @export
sun <- function(loc, date) {
  if(class(loc) == "numeric") loc <- matrix(loc, nrow = 1)
  if(class(loc) %in% c("data.frame", "matrix")) loc <- as.matrix(loc)
  date <- as.POSIXct(as.character(date))
  s <- data.frame(rise = maptools::sunriset(loc, date, direction = "sunrise", POSIXct.out = TRUE)$time,
                  set = maptools::sunriset(loc, date, direction = "sunset", POSIXct.out = TRUE)$time)

  return(s)
}

# Average clock time
mean_clock <- function(time, origin = FALSE) {
  tz <- lubridate::tz(time[1])
  mean_time <- format(mean(as.POSIXct(paste("1970-01-01", format(time, "%H:%M:%S")))), "%H:%M:%S")
  mean_date <- ifelse(origin, "1970-01-01", as.character(as.Date(mean(time))))
  return(as.POSIXct(paste(mean_date, mean_time), tz = tz))
}
