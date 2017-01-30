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
#' @param res Character. The resolution over which to calculate activity. Should
#'   be in the format of "15 min" or "1 hour", etc.
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
#' @export
sun <- function(loc, date, tz) {
  if(class(loc) == "numeric") loc <- matrix(loc, nrow = 1)
  if(class(loc) %in% c("data.frame", "matrix")) loc <- as.matrix(loc)
  date <- as.POSIXct(as.character(date), tz = tz)
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
