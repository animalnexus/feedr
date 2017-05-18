#' Get timezone offset from UTC
#'
#' @param tz Character. Timezone to calculate offset from
#' @param dst Character. Whether or not to consider day-light-savings
#' @param tz_name Logical. Whether or not to return name of the timezone or
#'   just the offset in hours.
#'
#' @export
tz_offset <- function(tz, dst = FALSE, tz_name = FALSE) {
  tz <- check_tz(tz)
  if(!dst) t <- as.numeric(difftime(as.POSIXct("2016-01-01 00:00:00", tz = "UTC"),
                                    as.POSIXct("2016-01-01 00:00:00", tz = tz), units = "hours"))
  if(dst) t <- as.numeric(difftime(as.POSIXct("2016-06-01 00:00:00", tz = "UTC"),
                                   as.POSIXct("2016-06-01 00:00:00", tz = tz), units = "hours"))
  if(tz_name) {
    if(t > 0) t <- paste0("Etc/GMT-", t)
    if(t <= 0) t <- paste0("Etc/GMT+", abs(t))
  }
  return(t)
}

last <- function(x)  return(x[length(x)])

mp <- function(x) paste0(sort(unlist(strsplit(as.character(x), "_"))), collapse = "_")

# Grab extra columns unique only
keep_extra <- function(d, n, only = c("animal_id", "logger_id", "date")){

  # Ungroup, if exists
  d <- dplyr::ungroup(d)

  d <- unique(d[, setdiff(names(d), n)])

  # If loc present, deconstruct
  if(any(names(d) == "loc")){
    d$lon <- as.numeric(gsub("\\(([-0-9.]+),[-0-9.]+\\)", "\\1", d$loc))
    d$lat <- as.numeric(gsub("\\([-0-9.]+,([-0-9.]+)\\)", "\\1", d$loc))
    d <- d[, names(d) != "loc",]
  }

  extra <- names(d)[!(names(d) %in% c("animal_id", "logger_id", "date"))]
  animal_id <- logger_id <- date <- all <- NULL

  if("animal_id" %in% only) animal_id <- extra[lapply(extra, FUN = function(x) nrow(unique(cbind(d$animal_id, d[, x])))) == length(unique(d$animal_id))]
  if("logger_id" %in% only) logger_id <- extra[lapply(extra, FUN = function(x) nrow(unique(cbind(d$logger_id, d[, x])))) == length(unique(d$logger_id))]
  if("date" %in% only) date <- extra[lapply(extra, FUN = function(x) nrow(unique(cbind(d$date, d[, x])))) == length(unique(d$date))]

  #if(all(c("logger_id", "animal_id", "date") %in% only)) {
    all <- intersect(intersect(animal_id, logger_id), date)
    bf <- setdiff(intersect(animal_id, logger_id), all)
    bd <- setdiff(intersect(animal_id, date), all)
    fd <- setdiff(intersect(logger_id, date), all)
    logger_id <- setdiff(logger_id, all)
    animal_id <- setdiff(setdiff(animal_id, all), bf)
    date <- setdiff(setdiff(setdiff(date, all), bd), fd)
  #}

  if(length(all) > 0) all <- unique(d[, c("animal_id", "logger_id", "date", all)]) else all <- NULL
  if(length(animal_id) > 0) animal_id <- unique(d[, c("animal_id", animal_id)]) else animal_id <- NULL
  if(length(logger_id) > 0) logger_id <- unique(d[, c("logger_id", logger_id)]) else logger_id <- NULL
  if(length(date) > 0) date <- unique(d[, c("date", date)]) else date <- NULL
  return(list(all = all, animal_id = animal_id, logger_id = logger_id, date = date))
}

merge_extra <- function(d, extra, only = NULL) {
  if(!is.null(extra$all)) d <- dplyr::left_join(d, extra$all, by = c("animal_id", "logger_id", "date"))
  if(!is.null(extra$animal_id)) d <- dplyr::left_join(d, extra$animal_id, by = "animal_id")
  if(!is.null(extra$logger_id)) d <- dplyr::left_join(d, extra$logger_id, by = "logger_id")
  if(!is.null(extra$date)) d <- dplyr::left_join(d, extra$date, by = "date")
  return(d)
}

# Average clock time
mean_clock <- function(time, origin = FALSE) {
  tz <- lubridate::tz(time[1])
  mean_time <- format(mean(as.POSIXct(paste("1970-01-01", format(time, "%H:%M:%S")))), "%H:%M:%S")
  mean_date <- ifelse(origin, "1970-01-01", as.character(as.Date(mean(time))))
  return(as.POSIXct(paste(mean_date, mean_time), tz = tz))
}

