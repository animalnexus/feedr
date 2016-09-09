dep <- function(x){
  if (exists(x)) {
    warning("Argument ", x, " is deprecated; please use ", gsub("\\.", "_", x), " instead.",
            call. = FALSE)
  }
}


#' Get timezone offset from UTC
#'
#' @export
tz_offset <- function(tz, dst = FALSE, tz_name = FALSE) {
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

mp <- function(x) paste0(sort(unlist(strsplit(as.character(x), "_"))), collapse = "_")

# Grab extra columns unique only
keep_extra <- function(d, n, only = c("bird_id", "feeder_id")){

  d <- unique(d[, setdiff(names(d), n)])

  # If loc present, deconstruct
  if(any(names(d) == "loc")){
    d$lon <- as.numeric(gsub("\\(([-0-9.]+),[-0-9.]+\\)", "\\1", d$loc))
    d$lat <- as.numeric(gsub("\\([-0-9.]+,([-0-9.]+)\\)", "\\1", d$loc))
    d <- d[, names(d) != "loc",]
  }

  extra <- names(d)[!(names(d) %in% c("bird_id", "feeder_id"))]
  bird_id <- feeder_id <- both <- NULL

  if("bird_id" %in% only) bird_id <- extra[lapply(extra, FUN = function(x) nrow(unique(cbind(d$bird_id, d[, x])))) == length(unique(d$bird_id))]
  if("feeder_id" %in% only) feeder_id <- extra[lapply(extra, FUN = function(x) nrow(unique(cbind(d$feeder_id, d[, x])))) == length(unique(d$feeder_id))]

  if(all(c("feeder_id", "bird_id") %in% only)) {
    both <- intersect(bird_id, feeder_id)
    feeder_id <- setdiff(setdiff(feeder_id, both), bird_id)
    bird_id <- setdiff(setdiff(bird_id, both), feeder_id)
  }

  if(length(both) > 0) both <- unique(d[, c("bird_id", "feeder_id", both)]) else both <- NULL
  if(length(bird_id) > 0) bird_id <- unique(d[, c("bird_id", bird_id)]) else bird_id <- NULL
  if(length(feeder_id) > 0) feeder_id <- unique(d[, c("feeder_id", feeder_id)]) else feeder_id <- NULL
  return(list(both = both, bird_id = bird_id, feeder_id = feeder_id))
}

merge_extra <- function(d, extra, only = NULL) {
  if(!is.null(extra$both)) d <- dplyr::left_join(d, extra$both, by = c("bird_id", "feeder_id"))
  if(!is.null(extra$bird_id)) d <- dplyr::left_join(d, extra$bird_id, by = "bird_id")
  if(!is.null(extra$feeder_id)) d <- dplyr::left_join(d, extra$feeder_id, by = "feeder_id")
  return(d)
}
