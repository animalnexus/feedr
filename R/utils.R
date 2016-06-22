mp <- function(x) paste0(sort(unlist(strsplit(as.character(x), "_"))), collapse = "_")

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
