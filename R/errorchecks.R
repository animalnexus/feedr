
check.name <- function(d, n = c("bird_id", "feeder_id")) {
  if(!all(n %in% names(d))) stop(paste0("Required columns aren't present. Require: ", paste0("'", n, "'", collapse = ", ")))
}

check.time <- function(d, n = c("start", "end"), internal = TRUE) {
  if(!all(sapply(d[, n], class) == c("POSIXct", "POSIXt"))) {
    stop(paste0("Columns ", paste0("'", n, "'", collapse = ", "), " must be in R's date/time formating (POSIXct).", ifelse(internal == TRUE, " This data frame should have been created with a feedr function. Have you changed the values since they were created?", " Consider as.POSIXct() and strptime() or lubridate::parse_date_time().")))
  }
}

check.indiv <- function(d) {
  if(length(unique(d$bird_id)) > 1) stop("This function is only designed to be run on one individual at a time. Consider using the ddply() function from the plyr package, or the do() function from the dplyr package to apply this function to all birds.")
}

check.format <- function(d, map = FALSE, disp = FALSE) {
  msg_f <- "Using '_' in feeder_id values conflicts with the mapping functions."
  msg_b <- "Using '_' in bird_id values conflicts with the displacement/dominance functions."

  if(!map) msg_f <- paste0(msg_f, " You should remove any '_'s if you plan to use these functions.")
  if(!disp) msg_b <- paste0(msg_b, " You should remove any '_'s if you plan to use these functions.")

  if(any(stringr::str_count(d$feeder_id, "_") > 0)) message(msg_f)
  if(any(stringr::str_count(d$bird_id, "_") > 0)) message(msg_b)
}



