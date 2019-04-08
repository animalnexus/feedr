
check_name <- function(d, n = c("animal_id", "logger_id"), type = "visit") {
  if(!is.null(type)) m <- paste0("You should be using '", type, "' data. ") else m <- ""
  if(!all(n %in% names(d))) stop(paste0(m, "Required columns aren't present. Require: ", paste0("'", n, "'", collapse = ", ")), call. = FALSE)
}

check_time <- function(d, n = c("start", "end"), internal = TRUE) {
  if(!all(sapply(d[, n], class) == c("POSIXct", "POSIXt"))) {
    stop(paste0("Columns ", paste0("'", n, "'", collapse = ", "), " must be in R's date/time formating (POSIXct).", ifelse(internal == FALSE, " Consider using as.POSIXct() and strptime() or lubridate::parse_date_time().", "")), call. = FALSE)
  }
}

# Check timezone
check_tz <- function(tz) {
  if(length(tz) > 1) {
    message("Cannot supply more than one timezone, using the first")
    tz <- tz[1]
  }
  if(is.null(tz) || is.na(tz) || tz == "") {
    message("Cannot set timezone, defaulting to UTC")
    tz <- "UTC"
  } else if (!(tz %in% OlsonNames())) {
    if(tolower(tz) %in% tolower(OlsonNames())) {
      t <- OlsonNames()[tolower(OlsonNames()) %in% tolower(tz)]
      message("Timezone ", tz, " not in OlsonNames(), assuming ", t)
      tz <- t
    } else {
      message("Timezone: ", tz, " not in OlsonNames(), defaulting to UTC")
      tz <- "UTC"
    }
  }
  return(tz)
}

check_indiv <- function(d) {
  if(length(unique(d$animal_id)) > 1) stop("This function is only designed to be run on one individual at a time. Consider using the ddply() function from the plyr package, or the do() function from the dplyr package to apply this function to all animals.", call. = FALSE)
}

check_format <- function(d, map = FALSE, disp = FALSE) {
  msg_l <- "Using '_' in logger_id values conflicts with the mapping functions."
  msg_a <- "Using '_' in animal_id values conflicts with the displacement function (disp())."

  if(!map) msg_l <- paste0(msg_l, " You should remove any '_'s if you plan to use these functions.")
  if(!disp) msg_a <- paste0(msg_a, " You should remove any '_'s if you plan to use these functions.")

  if("logger_id" %in% names(d)) if(any(stringr::str_count(d$logger_id, "_") > 0)) message(msg_l)
  if("animal_id" %in% names(d)) if(any(stringr::str_count(d$animal_id, "_") > 0)) message(msg_a)
}

check_input <- function(d, input = "lon", options = c("lon", "longitude", "long"), verbose = TRUE) {
  opts_string <- paste0("(^", paste0(options, collapse = "$)|(^"), "$)")
  n <- which(stringr::str_detect(names(d), stringr::regex(opts_string, ignore_case = TRUE)))

  # Check if any columns
  if(length(n) > 0){
    # Check if more than two columns for the input
    if(length(n) > 1) {
      c <- utils::combn(n, 2)
      if(ncol(c) < 10) {
        for(i in 1:ncol(c)) {
          if(!isTRUE(all.equal(d[, c[1, i]][[1]], d[, c[2, i]][[1]]))) {
            stop("There are multiple ", input, " columns which are not equivalent\n(expects ", input, " to be one of ", paste0(options, collapse = ", "), ", but ignores case")
          }
        }
      } else {
        stop("There are too many duplicate ", input, " columns\n(expects ", input, " to be one of ", paste0(options, collapse = ", "), ", but ignores case")
      }
      # Omit extra columns if duplicates
      if(verbose) message("Omitting duplicate columns for ", input)
      d <- d[, -n[2:length(n)]]
      n <- n[1]
    }
    if(any(names(d)[n] != input)) {
      if(verbose) message("Renaming column '", names(d)[n], "' to '", input, "'")
      names(d)[n] <- input
    }
  }
  return(d)
}


