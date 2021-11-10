#' Load raw read data
#'
#' Loads raw read data and formats for use with the feedr functions. This is
#' merely a wrapper function that does many things that you can do yourself.
#' It's utility depends on how standardized your data is, and whether you have
#' extra details you need to address.
#'
#' @details
#' Data is assumed to contain three columns (without column names) corresponding
#' to `animal_id`, `date` and `time` (without date). By default
#' they are expected to be separated by white space, but the `sep` argument
#' can be modified to reflect other separators, such as comma- or tab-separated
#' data.
#'
#' The columns `date` and `time` will be combined to extract the
#' date/time of each event. Thus, the `time_format` argument specifies the
#' order of the combined date and time columns and should be in formats usable
#' by the [lubridate::parse_date_time()] function from the
#' [lubridate] package (e.g., "ymd HMS", "mdy HMS", "dmy HMS", etc.). For
#' example, the default "mdy HMS" expects a date column in the format of
#' month/day/year and a time column in the format of H:M:S (note that separators
#' and leading zeros are ignored, thus month-day-year is equivalent to
#' month/day/year, see the `order` argument of the
#' [parse_date_time][lubridate::parse_date_time] function for more information. More complex
#' formats can also be specified:  For example, 09/30/16 2:00 pm can be
#' specified by time_format = "mdy HM p".
#'
#' Logger details are the logger_id and the lat/lon for the logger. A value of 0
#' reflects that the logger_id is in the file name, defined by the pattern
#' logger_pattern. A value of 1 reflects that the logger_id is in the first line
#' of the file, also defined by the pattern logger_pattern. A value of 2
#' reflects that in addition to the logger_id being in the first line ofthe
#' file, the lat/lon information is on the second line, in the format of
#' "latitude, longitude" both in decimal format (spacing doesn't matter, but the
#' comma does).
#'
#' @param r_file Character. The location of a single file to load.
#' @param tz Character. The time zone the date/times are in (should match one of
#'   the zones produced by `OlsonNames())`. Attempts to use user's system
#'   timezone, if none supplied. Defaults to UTC if all else fails.
#' @param tz_disp Character. The time zone the date/times should be displayed in
#'   (if not the same as `tz`; should match one of the zones produced by
#'   `OlsonNames())`. Defaults to tz if none supplied.
#' @param dst Logical. Whether or not to use Daylight Savings. When set to FALSE
#'   timezones are converted to the Etc/GMT+X timezones which do not include
#'   DST. (Note this overrides the timezone specification such that a timezone
#'   of America/Vancouver, which would normally include DST in the summer, will
#'   be transformed to a timezone with the same GMT offset, but not including
#'   DST).
#' @param details Numeric. Where to find logger details, either 0 (file name),
#'   1 (first line) or 2 (first two lines). See 'details'.
#' @param logger_pattern Character. A regular expression matching the logger id
#'   in the file name. NA (default) matches file name (extension omitted) or
#'   first line of the file (See the `details` argument). Alternatively,
#'   "[GPR]\{2,3\}[0-9]\{1,2\}" would match the names of TRU loggers.
#' @param feeder_pattern Deprecated. Use logger_pattern.
#' @param time_format Character. The date/time format of the 'date' and 'time'
#'   columns combined. Defaults to "mdy HMS". Should be in formats usable by the
#'   `parse_date_time()` function from the lubridate package (e.g., "ymd
#'   HMS", "mdy HMS", "dmy HMS", etc.). See details for more information.
#' @param extra_pattern Character vector. A vector of regular expressions
#'   matching any extra information in the file or directory names or in the
#'   first line of the file.
#' @param extra_name Character vector. A vector of column names matching the
#'   order of `extra_pattern` for storing the results of the pattern.
#' @param sep Character. An override for the separator in the
#'   `read.table()` call (see `sep =` under `?read.table` for
#'   more details).
#' @param skip Character. Extra lines to skip in addition to the lines specified
#'   by details.
#' @param verbose Logical. Whether to include progress messages or not.
#'
#' @examples
#' \dontrun{
#' # Load a single raw file:
#' r <- load_raw("GPR13DATA_2015_12_01.csv")
#'
#' # Modify logger pattern (match only "GPR13")
#' r <- load_raw("GPR13DATA_2015_12_01.csv", logger_pattern = "[GPR]{2,3}[0-9]{1,2}")
#'
#' # Modify logger pattern (match ids like: 2300, 2500, 2550)
#' r <- load_raw("2300.csv", logger_pattern = "[0-9]{4}")
#'
#' # Load a file where the logger id is detected as the first line in the file,
#' not the file name (still use default skip = 1):
#' r <- load_raw("2016-01-01_09_30.csv", details = 1)
#'
#' # Note that the following won't work because the pattern matches both the
#' logger id as well as the year:
#' r <- load_raw("2300_2015_12_01.csv", logger_pattern = "[0-9]{4}")
#'
#' # Extract extra data to be stored in another column:
#' r <- load_raw("2300.csv", extra_pattern = "exp[0-9]{1}", extra_name = experiment)
#'
#' }
#' @export
load_raw <- function(r_file,
                     tz = Sys.timezone(), tz_disp = NULL, dst = FALSE,
                     details = 1, logger_pattern = NA,
                     time_format = "mdy HMS",
                     extra_pattern = NULL, extra_name = NULL,
                     sep = "", skip = 0, verbose = TRUE,
                     feeder_pattern) {

  # Error Checks
  r_file <- try(as.character(r_file), silent = TRUE)
  if(class(r_file) != "character") stop("r_file must coercible to character")
  if(length(r_file) > 1) stop("r_file can only be length 1, the file name.")
  if(!(details %in% 0:2)) stop("'details' must be one of 0, 1, or 2.")

  # Check deprecated arguments
  if (!missing(feeder_pattern)) {
    warning("Argument feeder_pattern is deprecated; please use logger_pattern instead.",
            call. = FALSE)
    logger_pattern <- feeder_pattern
  }

  # Timezone checks
  tz <- check_tz(tz)
  if(is.null(tz_disp)) tz_disp <- tz else tz_disp <- check_tz(tz_disp)

  if(!dst) tz <- tz_offset(tz, tz_name = TRUE)
  if(!dst) tz_disp <- tz_offset(tz_disp, tz_name = TRUE)

  skip <- details + skip

  # Load data
  if(verbose) message("Loading file ", r_file, "...")
  r <- tryCatch(utils::read.table(r_file,
                                  col.names = c("animal_id","date","time"),
                                  colClasses = "character",
                                  skip = skip,
                                  sep = sep),
                error = function(c) {
                  if(grepl("did not have 3 elements", c$message)) {
                    c$message <- paste0(c$message, "\n\nA line did not have the three columns required. Did you specify appropriate 'details' and 'skip' values?")}
                  stop(c)
                })


    if(nrow(r) > 0){
      # Trim leading or trailing whitespace
      r <- dplyr::mutate_all(r, trimws)

      # Get logger ids
      if(details == 0) { # Match patterns in file name
        if(is.na(logger_pattern)) r$logger_id <- stringr::str_extract(basename(r_file), "^[^.]*")
        if(!is.na(logger_pattern)) r$logger_id <- stringr::str_extract(r_file, logger_pattern)
        if(any(is.na(r$logger_id))) stop("logger_id not detected in file name", call. = FALSE)
      } else if (details > 0) { # Get logger id from first line
        if(is.na(logger_pattern)) r$logger_id <- readLines(r_file, n = 1)
        if(!is.na(logger_pattern)) r$logger_id <- stringr::str_extract(readLines(r_file, n = 1), logger_pattern)
        if(any(is.na(r$logger_id))) stop("logger_id not detected from first line of file", call. = FALSE)
        if(nchar(r$logger_id[1]) == nchar(paste(r$animal_id[1], r$date[1], r$time[1]))) warning("logger_id extracted from first line of the file as '",r$logger_id[1], "', this seems odd", call. = FALSE)
      }

      # Get lat, lon
      if(details == 2) {
        locs <- readLines(r_file, n = 2)[2] %>%
          strsplit(split = ",") %>%
          unlist() %>%
          trimws()
        locs <- suppressWarnings(try(as.numeric(locs), silent = TRUE))
        if(class(locs) == "try-error" || is.na(locs) || length(locs) != 2) stop("Expecting one pair of lat/lon on second line of the file. Check format or change 'details'\n(Format should be e.g.,  53.91448, -122.76925).", call. = FALSE)
        r$lat <- locs[1]
        r$lon <- locs[2]
      }

      # Convert animal_id to character for combining later on
      r$animal_id <- as.character(r$animal_id)

      # Convert times
      r$time <- lubridate::parse_date_time(paste(r$date, r$time), orders = time_format, tz = tz)
      if(tz_disp != tz) r$time <- lubridate::with_tz(r$time, tz_disp)
      r$date <- as.Date(r$time, tz = lubridate::tz(r$time))

      # Reorder columns
      cols <- names(r)[names(r) %in% c("animal_id", "date", "time", "logger_id", "lat", "lon")]
      r <- dplyr::select(r, dplyr::all_of(cols)) %>%
        dplyr::arrange(time, animal_id)

      # Get any extra columns by matching patterns in file name as specified by extra_pattern and extra_name
      if(!is.null(extra_pattern)){
        if(is.null(extra_name)) stop("You have specified patterns to match for extra columns, but you have not specified what these column names ('extra_name') should be.")
        for(i in 1:length(extra_pattern)) r[, extra_name[i]] <- stringr::str_extract(r_file, extra_pattern[i])
      } else if(!is.null(extra_name)) stop("You have specified names for extra columns, but you have not specified what pattern to match for filling ('extra_pattern').")

      return(r)
    } else if(verbose) message("Empty file skipped: ", r_file)
}

#' Load and combine raw data files
#'
#' This is a wrapper function which loads and combines raw data files. If
#' `r_dir` is specified, these include all files in series of nested
#' folders, if `r_list` is specified it includes only the list of files
#' specified.
#'
#' @details
#'
#' Note that if both `r_dir` and `r_list` are specified, the directory
#' overrides the file list.
#'
#' Each data file is assumed to contain three columns (without column names)
#' corresponding to `animal_id`, `date` and `time` (without
#' date). By default they are expected to be separated by white space, but the
#' `sep` argument can be modified to reflect other separators, such as
#' comma- or tab-separated data.
#'
#' The columns `date` and `time` will be combined to extract the
#' date/time of each event. Thus, the `time_format` argument specifies the
#' order of the combined date and time columns and should be in formats usable
#' by the [lubridate::parse_date_time()] function from the
#' [lubridate] package (e.g., "ymd HMS", "mdy HMS", "dmy HMS", etc.). For
#' example, the default "mdy HMS" expects a date column in the format of
#' month/day/year and a time column in the format of H:M:S (note that separators
#' and leading zeros are ignored, thus month-day-year is equivalent to
#' month/day/year, see the `order` argument of the
#' [parse_date_time][lubridate::parse_date_time] function for more information. More complex
#' formats can also be specified:  For example, 09/30/16 2:00 pm can be
#' specified by time_format = "mdy HM p".
#'
#' Logger details are the logger_id and the lat/lon for the logger. A value of 0
#' reflects that the logger_id is in the file name, defined by the pattern
#' logger_pattern. A value of 1 reflects that the logger_id is in the first line
#' of the file, also defined by the pattern logger_pattern. A value of 2
#' reflects that in addition to the logger_id being in the first line ofthe
#' file, the lat/lon information is on the second line, in the format of
#' "latitude, longitude" both in decimal format (spacing doesn't matter, but the
#' comma does).
#'
#' @param r_dir Character. The director that holds all your raw data files (can
#'   be in subdirectories).
#' @param r_list Character. A list of files to import.
#' @param pattern Character. A regular expression pattern that matches the files
#'   you wish to include. Defaults to "DATA" to include only DATA files and not
#'   NOTE files.
#' @param tz Character. The time zone the date/times are in (should match one of
#'   the zones produced by `OlsonNames())`. Attempts to use user's system
#'   timezone, if none supplied. Defaults to UTC if all else fails.
#' @param tz_disp Character. The time zone the date/times should be displayed in
#'   (if not the same as `tz`; should match one of the zones produced by
#'   `OlsonNames())`.
#' @param dst Logical. Whether or not to use Daylight Savings. When set to FALSE
#'   timezones are converted to the Etc/GMT+X timezones which do not include
#'   DST. (Note this overrides the timezone specification such that a timezone
#'   of America/Vancouver, which would normally include DST in the summer, will
#'   be transformed to a timezone with the same GMT offset, but not including
#'   DST).
#' @param details Numeric. Where to find logger details, either 0 (file name),
#'   1 (first line) or 2 (first two lines). See 'details'.
#' @param logger_pattern Character. A regular expression matching the logger id
#'   in the file name. NA (default) matches file name (extension omitted) or
#'   first line of the file (See the `details` argument). Alternatively,
#'   "[GPR]\{2,3\}[0-9]\{1,2\}" would match the names of TRU loggers.
#' @param time_format Character. The date/time format of the 'date' and 'time'
#'   columns combined. Defaults to "mdy HMS". Should be in formats usable by the
#'   `parse_date_time()` function from the lubridate package (e.g., "ymd
#'   HMS", "mdy HMS", "dmy HMS", etc.). See details for more information.
#' @param extra_pattern Character vector. A vector of regular expressions
#'   matching any extra information in the file or directory names.
#' @param extra_name Character vector. A vector of column names matching the
#'   order of `extra_pattern` for storing the results of the pattern.
#' @param sep Character. An override for the separator in the
#'   `read.table()` call (see `sep =` under `?read.table` for
#'   more details).
#' @param skip Character. Extra lines to skip in addition to the lines specified
#'   by details.
#' @param verbose Logical. Whether to include progress messages or not.
#' @param feeder_pattern Deprecated. Use logger_pattern instead.
#'
#' @export
load_raw_all <- function(r_dir, r_list, pattern = "DATA",
                         tz = Sys.timezone(), tz_disp = NULL, dst = FALSE,
                         details = 1, logger_pattern = NA,
                         time_format = "mdy HMS",
                         extra_pattern = NULL, extra_name = NULL,
                         sep = "", skip = 0, verbose = TRUE,
                         feeder_pattern) {

  if (!missing(feeder_pattern)) {
    warning("Argument feeder_pattern is deprecated; please use logger_pattern instead.",
            call. = FALSE)
    logger_pattern <- feeder_pattern
  }

  if(!missing(r_dir)) {
    # Get file locations (match pattern and get all subfiles)
    r_list <- list.files(r_dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
    r_list <- r_list[!grepl("~", r_list)] # Omit temporary files
    if(length(r_list) == 0) stop("Either the directory is empty or your pattern matches no files")
  }

  # Load in data and assign extra colums
  r <- do.call('rbind', lapply(r_list, load_raw,
                               details = details,
                               tz = tz,
                               dst = dst,
                               logger_pattern = logger_pattern,
                               time_format = time_format,
                               extra_pattern = extra_pattern,
                               extra_name = extra_name,
                               sep = sep, skip = skip, verbose = verbose))
  r <- load_format(r, tz = tz, tz_disp = tz_disp)
  return(r)
}


#' Format data
#'
#' Formats manually loaded data. Not necessary if using any of the helper loading functions (e.g., `load_raw()`, `load_raw_all()`, or `data_dl()`.
#'
#' @details
#' Expects at least three named columns in the data: `animal_id`,
#' `logger_id`, `time`. Will rename to all lower case as required.
#' Also handles columns `lat` and `lon` (accepts latitude, longitude,
#' and long, but will be renamed to lat and lon).
#'
#' `time` should be a character or factor in date/time format , e.g.
#' 2016-09-30 14:00:00. The exact format of the date/time can be supplied using
#' the `time_format` argument and should represent a format usable by the
#' [lubridate::parse_date_time()] function from the [lubridate]
#' package (e.g., "ymd HMS", "mdy HMS", "dmy HMS", etc.). For example, 09/30/16
#' 2:00 pm can be specified by time_format = "mdy HM p".
#'
#' @param r Data frame. Data frame to format.
#' @param tz Character. The time zone the date/times are in (should match one of
#'   the zones produced by `OlsonNames())`. Attempts to use user's system
#'   timezone, if none supplied. Defaults to UTC if all else fails.
#' @param tz_disp Character. The time zone the date/times should be displayed in
#'   (if not the same as `tz`; should match one of the zones produced by
#'   `OlsonNames())`.
#' @param dst Logical. Whether or not to use Daylight Savings. When set to FALSE
#'   timezones are converted to the Etc/GMT+X timezones which do not include
#'   DST. (Note this overrides the timezone specification such that a timezone
#'   of America/Vancouver, which would normally include DST in the summer, will
#'   be transformed to a timezone with the same GMT offset, but not including
#'   DST).
#' @param time_format Character. The date/time format of the 'time' column. Defaults
#'   to "ymd HMS". Should be in formats usable by the `parse_date_time()`
#'   function from the lubridate package (e.g., "ymd HMS", "mdy HMS", "dmy HMS",
#'   etc.).
#' @param verbose Logical. Whether or not to print messages when renaming
#'   columns.
#'
#' @export
load_format <- function(r, tz = Sys.timezone(), tz_disp = NULL, dst = FALSE, time_format = "ymd HMS", verbose = TRUE){

  # Check timezones
  tz <- check_tz(tz)
  if(is.null(tz_disp)) tz_disp <- tz else tz_disp <- check_tz(tz_disp)
  if(!dst) tz <- tz_offset(tz, tz_name = TRUE)
  if(!dst) tz_disp <- tz_offset(tz_disp, tz_name = TRUE)

  # Trim leading or trailing whitespace
  r <- dplyr::mutate_if(r,
                        .predicate = ~ is.factor(.x) | is.character(.x),
                        .funs = trimws)

  # If locs combined, split apart
  if("loc" %in% names(r)) {
    r$lon <- as.numeric(gsub("\\(([-0-9.]+),[-0-9.]+\\)", "\\1", r$loc))
    r$lat <- as.numeric(gsub("\\([-0-9.]+,([-0-9.]+)\\)", "\\1", r$loc))
    r <- r[, names(r) != "loc",]
  }

  # Check input names
  r <- check_input(r, input = "animal_id", options = c("animal_id", "bird_id"), verbose = verbose)
  r <- check_input(r, input = "logger_id", options = c("logger_id", "feeder_id"), verbose = verbose)
  r <- check_input(r, input = "lon", options = c("lon", "longitude", "long"), verbose = verbose)
  r <- check_input(r, input = "lat", options = c("lat", "latitude"), verbose = verbose)
  r <- check_input(r, input = "time", options = "time", verbose = verbose)
  r <- check_input(r, input = "date", options = "date", verbose = verbose)

  # Extract Proper Date and Times
  if("time" %in% names(r)){
    if(!lubridate::is.POSIXct(r$time)) {
      r$time <- lubridate::parse_date_time(r$time, orders = time_format, tz = tz, truncated = 1)
    }
    if(tz != tz_disp) r$time <- lubridate::with_tz(r$time, tz_disp)
    r$date <- as.Date(r$time, tz = lubridate::tz(r$time))
  }

  # Make sure all factors are factors:
  if(any(names(r) == "animal_id")) r$animal_id <- as.factor(r$animal_id)
  if(any(names(r) == "logger_id")) r$logger_id <- as.factor(r$logger_id)

  # If locs already present, convert to numeric
  if(all(c("lat", "lon") %in% names(r))) {
    r$lon <- as.numeric(as.character(r$lon))
    r$lat <- as.numeric(as.character(r$lat))
  }

  # Reorder columns
  cols <- c("animal_id", "date", "time", "logger_id")
  cols <- cols[which(cols %in% names(r))]
  r <- r[, c(cols, names(r)[!(names(r) %in% cols)])]

  return(r)
}
