
#' Load read data from the web (legacy)
#'
#' Loads raw read data from csv files downloaded from the __old__ version of
#' \url{http://gaia.tru.ca/birdMOVES/datadownload.html} website and formats them
#' for use with the feedr functions. This is merely a wrapper function that does
#' many things that you can do yourself. It's utility depends on how
#' standardized your data is, and whether you have extra details you need to
#' address.
#'
#' @param r_file Character. The location of a single file to load.
#' @param tz Character. The time zone the date/times should be converted to (should match one of the zones produced by \code{OlsonNames())}.
#' @param sep Character. An override for the separator in the \code{read.table()} call (see \code{sep =} under \code{?read.table} for more details).
#'
#' @examples
#' \dontrun{r <- load.web.legacy("downloaded_file.csv")}


#' @export
load.web.legacy <- function(r_file, tz = "Canada/Pacific", sep = ",") {
  r <- read.table(r_file, sep = ",", col.names = c("date","feeder_id","bird_id"))
  r$time <- as.POSIXct(strptime(r$date, "%m-%d-%yT%H:%M:%SZ", tz = "Zulu"))
  attributes(r$time)$tzone <- tz

  r <- load.format(r, tz = tz)
  return(r)
}

#' Load read data from the web
#'
#' Loads raw read data from csv files downloaded from the __current__
#' \url{http://gaia.tru.ca/birdMOVES/datadownload.html} website and formats them
#' for use with the feedr functions. This is merely a wrapper function that does
#' many things that you can do yourself. It's utility depends on how
#' standardized your data is, and whether you have extra details you need to
#' address. This is updated to work with newer data download formats.
#'
#' This function is also used by \code{\link{get.data}} and so can accept a data
#' frame in place of a file location.
#'
#' @param r_file Character or Data frame. The location of a single file to load,
#'   or a data frame of raw reads to be formated.
#' @param tz Character. The time zone the date/times are in (should match one of
#'   the zones produced by \code{OlsonNames())}.
#' @param sep Character. An override for the separator in the
#'   \code{read.table()} call (see \code{sep =} under \code{?read.table} for
#'   more details).
#'
#' @examples
#' \dontrun{r <- load.web("downloaded_file.csv")}

#' @export
load.web <- function(r_file, tz = "Canada/Pacific", sep = ",") {
  r <- read.csv(r_file, strip.white = TRUE)
  r <- load.format(r, tz = tz)
  return(r)
}

#' Load raw read data
#'
#' Loads raw read data and formats for use with the feedr functions. This is
#' merely a wrapper function that does many things that you can do yourself.
#' It's utility depends on how standardized your data is, and whether you have
#' extra details you need to address
#'
#' @param r_file Character. The location of a single file to load.
#' @param tz Character. The time zone the date/times should be converted to
#'   (should match one of the zones produced by \code{OlsonNames())}.
#' @param feeder_pattern Character. A regular expression matching the feeder id
#'   in the file name.
#' @param extra_pattern Character vector. A vector of regular expressions
#'   matching any extra information in the file or directory names.
#' @param extra_name Character vector. A vector of column names matching the
#'   order of \code{extra_pattern} for storing the results of the pattern.
#' @param sep Character. An override for the separator in the
#'   \code{read.table()} call (see \code{sep =} under \code{?read.table} for
#'   more details).
#' @param skip Character. An override for the skip in the \code{read.table()}
#'   call (see \code{skip =} under \code{?read.table} for more details).
#'
#' @examples
#' \dontrun{
#' # Load a single raw file: r <- load.raw("GPR13DATA_2015_12_01.csv")
#'
#' # Load a single raw file where the feeder id is not the default, and is
#' rather something like: 2300, 2500, 2550
#' r <- load.raw("2300.csv", feeder_pattern = "[0-9]{4}")
#'
#' # Note that the following won't work because the pattern matches both the
#' feeder id as well as the year:
#' r <- load.raw("2300_2015_12_01.csv", feeder_pattern = "[0-9]{4}")
#'
#' # Extract extra data to be stored in another column:
#' r <- load.raw("2300.csv", extra_pattern = "exp[0-9]{1}", extra_name = experiment)
#'
#' # Load and combine multiple files at once:
#'
#' # First list the files in a folder called Data:
#' l <- list.files("./Data", full.names = TRUE)
#'
#' # Use lapply to apply the load.raw function to each file in l, then use
#' do.call to rbind them all together r <- do.call('rbind', lapply(l, load.raw))
#'
#' # All together now:
#' r <- do.call('rbind',
#'              lapply(l, load.raw,
#'                        extra_pattern = "exp[0-9]{1}",
#'                        extra_name = "experiment"))
#' }
#' @export
load.raw <- function(r_file, tz = "Canada/Pacific", feeder_pattern = "[GPR]{2,3}[0-9]{1,2}", extra_pattern = NULL, extra_name = NULL, sep = "", skip = 1) {
    r <- read.table(r_file, col.names = c("bird_id","date","time"), skip = skip, sep = sep)

    # Trim leading or trailing whitespace
    r <- plyr::ddply(r, c(), plyr::colwise(trimws))[ , -1]

    # Get feeder Ids by matching patterns in file name
    r$feeder_id <- stringr::str_extract(r_file, feeder_pattern)

    # Convert bird_id to character for combining later on
    r$bird_id <- as.character(r$bird_id)

    # Get any extra columns by matching patterns in file name as specified by extra_pattern and extra_name
    if(!is.null(extra_pattern)){
      if(is.null(extra_name)) stop("You have specified patterns to match for extra columns, but you have not specified what these column names ('extra_name') should be.")
      for(i in 1:length(extra_pattern)) r[,extra_name[i]] <- stringr::str_extract(r_file, extra_pattern[i])
    } else if(!is.null(extra_name)) stop("You have specified names for extra columns, but you have not specified what pattern to match for filling ('extra_pattern').")

    r$time <- lubridate::parse_date_time(paste(r$date, r$time), orders = "%m/%d/%y %H:%M:%S", tz = tz)
    r <- r[, names(r) != "date"]

    # Reorder columns
    r <- col.order(r, c("bird_id", "time", "feeder_id"))
    return(r)
}

#' Load and combine raw data files
#'
#' This is a wrapper function which loads and combines all raw data files from a
#' series of nested folders.
#'
#' @param r_dir Character. The director that holds all your raw data files (can
#'   be in subdirectories)
#' @param pattern Character. A regular expression pattern that matches the files
#'   you wish to include. Defaults to "DATA" to include only DATA files and not
#'   NOTE files.
#' @param tz Character. The time zone the date/times should be converted to
#'   (should match one of the zones produced by \code{OlsonNames())}.
#' @param feeder_pattern Character. A regular expression matching the feeder id
#'   in the file name.
#' @param extra_pattern Character vector. A vector of regular expressions
#'   matching any extra information in the file or directory names.
#' @param extra_name Character vector. A vector of column names matching the
#'   order of \code{extra_pattern} for storing the results of the pattern.
#' @param sep Character. An override for the separator in the
#'   \code{read.table()} call (see \code{sep =} under \code{?read.table} for
#'   more details).
#' @param skip Character. An override for the skip in the \code{read.table()}
#'   call (see \code{skip =} under \code{?read.table} for more details).
#'
#' @export
load.raw.all <- function(r_dir,
                         pattern = "DATA",
                         tz = "Canada/Pacific",
                         feeder_pattern = "[GPR]{2,3}[0-9]{1,2}",
                         extra_pattern = NULL,
                         extra_name = NULL,
                         sep = "",
                         skip = 1) {
  # Get file locations (match pattern and get all subfiles)
  r_list <- list.files(r_dir, pattern = pattern, recursive = TRUE, full.names = TRUE)

  # Remove temporary files
  r_list <- r_list[!grepl("~", r_list)]

  if(length(r_list) == 0) stop("Either the directory is empty or your pattern matches no files")

  # Load in data and assign extra colums
  r <- do.call('rbind', lapply(r_list,
                               load.raw,
                               tz = tz,
                               feeder_pattern = feeder_pattern,
                               extra_pattern = extra_pattern,
                               extra_name = extra_name,
                               sep = sep, skip = skip))
  r <- load.format(r, tz = tz)
  return(r)
}



#' Download data from BirdMoves website.
#'
#' This function uses RCurl to submit an HTML form and retrieve the csv file.
#' This is simply a convenience function to replace going to the website
#' yourself (http://gaia.tru.ca/birdMOVES/datadownload.html) and then running
#' the data frame through \code{\link{load.web}}.
#'
#' Note that while the website requires a date in the format of YYYY-MM-DD
#' HH:MM:SS, this function is a bit more flexible. Using
#' \code{\link[lubridate]{parse_date_time}} from the lubridate package, the
#' format of the date/time supplied will estimated. This allows for partial
#' date/times to be supplied (e.g., "2015-01-01 09" or "2015-09" or
#' "2015-09-01"). For best results, specify the date/time format as YYYY-MM-DD
#' HH:MM:SS and truncate as needed. Note that truncated times are interpreted as
#' 00 and trucated dates as the first of the month and the first month of the
#' year. Thus "2015" will be sumbitted as "2015-01-01 00:00:00".
#'
#' For specifying extra \code{bird_details}:
#'
#' \itemize{
#'   \item "species" gives species name
#'   \item "sex" gives sex (M, F, U for male, female and unknown)
#'   \item "age" gives age (ASY, AHY, SY, and HY for After Second Year, After
#' Hatch Year, Second Year, and Hatch Year) See
#' \href{https://www.pwrc.usgs.gov/BBL/manual/age.cfm}{USGS banding lab} for
#' details about banding
#'   \item "tagged_on" gives you the date the RFID tag was
#' mounted on the bird
#' }
#'
#' For specifying extra \code{feeder_details}:
#'
#' \itemize{
#'  \item "site_name" gives site name
#'  \item "loc" gives lon/lat coordiates
#'  }
#'
#' @param start Character. This is the start date (with or without time) for the
#'   data to download. There is some flexibility in the format (see details). If
#'   NULL, get records from start.
#' @param end  Character. This is the end date (with or without time) for the
#'   data to download. There is some flexibility in the format (see details). If
#'   NULL, get records to end.
#' @param url Character. This is the url for the web form action. _Not_ the url
#'   where users go to download their data by hand. The default should not need
#'   to be changed.
#' @param feeder_details Character vector. This specifies extra columns with
#'   details about the feeders to download. Use NULL to download no extra
#'   columns.
#' @param bird_details Character vector. This specifies extra columns with
#'   details about the birds to download. Use NULL to download no extra columns.
#' @param tz Character vector. Timezone to use for specifying and downloading
#'   data. Timezone must be one of "America/Vancouver" (default), "America/Costa
#'   Rica", or "GMC".
#' @param sites Character vector. Sites to download data from. Currently must be
#'   one or more of "Kamloops" (default) or "Costa Rica".
#'
#' @examples
#' \dontrun{
#' # Get all Kamloops data (may take a couple minutes)
#' r <- get.data()
#'
#' # Get all Costa Rica data (may take a couple minutes)
#' r <- get.data(sites = "Costa Rica")
#'
#' # Get all data (may take a couple minutes)
#' r <- get.data(sites = c("Kamloops", "Costa Rica"))
#'
#' # Get all 2016 data
#' r <- get.date(start = "2016")
#'
#' # Get specific data
#' r <- get.data(start = "2016-01-01 09:34:12",
#'               end = "2016-02-01",
#'               bird_details = c("species", "age", "sex", "tagged_on"))
#' }
#'
#' @export
get.data <- function(start = NULL,
                     end = NULL,
                     url = "http://gaia.tru.ca/birdMOVES/rscripts/rawvisits.csv",
                     feeder_details = c("loc"),
                     bird_details = c("species"),
                     tz = "America/Vancouver",
                     sites = "Kamloops") {

  # Stop if timezones different
  if(!(tz %in% c("America/Vancouver", "GMT", "America/Costa Rica"))) stop("Timezone must be one of 'America/Vancouver', 'GMT', 'America/Costa Rica'. (You can change timezones after they've been downloaded.)")

  # Stop if time is not in the correct format

  t.start <- NULL
  t.end <- NULL
  if(!is.null(start)) {
    suppressWarnings(t.start <- lubridate::parse_date_time(start, orders = "ymd hms", truncated = 5))
    if(is.na(t.start)) stop("Your start time is ambiguous. Format should be YYYY-MM-DD (HH:MM:SS is optional)")
    t.start <- format(t.start, "%Y-%m-%d %H:%M:%S")
  }
  if(!is.null(end)) {
    suppressWarnings(t.end <- lubridate::parse_date_time(end, orders = "ymd hms", truncated = 5))
    if(is.na(t.end)) stop("Your end time is ambiguous. Format should be YYYY-MM-DD (HH:MM:SS is optional)")
    t.end <- format(t.end, "%Y-%m-%d %H:%M:%S")
  }

  # Stop if url doesn't exist
  if(!RCurl::url.exists(url)) stop("The url '", url, "' doesn't exist (or you have no internet connection).")

  # Stop if sites not in list
  if(!all(sites %in% c("Kamloops", "Costa Rica"))) stop("Sites must be one or more of 'Kamloops' and/or 'Costa Rica'")

  sites <- gsub("Kamloops", "qskam", sites)
  sites <- gsub("Costa Rica", "qscr", sites)

  # Get form options
  params <- as.list(rep("1", length(c(feeder_details, bird_details, sites))))
  names(params) <- c(feeder_details, bird_details, sites)
  params <- append(params,
                   list(feeder_id = "1",
                        bird_id = "1",
                        tz = tz,
                        qstart = t.start,
                        qend = t.end,
                        qstarttz = tz,
                        qendtz = tz))

  g <- RCurl::getForm(url, .params = params)

  if(nchar(g) < 200) stop("There are no online data matching these parameters. Try different sites or a different date range.")

  r <- load.format(read.csv(text = g, strip.white = TRUE, colClasses = "character"), tz = tz)
  return(r)
}

#' Internal function: Format data
#'
#' Formats data for the loading function.
#'
load.format <- function(r, tz){

  # Trim leading or trailing whitespace
  r <- plyr::ddply(r, c(), plyr::colwise(trimws))[ , -1]

  # Extract Proper Date and Times
  names(r)[names(r) == "timezone"] <- "time"
  r$time <- lubridate::ymd_hms(r$time, tz = tz)

  # Make sure all factors are factors:
  r$bird_id <- as.factor(r$bird_id)
  r$feeder_id <- as.factor(r$feeder_id)

  # Reorder columns
  r <- col.order(r, c("bird_id", "time", "feeder_id"))

  return(r)
}

