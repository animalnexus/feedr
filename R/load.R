
#' Load read data from the web (legacy)
#'
#' Loads raw read data from csv files downloaded from the __old__ version of \url{http://gaia.tru.ca/birdMOVES/datadownload.html} website and formats them for use with the feedr functions. This is merely a wrapper function that does many things that you can do yourself. It's utility depends on how standardized your data is, and whether you have extra details you need to address.
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

  # Trim any trailing or leading white spaces
  r <- data.frame(apply(r, MARGIN = 2, FUN = trimws))
  r <- r[,c("bird_id","time", "feeder_id")]

  return(r)
}

#' Load read data from the web
#'
#' Loads raw read data from csv files downloaded from the __current__ \url{http://gaia.tru.ca/birdMOVES/datadownload.html} website and formats them for use with the feedr functions. This is merely a wrapper function that does many things that you can do yourself. It's utility depends on how standardized your data is, and whether you have extra details you need to address. This is updated to work with newer data download formats.
#'
#' This function is also used by \code{\link{get.data}} and so can accept a data frame in place of a file location.
#'
#' @param r_file Character or Data frame. The location of a single file to load, or a data frame of raw reads to be formated.
#' @param tz Character. The time zone the date/times are in (should match one of the zones produced by \code{OlsonNames())}.
#' @param sep Character. An override for the separator in the \code{read.table()} call (see \code{sep =} under \code{?read.table} for more details).
#'
#' @examples
#' \dontrun{r <- load.web("downloaded_file.csv")}

#' @export
load.web <- function(r_file, tz = "Canada/Pacific", sep = ",") {
  if(!is.data.frame(r_file)) {
    r <- read.csv(r_file, strip.white = TRUE)
    r <- r[, c("feeder_id","bird_id","timezone")]
    } else r <- r_file

  names(r)[names(r) == "timezone"] <- "time"
  r$time <- as.POSIXct(r$time, tz = tz)

  # Remove all leading and trailing spaces from observations
  r <- data.frame(apply(r, MARGIN = 2, FUN = trimws))

  r <- r[,c("bird_id","time", "feeder_id", names(r)[!(names(r) %in% c("bird_id", "time", "feeder_id"))])]
  return(r)
}

#' Load raw read data
#'
#' Loads raw read data and formats for use with the feedr functions. This is merely a wrapper function that does many things that you can do yourself. It's utility depends on how standardized your data is, and whether you have extra details you need to address
#'
#' @param r_file Character. The location of a single file to load.
#' @param tz Character. The time zone the date/times should be converted to (should match one of the zones produced by \code{OlsonNames())}.
#' @param feeder_pattern Character. A regular expression matching the feeder id in the file name.
#' @param extra_pattern Character vector. A vector of regular expressions matching any extra information in the file or directory names.
#' @param extra_name Character vector. A vector of column names matching the order of \code{extra_pattern} for storing the results of the pattern.
#' @param sep Character. An override for the separator in the \code{read.table()} call (see \code{sep =} under \code{?read.table} for more details).
#' @param skip Character. An override for the skip in the \code{read.table()} call (see \code{skip =} under \code{?read.table} for more details).
#'
#' @examples
#' \dontrun{
#' # Load a single raw file:
#' r <- load.raw("GPR13DATA_2015_12_01.csv")
#'
#' # Load a single raw file where the feeder id is not the default, and is rather something like: 2300, 2500, 2550
#' r <- load.raw("2300.csv", feeder_pattern = "[0-9]{4}")
#'
#' # Note that the following won't work because the pattern matches both the feeder id as well as the year:
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
#' # Use lapply to apply the load.raw function to each file in l, then use do.call to rbind them all together
#' r <- do.call('rbind', lapply(l, load.raw))
#'
#' # All together now:
#' r <- do.call('rbind', lapply(l, load.raw, extra_pattern = "exp[0-9]{1}", extra_name = "experiment"))
#' }
#' @export
load.raw <- function(r_file, tz = "Canada/Pacific", feeder_pattern = "[GPR]{2,3}[0-9]{1,2}", extra_pattern = NULL, extra_name = NULL, sep = "", skip = 1) {
    r <- read.table(r_file, col.names = c("bird_id","date","time"), skip = skip, sep = sep)

    # Get feeder Ids by matching patterns in file name
    r$feeder_id <- stringr::str_extract(r_file, feeder_pattern)

    # Convert bird_id to character for combining later on
    r$bird_id <- as.character(r$bird_id)

    # Extract Proper Date and Times
    r$time <- as.POSIXct(strptime(paste(r$date, r$time), format = "%m/%d/%y %H:%M:%S",tz = tz))

    # Trim leading or trailing whitespace
    r <- data.frame(apply(r, MARGIN = 2, FUN = trimws))
    r <- r[,c("bird_id","time","feeder_id")]

    # Get any extra columns by matching patterns in file name as specified by extra_pattern and extra_name
    if(!is.null(extra_pattern)){
      if(is.null(extra_name)) stop("You have specified patterns to match for extra columns, but you have not specified what these column names ('extra_name') should be.")
      for(i in 1:length(extra_pattern)) r[,extra_name[i]] <- stringr::str_extract(r_file, extra_pattern[i])
    } else if(!is.null(extra_name)) stop("You have specified names for extra columns, but you have not specified what pattern to match for filling ('extra_pattern').")
    return(r)
}


#' Download data from Thompson Rivers University "BirdMoves" website.
#'
#' This function uses RCurl to submit an HTML form and retrieve the csv file. This is simply a convenience function to replace going to the website yourself (http://gaia.tru.ca/birdMOVES/datadownload.html) and then running the data frame through \code{\link{load.web}}.
#'
#' Note that while the website requires a date in the format of YYYY-MM-DD HH:MM:SS, this function is a bit more flexible. Using \code{\link[lubridate]{parse_date_time}} from the lubridate package, the format of the date/time supplied will estimated. This allows for partial date/times to be supplied (e.g., "2015-01-01 09" or "2015-09" or "2015-09-01"). For best results, specify the date/time format as YYYY-MM-DD HH:MM:SS and truncate as needed. Note that truncated times are interpreted as 00 and trucated dates as the first of the month and the first month of the year. Thus "2015" will be sumbitted as "2015-01-01 00:00:00".
#'
#' For specifying extra \code{bird_details}:
#'
#' - "species" gives species name
#' - "sex" gives sex (M, F, U for male, female and unknown)
#' - "age" gives age (ASY, AHY, SY, and HY for After Second Year, After Hatch Year, Second Year, and Hatch Year)
#'   See \href{https://www.pwrc.usgs.gov/BBL/manual/age.cfm}{USGS banding lab} for details about banding
#' - "tagged_on" gives you the date the RFID tag was mounted on the bird
#'
#' For specifying extra \code{feeder_details}:
#'
#' - "site_name" gives site name
#' - "loc" gives lon/lat coordiates
#'
#' @param start Character. This is the start date (with or without time) for the data to download. There is some flexibility in the format (see details). If NULL, get records from start.
#' @param end  Character. This is the end date (with or without time) for the data to download. There is some flexibility in the format (see details). If NULL, get records to end.
#' @param url Character. This is the url for the web form action. _Not_ the url where users go to download their data by hand. The default should not need to be changed.
#' @param feeder_details Character vector. This specifies extra columns with details about the feeders to download. Use NULL to download no extra columns.
#' @param bird_details Character vector. This specifies extra columns with details about the birds to download. Use NULL to download no extra columns.
#' @param tz Character vector. Timezone to use for specifying and downloading data. Timezone must be one of "America/Vancouver" (default), "America/Costa Rica", or "GMC".
#' @param sites Character vector. Sites to download data from. Currently must be one or more of "Kamloops" (default) or "Costa Rica".
#'
#' @examples
#' \dontrun{
#' # Get all data (may take a couple minutes)
#' r <- get.data()
#'
#' # Get all 2016 data
#' r <- get.date(start = "2016")
#'
#' # Get specific data
#' r <- get.data(start = "2016-01-01 09:34:12", end = "2016-02-01", bird_details = c("species", "age", "sex", "tagged_on"))
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
  if(!url.exists(url)) stop("The url '", url, "' doesn't exist (or you have no internet connection).")

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
  r <- load.web(read.csv(text = g, strip.white = TRUE), tz = tz)
  return(r)
}

