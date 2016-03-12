#' Load read data from the web
#'
#' Loads raw read data from csv files downloaded from the \url{http://gaia.tru.ca/birdMOVES/datadownload.html} website and formats them for use with the feedr functions. This is merely a wrapper function that does many things that you can do yourself. It's utility depends on how standardized your data is, and whether you have extra details you need to address.
#'
#' @param r_file Character. The location of a single file to load.
#' @param tz Character. The time zone the date/times should be converted to (should match one of the zones produced by \code{OlsonNames())}.
#' @param sep Character. An override for the separator in the \code{read.table()} call (see \code{sep =} under \code{?read.table} for more details).
#'
#' @examples
#' \dontrun{r <- load.web("downloaded_file.csv")}


#' @export
load.web <- function(r_file, tz = "Canada/Pacific", sep = ",") {
  r <- read.table(r_file, sep = ",", col.names = c("date","feeder_id","bird_id"))
  r$time <- as.POSIXct(strptime(r$date, "%m-%d-%yT%H:%M:%SZ", tz = "Zulu"))
  attributes(r$time)$tzone <- tz
  r <- r[,c("bird_id","time", "feeder_id")]
  return(r)
}

#' Load read data from the web (update)
#'
#' Loads raw read data from csv files downloaded from the \url{http://gaia.tru.ca/birdMOVES/datadownload.html} website and formats them for use with the feedr functions. This is merely a wrapper function that does many things that you can do yourself. It's utility depends on how standardized your data is, and whether you have extra details you need to address. This is updated to work with newer data download formats
#'
#' @param r_file Character. The location of a single file to load.
#' @param tz.to Character. The time zone the date/times should be converted to (should match one of the zones produced by \code{OlsonNames())}.
#' @param tz.from Character. The time zone the date/times should be converted from (should match one of the zones produced by \code{OlsonNames())}.
#' @param sep Character. An override for the separator in the \code{read.table()} call (see \code{sep =} under \code{?read.table} for more details).
#'
#' @examples
#' \dontrun{r <- load.web("downloaded_file.csv")}

#' @export
load.web2 <- function(r_file, tz.from = "GMT", tz.to = "Canada/Pacific", sep = ",") {
  r <- read.csv(r_file)
  r <- r[, c("feeder_id","bird_id","timezone")]
  names(r)[3] <- "time"
  r$time <- as.POSIXct(r$time, tz = tz.from)
  attributes(r$time)$tzone <- tz.to
  r <- r[,c("bird_id","time", "feeder_id")]
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
    r <- r[,c("bird_id","time","feeder_id")]

    # Get any extra columns by matching patterns in file name as specified by extra_pattern and extra_name
    if(!is.null(extra_pattern)){
      if(is.null(extra_name)) stop("You have specified patterns to match for extra columns, but you have not specified what these column names ('extra_name') should be.")
      for(i in 1:length(extra_pattern)) r[,extra_name[i]] <- stringr::str_extract(r_file, extra_pattern[i])
    } else if(!is.null(extra_name)) stop("You have specified names for extra columns, but you have not specified what pattern to match for filling ('extra_pattern').")
    return(r)
}
