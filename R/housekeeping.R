
#' Check for and omit non-bird bird ids
#'
#' This function pads all short ids with leading zeros, then compares the bird
#' ids in your data file to those in an index file. Any ids matched to a species
#' with a value in the \code{omit_bird} argument will be omitted from the data
#' set.
#'
#' This is useful for removing ids you know are errors (e.g., \code{0000000000})
#' or which you know are wands.
#'
#' @param r Data frame. A data frame of read data, with at least column
#'   \code{bird_id}.
#' @param bird_ids Character or Data frame. Either the name and location of a
#'   .csv file containing the bird id index or a data frame of the bird id
#'   index. In either case, the data must contain two columns: \code{bird_id}
#'   and \code{species}.
#' @param omit_bird Character vector. All the values of \code{species} in the
#'   bird id index which you would like to omit from your data. Defaults to
#'   \code{c("wand", "error")}.
#' @return A data frame without the specified bird ids. Messages are printed to
#'   inform the user of matching or non-matching bird ids.
#'
#'
#' @examples
#' \dontrun{
#' r <- load.web("downloaded_file.csv")
#' r <- check.ids(r, bird_ids = "bird_index.csv")
#'
#' r <- load.web("downloaded_file.csv")
#' b <- read.csv("bird_index.csv")
#' r <- check.ids(r, bird_ids = b)
#' }
#' @export
check.ids <- function(r, bird_ids, omit_bird = c("wand", "error")){
  if(length(bird_ids) > 1 & !is.data.frame(bird_ids)) stop("bird_ids should either be the name of a comma separated file (csv) OR should be a data frame. In either case, the data should contain headers 'bird_id' and 'species'")

  if(!is.data.frame(bird_ids)) bird_ids <- read.csv(bird_ids)

  # Check for other id problems
  if(any(nchar(as.character(bird_ids$bird_id)) != 10)) stop("You have some bird_ids in your bird_id index that are not 10 characters long")
  if(any(nchar(as.character(r$bird_id)) != 10)) stop("You have some bird_ids in your read data that are not 10 characters long")

  # Look for unknown ids in your data
  unlisted <- unique(r$bird_id[!(r$bird_id  %in% unique(bird_ids$bird_id))])
  if(length(unlisted) > 0) message(paste("Some ids present in your data do not exist in the bird_id index:", paste(unlisted, collapse = ", "))) else message("All ids in your data are also in your bird_id index")

  # Look for individuals in your bird_ids that are not in your data
  unseen <- unique(bird_ids$bird_id[!(bird_ids$bird_id %in% unique(r$bird_id))])
  if(length(unseen) > 0) message(paste("Some ids present in your bird_id index, are not in your data:", paste0(unseen, collapse = ", "))) else message("All ids in your bird_id index are also in your data")

  # Which ids in the data set match the "omit" section?
  if(!is.data.frame(bird_ids)) bird_ids <- read.csv(bird_ids)
  ob <- unique(bird_ids[bird_ids$species %in% omit_bird, "bird_id"]) # Which to omit in general
  ob2 <- unique(r$bird_id[r$bird_id %in% ob]) # Which to omit in this case
  if(length(ob2) > 0) message(paste("The following bird ids have been omitted:",paste0(ob2, collapse = ", "))) else message("No ids have been omitted")

  # Only keep those that aren't in the "omit" section (also keeps ids not in any section: 'unlisted')
  r <- r[!(r$bird_id %in% ob),]
  return(r)
}


#' Check for and correct odd bird ids
#'
#' This function compares the bird ids in your data file to those in a problem
#' index file. Any ids matched will be replaced with the corrected value in the
#' problem index.
#'
#' This is useful for correcting ids that have been mangled by opening files in
#' Word (where leading zeros are stripped) or for dealing with any odd problems
#' in the field.
#'
#' @param r Data frame. A data frame of read data, with at least column
#'   \code{bird_id}.
#' @param problems Character or Data frame. Either the name and location of a
#'   .csv file containing the problem index or a data frame of the problem
#'   index. In either case, the data must contain two columns:
#'   \code{original_id} and \code{corrected_id}.
#'
#' @return A data frame with corrected ids. Messages are printed to inform the
#'   user of any corrections made.
#'
#' @export
check.problems <- function(r, problems){
  if(length(problems) > 1 & !is.data.frame(problems)) stop("Problems should either be the name of a comma separated file (csv) OR should be a data frame. In either case, the data should contain headers 'original_id' and 'corrected_id'")

  # Get factor categories for bird_id
  birds <- levels(r$bird_id)
  r$bird_id <- as.character(r$bird_id)

  # If problems is a file, load it
  if(!is.data.frame(problems)) problems <- read.csv(problems)

  if(!all(names(problems) %in% c("original_id", "corrected_id"))) stop("Problems should contain headers 'original_id' and 'corrected_id'")
  if(nrow(problems) < 1) stop("Problems data frame has no rows")

  # Trim leading or trailing whitespace
  problems <- data.frame(apply(problems, MARGIN = 2, FUN = trimws))

  problems$original_id <- as.character(problems$original_id)
  problems$corrected_id <- as.character(problems$corrected_id)

  # Fix problem IDs
  fixes <- data.frame()
  for(i in 1:nrow(problems)){
    if(length(r$bird_id[r$bird_id == problems$original_id[i]]) > 0) fixes <- rbind(fixes, problems[i,])
    r$bird_id[r$bird_id == problems$original_id[i]] <- problems$corrected_id[i]
  }

  # Get factors back, but remove old level and add new
  birds <- birds[!(birds %in% fixes$original_id)]
  birds <- c(birds, fixes$corrected_id)

  r$bird_id <- factor(r$bird_id, levels = birds)

  if(nrow(fixes) > 0) {
    message("The following bird ids have been corrected:")
    message(paste0(apply(fixes, 1, paste0, collapse = " to "), collapse = "\n"))
  } else message("No bird ids needed to be fixed")
  return(r)
}


#' Get timezone from lat/lon
#'
#' @param coords Vector or Data frame. Lat, lon coordinates. Can be a vector for
#'   a set of two, or a data frame or matrix for multiple. Provide either coords OR lat and lon
#' @param lat Vector. One or more latitudes, must also provide longitude
#' @param lon Vector. One or more longitudes, must also provide latitude
#'
#' @examples
#'
#' lat = 53.881857
#' lon = -122.786271
#'
#' @import magrittr
#' @export

get.tz <- function(coords = NULL, lat = NULL, lon = NULL, etc = FALSE){
  # Based on http://stackoverflow.com/a/23414695

  if(!is.null(coords)) {
    if(is.vector(coords)) {
      x <- data.frame(lat = coords[1], lon = coords[2])
    } else if(is.matrix(coords)) {
      x <- data.frame(coords)
    } else if(is.data.frame(coords)) x <- coords
  } else if(all(!is.null(lat), !is.null(lon))) {
    x <- data.frame(lat = lat, lon = lon)
  } else stop("Must provide lat and lon either as vector to 'coords' or individual in both 'lat' and 'lon'")

  if(!all(apply(x, 2, is.numeric))) stop("Coordinates must be numeric")

  tz <- vector()
  offset <- vector()
  for(i in 1:nrow(x)) {
    time1 <- Sys.time()
  # https://developers.google.com/maps/documentation/timezone/
    apiurl <- paste0("https://maps.googleapis.com/maps/api/timezone/xml?",
                     "location=", x[i,1], ",", x[i,2], "&",
                     "timestamp=", as.numeric(time1), "&",
                     "sensor=false")
    tz <- c(tz, xml2::read_xml(apiurl) %>% xml2::xml_find_all("//time_zone_id") %>% xml2::xml_text())
    offset <- c(offset, xml2::read_xml(apiurl) %>% xml2::xml_find_all("//raw_offset") %>% xml2::xml_text())
  }

  offset <-  as.numeric(offset) / 60 / 60

  return(list(tz, offset))

}
