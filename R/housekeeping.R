
#' Check for and omit non-animal animal ids
#'
#' This function pads all short ids with leading zeros, then compares the animal
#' ids in your data file to those in an index file. Any ids matched to a species
#' with a value in the \code{omit_animal} argument will be omitted from the data
#' set.
#'
#' This is useful for removing ids you know are errors (e.g., \code{0000000000})
#' or which you know are wands.
#'
#' @param r Data frame. A data frame of read data, with at least column
#'   \code{animal_id}.
#' @param ids Character or Data frame. Either the name and location of a
#'   .csv file containing the animal id index or a data frame of the animal id
#'   index. In either case, the data must contain two columns: \code{animal_id}
#'   and \code{species}.
#' @param omit Character vector. All the values of \code{species} in the
#'   animal id index which you would like to omit from your data. Defaults to
#'   \code{c("wand", "error")}.
#' @param id_length Numeric. How many characters are expected in each id? This
#'   will test to make sure all ids are the right length (i.e. make sure
#'   leading zeros haven't been omitted). NA skips this test.
#'
#' @return A data frame without the specified animal ids. Messages are printed to
#'   inform the user of matching or non-matching animal ids.
#'
#'
#' @examples
#' \dontrun{
#' r <- check_ids(finches, ids = "animal_index.csv")
#'
#' index <- read.csv("animal_index.csv")
#' r <- check_ids(finches, ids = index)
#' }
#' @export
check_ids <- function(r, ids, omit = c("wand", "error"), id_length = 10, bird_ids){

  if (!missing(bird_ids)) {
    warning("Argument bird_ids is deprecated; please use ids instead.",
            call. = FALSE)
    ids <- bird_ids
  }

  if(is.null(ids) || (length(ids) > 1 & !is.data.frame(ids))) stop("ids should either be the name of a comma separated file (csv) OR should be a data frame. In either case, the data should contain headers 'animal_id' and 'species'")

  if(!is.data.frame(ids)) ids <- utils::read.csv(ids)

  # Check for other id problems
  if(!is.na(id_length)){
    msg <- character()
    if(any(nchar(as.character(ids$animal_id)) != id_length)) msg <- c(msg, paste0("You have some ids in your animal_id index that are not ", id_length, " characters long"))
    if(any(nchar(as.character(r$animal_id)) != id_length)) msg <- c(msg, paste0("You have some ids in your read data that are not ", id_length, " characters long"))
    if(length(msg) > 0) stop(paste0(msg, collapse = "\n  "))
  }

  # Look for unknown ids in your data
  unlisted <- unique(r$animal_id[!(r$animal_id  %in% unique(ids$animal_id))])
  if(length(unlisted) > 0) message(paste("Some ids present in your data do not exist in the animal_id index:", paste(unlisted, collapse = ", "))) else message("All ids in your data are also in your animal_id index")

  # Look for individuals in your ids that are not in your data
  real_animals <- unique(ids$animal_id[!(ids$species %in% omit)])
  unseen <- real_animals[!(real_animals %in% unique(r$animal_id))]
  if(length(unseen) > 0) message(paste("Some ids present in your animal_id index, are not in your data:", paste0(unseen, collapse = ", "))) else message("All ids in your animal_id index are also in your data")

  # Which ids in the data set match the "omit" section?
  if(!is.data.frame(ids)) ids <- utils::read.csv(ids)
  ob <- unique(ids[ids$species %in% omit, "animal_id"]) # Which to omit in general
  ob2 <- unique(r$animal_id[r$animal_id %in% ob]) # Which to omit in this case
  if(length(ob2) > 0) message(paste("The following animal ids have been omitted:",paste0(ob2, collapse = ", "))) else message("No ids have been omitted")

  # Only keep those that aren't in the "omit" section (also keeps ids not in any section: 'unlisted')
  r <- r[!(r$animal_id %in% ob),]
  return(r)
}


check.ids <- function(r, animal_ids, omit_animal = c("wand", "error")){
  .Deprecated("check_ids")
  check_ids(r, animals_ids, omit_animal)
}

#' Check for and correct odd animal ids
#'
#' This function compares the animal ids in your data file to those in a problem
#' index file. Any ids matched will be replaced with the corrected value in the
#' problem index.
#'
#' This is useful for correcting ids that have been mangled by opening files in
#' Word (where leading zeros are stripped) or for dealing with any odd problems
#' in the field.
#'
#' @param r Data frame. A data frame of read data, with at least column
#'   \code{animal_id}.
#' @param problems Character or Data frame. Either the name and location of a
#'   .csv file containing the problem index or a data frame of the problem
#'   index. In either case, the data must contain two columns:
#'   \code{original_id} and \code{corrected_id}.
#'
#' @return A data frame with corrected ids. Messages are printed to inform the
#'   user of any corrections made.
#'
#' @import magrittr
#' @export
check_problems <- function(r, problems){
  if(length(problems) > 1 & !is.data.frame(problems)) stop("Problems should either be the name of a comma separated file (csv) OR should be a data frame. In either case, the data should contain headers 'original_id' and 'corrected_id'")

  # Confirm that expected columns and formats are present
  check_name(r, n = c("animal_id", "logger_id", "time"), "raw RFID")
  check_time(r, n = "time", internal = FALSE)
  check_format(r)

  # Get factor categories for animal_id
  animals <- levels(r$animal_id)
  r$animal_id <- as.character(r$animal_id)

  # If problems is a file, load it
  if(!is.data.frame(problems)) problems <- utils::read.csv(problems)

  if(!all(names(problems) %in% c("original_id", "corrected_id"))) stop("Problems should contain headers 'original_id' and 'corrected_id'")
  if(nrow(problems) < 1) stop("Problems data frame has no rows")

  # Trim leading or trailing whitespace
  problems <- problems %>%
    dplyr::mutate_all(trimws) %>%
    dplyr::mutate_all(as.character)

  # Fix problem IDs
  fixes <- data.frame()
  for(i in 1:nrow(problems)){
    if(length(r$animal_id[r$animal_id == problems$original_id[i]]) > 0) fixes <- rbind(fixes, problems[i,])
    r$animal_id[r$animal_id == problems$original_id[i]] <- problems$corrected_id[i]
  }

  # Get factors back, but remove old level and add new
  animals <- animals[!(animals %in% fixes$original_id)]
  animals <- c(animals, fixes$corrected_id)

  r$animal_id <- factor(r$animal_id, levels = animals)

  if(nrow(fixes) > 0) {
    message("The following animal ids have been corrected:")
    message(paste0(apply(fixes, 1, paste0, collapse = " to "), collapse = "\n"))
  } else message("No animal ids needed to be fixed")
  return(r)
}

check.problems <- function(r, problems){
  .Deprecated("check_problems")
  check_ids(r, problems)
}

