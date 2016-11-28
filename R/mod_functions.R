startup <- function(x) {
  #require that input objects were at least created (first pass)
  all(c("data_site_name",
        "data_species",
        "data_date",
        "data_bird_id",
        "data_feeder_id"
  ) %in% names(x))
}

get_counts <- function(c, filter = NULL, summarize_by = NULL) {

  if(!is.null(filter)){
    if("species" %in% names(filter))   c <- dplyr::filter(c, species %in% filter$species)
    if("date" %in% names(filter))      c <- dplyr::filter(c, date %within% interval(filter$date[1], filter$date[2]))
    if("bird_id" %in% names(filter))   c <- dplyr::filter(c, bird_id %in% filter$bird_id)
    if("feeder_id" %in% names(filter)) c <- dplyr::filter(c, feeder_id %in% filter$feeder_id)
  }

  if(!is.null(summarize_by)){
    c <- c %>%
      dplyr::group_by_(summarize_by) %>%
      dplyr::summarize(sum = sum(count)) %>%
      tidyr::complete_(summarize_by, fill = list('sum' = 0)) %>%
      dplyr::arrange_(summarize_by) %>%
      dplyr::mutate_(name = lazyeval::interp(~ paste0(var, " (", sum, ")"), var = as.name(summarize_by)),
              variable = ~summarize_by) %>%
      dplyr::rename_("choices" = summarize_by) %>%
      dplyr::mutate(choices = as.character(choices))
  }
  return(c)
}



choices <- function(s, var){
  c <- s$choices[s$variable == var]
  names(c) <- s$name[s$variable == var]
  return(c)
}

selected <- function(s, var){
  s <- s$choices[s$variable == var & s$sum > 0]
  return(s)
}

# Get list of values from i and make sure all have the same levels
# i = NULL means start from scratch
# i = input (or reactive) means dealing with ui input values
# i = anything else means dealing with selection values
values_list <- function(i = NULL, counts){
  if(any(class(i) == "reactivevalues")){
    if(!is.null(i$plot_data_brush)) {
      dates <- c(as.Date(i$plot_data_brush$xmin, lubridate::origin),
                 as.Date(i$plot_data_brush$xmax, lubridate::origin))
      if(dates[1] < min(counts$date)) dates[1] <- min(counts$date)
      if(dates[2] > max(counts$date)) dates[2] <- max(counts$date)
    } else {
      dates <- i$data_date
    }
    d <- list(
      'species' = i$data_species,
      'date' = dates,
      'bird_id' = i$data_bird_id,
      'feeder_id' = i$data_feeder_id)
  } else {
    if(is.null(i)) {
      i <- counts_sum
    }
    if("choices" %in% names(i)) {
      d <- list(
        'species' = selected(i, "species"),
        'date' = c(min(as.Date(selected(i, "date"))), max(as.Date(selected(i, "date")))),
        'bird_id' = selected(i, "bird_id"),
        'feeder_id' = selected(i, "feeder_id"))
    } else {
      d <- list('species' = as.character(unique(i$species)),
                'date' = c(min(i$date), max(i$date)),
                'bird_id' = as.character(unique(i$bird_id)),
                'feeder_id' = as.character(unique(i$feeder_id)))
    }
  }

  d$species <- sort(as.character(d$species))
  d$date <- as.Date(d$date)
  d$bird_id <- sort(as.character(d$bird_id))
  d$feeder_id <- sort(as.character(d$feeder_id))

  return(d)

}

## Get bird image
# database <- unique(finches[, c("bird_id", "species")])
# which = 1:6; size = 300; imgs = NULL; imgs_wiki = NULL
get_image <- function(database, which, size = 300, imgs = NULL, imgs_wiki = NULL){

  if(is.null(imgs)) imgs <- read.csv(system.file("extdata", "shiny-data", "img_index.csv", package = "feedr"), colClasses = "character")
  if(is.null(imgs_wiki)) imgs_wiki <- read.csv(system.file("extdata", "shiny-data", "wiki_index.csv", package = "feedr"), colClasses = "character")

  ## Get the bird_id (which is either ID or index in data base)
  if(is.null(which) | is.null(database)) {  # No ID
    bird <- data.frame(bird_id = NA, species = "unknown", img = NA, citation = NA, author = NA)
  } else if (is.numeric(which)) {  # ID by database location
    bird <- database[which, c("bird_id", "species")]
    bird$bird_id[nchar(as.character(bird$bird_id)) == 0] <- NA
  } else {  # Actual ID
    bird <- database[database$bird_id %in% which, c("bird_id", "species")]
  }

  ## Get image if we have it
  if(any(!is.na(bird$bird_id))){
    bird$id <- 1:nrow(bird) ## Preserve row order

    ## Get img from our pictures
    suppressWarnings({
    bird <- dplyr::left_join(bird, imgs[, c("bird_id", "img", "citation", "author")], by = "bird_id")
    })
  }

  ## Get img of species from wikimedia if we don't have it
  bird$species[!(bird$species %in% imgs_wiki$species)] <- "unknown"
  bird[is.na(bird$img), c("img", "citation", "author")] <- imgs_wiki[match(bird$species[is.na(bird$img)], imgs_wiki$species), c("img", "citation", "author")]

  ## Create css to overlay image
  bird$css <- NA
  bird$css[!is.na(bird$citation)] <- paste0("<div class = \"wiki-watermark\">Wiki: <a href = \"", bird$citation[!is.na(bird$citation)],"\" target=\"blank\">", bird$author[!is.na(bird$citation)], "</a></div>")
  bird$css[is.na(bird$citation)] <- paste0("<div class = \"wiki-watermark\">", bird$author[is.na(bird$citation)], "</div>")

  ## Create div for img
  html <- paste0("<img src='", bird$img, "' height = ", size, ">\n", bird$css)
  return(html)
}


data_limits <- function() {
  ## If you have 0-7 day = 5min interval
  ## If you have > 7 days (1week) = 30min interval
  ## If you have > 14 days (2 weeks) = 1 hr
  ## If you have > 21 days (3 weeks) = 3 hr
  ## If you have > 28 days (1 month) = 6 hours
  ## If you have > 6 weeks (1.5 months) = 12 hours
  ## If you have > 8 weeks (2 months) = 24 hours

  list(i = c("5 min" = 5, "15 min" = 15, "30 min" = 30, "1 hr" = 60, "3 hr" = 60*3, "6 hr" = 60*6, "12 hr" = 60 * 12, "24 hr" = 60 *24),
       n = c(0, 7, 7, 14, 21, 28, 7*6, 7*8) * 60 * 24)
}

interval_selection <- function(data_range, i = NULL){
  if(is.null(i)) i <- data_limits()
  return(last(i$i[i$n <= data_range]))
}

#' @import magrittr
data_tz <- function(data) {
  # Fix time zone to local non-DST
  cols <- which(sapply(data, lubridate::is.POSIXct))
  tz <- tz_offset(attr(data[, cols[1]][[1]], "tzone"), tz_name = TRUE)
  for(i in cols) data[, i] <- lubridate::with_tz(data[, i], tzone = tz)
  return(data)
}
