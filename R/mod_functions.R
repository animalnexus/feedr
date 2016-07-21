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
get_image <- function(database, which, size, imgs, imgs_wiki){
  ## Get the bird_id
  if(is.null(which) | is.null(database)) {  # No ID
    bird <- data.frame(bird_id = NA, species = NA, img = NA)
  } else if (is.numeric(which)) {  # ID by database location
    bird <- database[which, c("bird_id", "species")]
    if(nchar(as.character(bird$bird_id)) < 1) bird$bird_id <- NA
  } else {  # Actual ID
    bird <- database[database$bird_id %in% which, c("bird_id", "species")]
  }
  ## Keep row orders
  if(any(!is.na(bird$bird_id))){
    bird$id <- 1:nrow(bird)
    suppressWarnings(bird <- dplyr::left_join(bird, imgs[, c("bird_id", "img")], by = "bird_id"))
    bird$img[!is.na(bird$img)] <- paste0("http://gaia.tru.ca/birdMOVES/img.kl/", bird$img[!is.na(bird$img)], ".jpg")

    ## No specific image, but we know the species:
    bird$species <- as.character(bird$species)
  }

  bird$species[is.na(bird$species)] <- "unknown"
  suppressWarnings(bird <- dplyr::left_join(bird, imgs_wiki, by = "species"))

  bird$css <- paste0("<div class = \"wiki-watermark\">Wiki: <a href = \"",bird$page,"\" target=\"blank\">",bird$author,"</a></div>")
  bird$css[!is.na(bird$img)] <- NA
  bird$img[is.na(bird$img)] <- bird$url[is.na(bird$img)]

  html <- paste("<div class = \"bird-img\"><img src='", bird$img, "' height = ", size, ">")
  html[!is.na(bird$css)] <- paste0(html, "\n", bird$css[!is.na(bird$css)], "</div>")
  html[is.na(bird$css)] <- paste0(html, "\n", "/div>")
  return(html)
}

