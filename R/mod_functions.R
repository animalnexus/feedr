#' @import shiny
ui_app <- function(name, ..., diagnostic = FALSE, launch.browser = getOption("shiny.launch.browser", interactive())) {

  addResourcePath("assets", system.file("shiny-examples", "app_files", package = "feedr"))

  app <- shiny::shinyApp(ui = shiny::fluidPage(includeCSS(system.file("shiny-examples", "app_files", "style.css", package = "feedr")),
                                               shinyjs::useShinyjs(),
                                               get(paste0("mod_UI_", name))("standalone"),
                                               mod_UI_stop("stp"),
                                               mod_UI_pause("pause")),
                         server = function(input, output, session) {
                           shiny::callModule(get(paste0("mod_", name)), id = "standalone", ...)
                           shiny::callModule(mod_stop, id = "stp")  # Add Exit Buttons
                           shiny::callModule(mod_pause, id = "pause", diagnostic = diagnostic)  # Add Pause button if 'diagnostic == TRUE'
                         }
  )
  shiny::runApp(app)
}

compare_values <- function(x, y) {
  identical(sort(as.character(unique(x))), sort(as.character(unique(y))))
}

get_counts <- function(c, filter = NULL, summarize_by = NULL) {

  if("date" %in% names(c)) c$date <- as.Date(c$date)

  if(!is.null(filter)){
    if("species" %in% names(filter))   c <- dplyr::filter(c, species %in% filter$species)
    if("date" %in% names(filter))      c <- dplyr::filter(c, date %within% interval(filter$date[1], filter$date[2]))
    if("animal_id" %in% names(filter))   c <- dplyr::filter(c, animal_id %in% filter$animal_id)
    if("logger_id" %in% names(filter)) c <- dplyr::filter(c, logger_id %in% filter$logger_id)
  }

  if(!is.null(summarize_by)) {
    if(nrow(c) > 0) {
      c <- c %>%
        dplyr::group_by_(summarize_by) %>%
        dplyr::summarize(sum = sum(count)) %>%
        tidyr::complete_(summarize_by, fill = list('sum' = 0)) %>%
        dplyr::arrange_(summarize_by) %>%
        dplyr::mutate_(name = lazyeval::interp(~ paste0(var, " (", sum, ")"), var = as.name(summarize_by)),
                       variable = ~summarize_by) %>%
        dplyr::rename_("choices" = summarize_by) %>%
        dplyr::mutate(choices = as.character(choices))
    } else c <- NULL
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
    # if(!is.null(i$plot_data_brush)) {
    #   dates <- c(as.Date(i$plot_data_brush$xmin, lubridate::origin),
    #              as.Date(i$plot_data_brush$xmax, lubridate::origin))
    #   if(dates[1] < min(counts$date)) dates[1] <- min(counts$date)
    #   if(dates[2] > max(counts$date)) dates[2] <- max(counts$date)
    # } else {
      dates <- i$data_date
    #}
    d <- list(
      'species' = i$data_species,
      'date' = dates,
      'animal_id' = i$data_animal_id,
      'logger_id' = i$data_logger_id)
  } else {
    if(is.null(i)) {
      i <- counts_sum
    }
    if("choices" %in% names(i)) {
      d <- list(
        'species' = selected(i, "species"),
        'date' = c(min(as.Date(selected(i, "date"))), max(as.Date(selected(i, "date")))),
        'animal_id' = selected(i, "animal_id"),
        'logger_id' = selected(i, "logger_id"))
    } else {
      d <- list('species' = as.character(unique(i$species)),
                'date' = c(min(i$date), max(i$date)),
                'animal_id' = as.character(unique(i$animal_id)),
                'logger_id' = as.character(unique(i$logger_id)))
    }
  }

  d$species <- sort(as.character(d$species))
  d$date <- as.Date(d$date)
  d$animal_id <- sort(as.character(d$animal_id))
  d$logger_id <- sort(as.character(d$logger_id))

  return(d)

}

## Get animal image
# database <- unique(finches[, c("animal_id", "species")])
# which = 1:6; size = 300; imgs = NULL; imgs_wiki = NULL
get_image <- function(database, which, size = 300, imgs = NULL, imgs_wiki = NULL){

  if(!("species" %in% names(database))) database$species <- "XXXX"

  if(is.null(imgs)) imgs <- utils::read.csv(system.file("extdata", "shiny-data", "img_index.csv", package = "feedr"), colClasses = "character")
  if(is.null(imgs_wiki)) imgs_wiki <- utils::read.csv(system.file("extdata", "shiny-data", "wiki_index.csv", package = "feedr"), colClasses = "character")

  ## Get the animal_id (which is either ID or index in data base)
  if(is.null(which) | is.null(database)) {  # No ID
    animal <- data.frame(animal_id = NA, species = "unknown", img = NA, citation = NA, author = NA)
  } else if (is.numeric(which)) {  # ID by database location
    animal <- database[which, c("animal_id", "species")]
    animal$animal_id[nchar(as.character(animal$animal_id)) == 0] <- NA
  } else {  # Actual ID
    animal <- database[database$animal_id %in% which, c("animal_id", "species")]
  }

  ## Get image if we have it
  if(any(!is.na(animal$animal_id))){
    animal$id <- 1:nrow(animal) ## Preserve row order

    ## Get img from our pictures
    suppressWarnings({
    animal <- dplyr::left_join(animal, imgs[, c("animal_id", "img", "citation", "author")], by = "animal_id")
    })
  }

  animal$species <- as.character(animal$species)

  ## Get img of species from wikimedia if we don't have it
  animal$species[!(animal$species %in% imgs_wiki$species)] <- "unknown"
  animal[is.na(animal$img), c("img", "citation", "author")] <- imgs_wiki[match(animal$species[is.na(animal$img)], imgs_wiki$species), c("img", "citation", "author")]

  ## Create css to overlay image
  animal$css <- NA
  animal$css[!is.na(animal$citation)] <- paste0("<div class = \"wiki-watermark\">Wiki: <a href = \"", animal$citation[!is.na(animal$citation)],"\" target=\"blank\">", animal$author[!is.na(animal$citation)], "</a></div>")
  animal$css[is.na(animal$citation)] <- paste0("<div class = \"wiki-watermark\">", animal$author[is.na(animal$citation)], "</div>")

  ## Create div for img
  html <- paste0("<img src='", animal$img, "' style = 'max-height:", size, "'>\n", animal$css)
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

ready <- function(r){
  shiny::isTruthy(try(r, silent = TRUE))
}
