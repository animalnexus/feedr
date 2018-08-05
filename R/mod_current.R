# Launch current

ui_current <- function(diagnostic = FALSE){
  ui_app(name = "map_current")
}



## Get current data
#' @import shiny
#' @import magrittr
mod_UI_map_current <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(12,
             leafletOutput(ns("map_current"), height = 500),
             htmlOutput(ns("current_time")),
             div(actionButton(ns("current_update"), "Update Now", style = "margin: 0 auto"), actionButton(ns("help_update"), "?", class = "help"))
      )
    )
  )
}


#' @import shiny
#' @import magrittr
#' @import RPostgreSQL
mod_map_current <- function(input, output, session) {

  # Setup -------------------------------------------------------------------
  ns <- session$ns

  values <- reactiveValues(
    current_map = NULL,
    current_time = NULL)

  # Database ----------------------------------------------------------------

  # Internet?
  net <- curl::has_internet()

  # No Internet
  if(!net && ns("") == "standalone-") {
    # Placeholder
  }

  # Loggers -------------------------------------------------------------------
  loggers_all <- RCurl::getForm(url_loggers, key = check_db()) %>%
    utils::read.csv(text = ., strip.white = TRUE, colClasses = "character") %>%
    dplyr::rename(logger_id = feeder_id) %>%
    load_format() %>%
    dplyr::filter(site_name == "Kamloops, BC")


  # Icons -------------------------------------------------------------------
  sp_icons <- leaflet::awesomeIconList("Mountain Chickadee" = leaflet::makeAwesomeIcon(icon = "star",
                                                                         marker = "green",
                                                                         iconColor = "white"),
                                       "House Finch" = leaflet::makeAwesomeIcon(icon = "star",
                                                                         marker = "red",
                                                                         iconColor = "white"),
                                       "Dark-eyed Junco" = leaflet::makeAwesomeIcon(icon = "star",
                                                                         marker = "darkpurple",
                                                                         iconColor = "white"))

  get_icon <- function(x) {
    cols <- c("red" = "#D43E2A",
              "green" = "#6FAB25",
              "darkpurple" = "#5A386A")

    span(class="fa-stack fa-md",
         div(icon("circle", class = "fa-stack-2x"), style = paste0("color:", cols[x$markerColor])),
         icon(x$icon, class = "fa-stack-1x fa-inverse"))
  }

  # Help -------------------------------------------------------------------
  observeEvent(input$help_update, {
    showModal(modalDialog(size = "m",
                          title = "Update Current Activity",
                          easyClose = TRUE,
                          tagList("This map reflects recent activity at RFID-enabled bird feeders by tagged birds on campus at Thompson Rivers University.",
                                  tags$ul(style = "margin-top: 10px;",
                                    tags$li("The map will automatically refresh every five minutes, or you can force an update by clicking on the 'Update Now' button."),
                                    tags$li("Click on a 'pin' to get more information about the individual and the visit."),
                                    tags$li("Pin colour reflects species:"),
                                  lapply(1:length(sp_icons), function(x) tagList(get_icon(sp_icons[[x]]), " = ", names(sp_icons)[x]))),
                                  "Note that if individuals only make brief visits to the feeder, total time will remain zero.")
                          ))
  })

  # Circle function ---------------------------------------------------------
  circle <- function(point, data, radius = 0.5){
    n <- seq(0, by = 360/nrow(data), length.out = nrow(data))
    temp <- data.frame(do.call("rbind", lapply(n, function(x) {
      maptools::gcDestination(lon = point$lon,
                              lat = point$lat,
                              bearing = x,
                              dist = radius, dist.units = "km", model = "WGS84")
    })), row.names = NULL)
    names(temp) <- c("lon", "lat")
    circle <- cbind(data, temp)
    return(circle)
  }

  # Current activity ----------------------------------------------------
  current <- reactive({
    validate(need(net, message = "No Internet access. To see Current Activity, an Internet connection is required"))

    invalidateLater(5 * 60 * 1000) # Update again after 5min
    input$current_update

    cat("Getting current activity...\n")
    isolate({
      values$current_time <- Sys.time()
      withProgress(message = "Updating...", {
        qry <- paste("fieldsites.site_id = 'kl'",
                     "ORDER BY time::timestamp DESC LIMIT 100")
        data <- RCurl::getForm(url, where = qry, key = check_db()) %>%
          utils::read.csv(text = ., strip.white = TRUE, colClasses = "character") %>%
          dplyr::rename(animal_id = bird_id, logger_id = feeder_id, species = engl_name) %>%
          load_format(., tz = "UTC", tz_disp = "America/Vancouver") %>%
          dplyr::mutate(logger_id = as.character(logger_id)) %>% # To avoid join warnings
          dplyr::left_join(loggers_all, by = "logger_id") %>%
          visits(.)

        last_24 <- lubridate::with_tz(Sys.time(), tz = "America/Vancouver") - lubridate::hours(24)
        if(nrow(dplyr::filter(data, end >= last_24)) > 0) data <- dplyr::filter(data, end >= last_24)

        data <- data %>%
          dplyr::group_by(animal_id, logger_id, species, age, sex, lat, lon) %>%
          dplyr::summarize(first = min(start),
                           last = max(end),
                           n = length(animal_id),
                           time = round(sum(end - start)/60, 2)) %>%
          dplyr::group_by(logger_id)
        cat("Calc circle...\n")

          data <- data %>%
            dplyr::do(circle(point = unique(.[, c("lat", "lon")]), data = ., radius = 0.01))
        cat("Finished with current activity...\n")
      })
    })
    return(data)
  })

  # Status output ------------------------------------
  output$current_time <- renderText({
    req(current(), nrow(current()) > 0)
    paste0("Most recent update: ", lubridate::with_tz(values$current_time, tz = "America/Vancouver"), " Pacific <br>",
           "Most recent activity: ", max(current()$last), " Pacific <br>",
           "Time window: ", round(as.numeric(difftime(max(current()$last), min(current()$first), units = "hours")), 2), " hour(s) <br>"
           )
  })


  # Map ------------------------------------------------

  # Map of current activity
  output$map_current <- renderLeaflet({
    req(current())
    cat("Initializing map of current activity (", as.character(Sys.time()), ") ...\n")
    isolate({
      d <- loggers_all
      map <- map_leaflet_base(locs = d) %>%
        leaflet::addScaleBar(position = "bottomright") %>%
        leaflet::addAwesomeMarkers(data = current(),
                                   icon = ~sp_icons[species],
                                   popup = ~paste0("<div class = \"current\">",
                                                   get_image(current(), animal_id, "100px"),
                                                   "<strong>Species:</strong> ", species, "<br>",
                                                   "<strong>Animal ID:</strong> ", animal_id, "<br>",
                                                   "<strong>No. visits:</strong> ", n, "<br>",
                                                   "<strong>Total time:</strong> ", time, "min <br>",
                                                   "<strong>Most recent visit:</strong> ", last, "<br>",
                                                   "</div>"),
                                   lng = ~lon, lat = ~lat, group = "Activity") %>%
        addLayersControl(baseGroups = c("Satellite", "Terrain", "Open Street Map", "Black and White"),
                         overlayGroups = c("Loggers", "Activity"),
                         options = layersControlOptions(collapsed = TRUE))

    })
  })

  ## Add activity points
  # Add circle markers for sample sizes
  observeEvent(current(), {
    req(values$current_map)

    cat("Refreshing map of current activity (", as.character(Sys.time()), ") ...\n")
    if(nrow(current()) > 0) {
      leaflet::leafletProxy(ns("map_current")) %>%
        leaflet::clearGroup(group = "Activity") %>%
        leaflet::addAwesomeMarkers(data = current(),
                                   icon = ~sp_icons[species],
                                   popup = ~paste0("<strong>Species:</strong> ", species, "<br>",
                                                   "<strong>Animal ID:</strong> ", animal_id, "<br>",
                                                   "<strong>No. visits:</strong> ", n, "<br>",
                                                   "<strong>Total time:</strong> ", time, "min <br>",
                                                   get_image(current(), animal_id, 100)),
                                   lng = ~lon, lat = ~lat, group = "Activity")

    } else {
      leaflet::leafletProxy("map_data") %>%
        leaflet::clearGroup(group = "Activity")
    }
  })
}
