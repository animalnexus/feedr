# Launch current

# mod_current <- function() {
#   if(file.exists("/usr/local/share/feedr/db_full.R")) {
#     source("/usr/local/share/feedr/db_full.R")
#   } else db <- NULL
#
#   app <- shiny::shinyApp(ui = shiny::fluidPage(mod_UI_map_current("standalone")),
#                          server = function(input, output, session) {
#                            shiny::callModule(mod_map_current, "standalone", db = db)
#                          }
#   )
#   shiny::runApp(app, launch.browser = TRUE)
# }

ui_current <- function(){
  if(file.exists("/usr/local/share/feedr/db_full.R")) {
    source("/usr/local/share/feedr/db_full.R")
  } else db <- NULL
  ui_app(name = "map_current", db = db)
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
             div(actionButton(ns("current_update"), "Update Now", style = "margin: 0 auto"), actionButton(ns("help_update"), "?", class = "help"))#,
             #htmlOutput(ns("summary_current")),
             #actionButton(ns("pause"), "Pause")
      )
    )
  )
}


#' @import shiny
#' @import magrittr
#' @import RPostgreSQL
mod_map_current <- function(input, output, session, db) {

  ns <- session$ns

  sp_icons <- leaflet::awesomeIconList("MOCH" = leaflet::makeAwesomeIcon(icon = "star",
                                                                         marker = "green",
                                                                         iconColor = "white"),
                                       "HOFI" = leaflet::makeAwesomeIcon(icon = "star",
                                                                         marker = "red",
                                                                         iconColor = "white"),
                                       "DEJU" = leaflet::makeAwesomeIcon(icon = "star",
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

  observeEvent(input$help_update, {
    showModal(modalDialog(size = "m",
                          title = "Update Current Activity",
                          easyClose = TRUE,
                          tagList("This map reflects recent activity at RFID-enabled bird feeders by tagged birds on campus at Thompson Rivers University.",
                                  tags$ul(style = "margin-top: 10px;",
                                    tags$li("The map will automatically refresh every five minutes, or you can force an update by clicking on the 'Update Now' button."),
                                    tags$li("Click on a 'pin' to get more information about the individual and the visit."),
                                    tags$li("Pin colour reflects species:"),
                                  lapply(1:length(sp_icons), function(x) tagList(get_icon(sp_icons[[x]]), " = ", names(sp_icons)[x]))))
                          ))
  })

  if(!is.null(db) && !curl::has_internet()) db <- NULL

  circle <- function(point, data, radius = 0.5){
    n <- seq(0, by = 360/nrow(data), length.out = nrow(data))
    temp <- data.frame(do.call("rbind", lapply(n, FUN = function(x) {
      maptools::gcDestination(lon = point$lon,
                              lat = point$lat,
                              bearing = x,
                              dist = radius, dist.units = "km", model = "WGS84")
    })), row.names = NULL)
    names(temp) <- c("lon", "lat")
    circle <- cbind(data, temp)
    return(circle)
  }

  values <- reactiveValues(
    current_map = NULL,
    current_time = NULL)

  observeEvent(input$pause, {
    browser()
  })

  if(!is.null(db)){
    con <- dbConnect(dbDriver("PostgreSQL"), host = db$host, port = db$port, dbname = db$name, user = db$user, password = db$pass)
    suppressWarnings({
      loggers_all <- dbGetQuery(con,
                                statement = paste("SELECT feeders.feeder_id, feeders.site_name, feeders.loc, fieldsites.dataaccess",
                                                  "FROM feeders, fieldsites",
                                                  "WHERE (fieldsites.site_name = feeders.site_name)")) %>%
        load_format(tz = "America/Vancouver") %>%
        dplyr::mutate(site_name = factor(site_name))
    })
    dbDisconnect(con)
  }

  ## Get current Activity
  current <- reactive({
    validate(need(!is.null(db), message = "No Database access. To see Current Activity, check out animalnexus.ca"))
    #req(!is.null(db))

    invalidateLater(5 * 60 * 1000)
    input$current_update

    isolate({
      con <- dbConnect(dbDriver("PostgreSQL"), host = db$host, port = db$port, dbname = db$name, user = db$user, password = db$pass)

      values$current_time <- Sys.time()
      withProgress(message = "Updating...", {
        suppressWarnings({
          data <- dbGetQuery(con,
                             statement = paste("SELECT raw.visits.bird_id, raw.visits.feeder_id, raw.visits.time, feeders.site_name, feeders.loc, birds.species, birds.age, birds.sex",
                                                "FROM raw.visits, feeders, birds",
                                                "WHERE (raw.visits.feeder_id = feeders.feeder_id)",
                                                "AND (birds.bird_id = raw.visits.bird_id)",
                                                "AND birds.species NOT IN ( 'XXXX' )",
                                                "AND feeders.site_name IN ( 'Kamloops, BC' )",
                                                "AND raw.visits.time::timestamp > ( CURRENT_TIMESTAMP::timestamp - INTERVAL '24 hours' )"))

          if(nrow(data) == 0) data <- dbGetQuery(con,
                                                     statement = paste("SELECT raw.visits.bird_id, raw.visits.feeder_id, raw.visits.time, feeders.site_name, feeders.loc, birds.species, birds.age, birds.sex ",
                                                                       "FROM raw.visits, feeders, birds ",
                                                                       "WHERE (raw.visits.feeder_id = feeders.feeder_id) ",
                                                                       "AND (birds.bird_id = raw.visits.bird_id) ",
                                                                       "AND birds.species NOT IN ( 'XXXX' )",
                                                                       "AND feeders.site_name IN ( 'Kamloops, BC' ) ",
                                                                       "ORDER BY raw.visits.time::timestamp DESC LIMIT 100"))
          })
        dbDisconnect(con)

        if(nrow(data) > 0) {
          data <- data %>%
            load_format(., tz = "America/Vancouver", tz_disp = "America/Vancouver") %>%
            visits(.) %>%
            dplyr::group_by(animal_id, logger_id, species, age, sex, lon, lat) %>%
            dplyr::summarize(most_recent = max(end),
                             n = length(animal_id),
                             time = round(sum(end - start)/60, 2)) %>%
            dplyr::group_by(logger_id) %>%
            dplyr::do(circle(point = unique(.[, c("lat", "lon")]), data = ., radius = 0.01))
        } else data <- NULL
      })
    })
    data
  })

  output$current_time <- renderText(paste0("Most recent activity: ", max(current()$most_recent), " Pacific <br>",
                                           "Most recent update: ", lubridate::with_tz(values$current_time, tz = "America/Vancouver"), " Pacific"))

  ## Map of current activity
  output$map_current <- renderLeaflet({
    req(current())
    cat("Initializing map of current activity (", as.character(Sys.time()), ") ...\n")
    isolate({
      d <- loggers_all %>% dplyr::filter(site_name == "Kamloops, BC")
      map <- map_leaflet_base(locs = d) %>%
        leaflet::addScaleBar(position = "bottomright") %>%
        leaflet::addAwesomeMarkers(data = current(),
                                   icon = ~sp_icons[species],
                                   popup = ~paste0("<div class = \"current\">",
                                                   get_image(current(), animal_id, "100px"),
                                                   "<strong>Species:</strong> ", species, "<br>",
                                                   "<strong>Animal ID:</strong> ", animal_id, "<br>",
                                                   "<strong>No. RFID reads:</strong> ", n, "<br>",
                                                   "<strong>Total time:</strong> ", time, "min <br>",
                                                   "<strong>Most recent visit:</strong> ", most_recent, "<br>",
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
                                                   "<strong>No. RFID reads:</strong> ", n, "<br>",
                                                   "<strong>Total time:</strong> ", time, "min <br>",
                                                   get_image(current(), animal_id, 100)),
                                   lng = ~lon, lat = ~lat, group = "Activity")

    } else {
      leaflet::leafletProxy("map_data") %>%
        leaflet::clearGroup(group = "Activity")
    }
  })


  output$summary_current <- renderText({
    req(current())
    paste0("<strong>Time:</strong> ", Sys.time(), "<br>",
           "<strong>Interval:</strong> 7 minutes", "<br>",
           "<strong>No. animals:</strong> ", length(unique(current()$animal_id)), "<br>",
           "<strong>No. loggers:</strong> ", length(unique(current()$logger_id)), "<br>",
           "<strong>Total time present:</strong> ", sum(current()$time), " minutes", "<br>"
    )
  })

}
