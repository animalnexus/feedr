## Get current data
#' @import shiny
#' @import magrittr
mod_UI_map_current <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(2,
             h3("Summary"),
             actionButton(ns("current_update"), "Update Now"),
             htmlOutput(ns("summary_current"))),
      column(10,
             leafletOutput(ns("map_current"), height = 600),
             htmlOutput(ns("current_time"))
      )
    )
  )
}


#' @import shiny
#' @import magrittr
mod_map_current <- function(input, output, session, db) {

  ns <- session$ns

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

  sp_icons <- leaflet::awesomeIconList("Mountain Chickadee" = leaflet::makeAwesomeIcon(icon = "star",
                                                                                       marker = "green",
                                                                                       iconColor = "white"),
                                       "House Finch" = leaflet::makeAwesomeIcon(icon = "star",
                                                                                marker = "red",
                                                                                iconColor = "white"),
                                       "Dark-eyed Junco" = leaflet::makeAwesomeIcon(icon = "star",
                                                                                    marker = "darkpurple",
                                                                                    iconColor = "white"))


  imgs_wiki <- read.csv(system.file("extdata", "shiny-data", "species.csv", package = "feedr"), colClasses = c("factor", "character"))
  imgs <- read.csv(system.file("extdata", "shiny-data", "img_index.csv", package = "feedr"))

  values <- reactiveValues(
    current_map = NULL,
    current_time = NULL)

  if(!is.null(db)){
    con <- dbConnect(dbDriver("PostgreSQL"), host = db$host, port = db$port, dbname = db$name, user = db$user, password = db$pass)
    suppressWarnings({
      feeders_all <- dbGetQuery(con,
                                statement = paste("SELECT feeders.feeder_id, feeders.site_name, feeders.loc, fieldsites.dataaccess",
                                                  "FROM feeders, fieldsites",
                                                  "WHERE (fieldsites.site_name = feeders.site_name)")) %>%
        load_format(tz = "") %>%
        dplyr::mutate(site_name = factor(site_name))
    })
    dbDisconnect(con)
  }

  ## Get current Activity
  current <- reactive({
    req(!is.null(db))

    invalidateLater(5 * 60 * 1000)
    input$current_update

    isolate({
      con <- dbConnect(dbDriver("PostgreSQL"), host = db$host, port = db$port, dbname = db$name, user = db$user, password = db$pass)

      values$current_time <- Sys.time()
      withProgress(message = "Updating...", {
        suppressWarnings({
          data <- dbGetQuery(con,
                             statement = paste0("SELECT raw.visits.bird_id, raw.visits.feeder_id, raw.visits.time, feeders.site_name, feeders.loc, birds.species, birds.age, birds.sex ",
                                                "FROM raw.visits, feeders, birds ",
                                                "WHERE (raw.visits.feeder_id = feeders.feeder_id) ",
                                                "AND (birds.bird_id = raw.visits.bird_id) ",
                                                "AND feeders.site_name IN ( 'Kamloops, BC' ) ",
                                                #"AND raw.visits.time::timestamp > (CURRENT_TIMESTAMP - INTERVAL '7 minutes') ",
                                                "AND raw.visits.time::timestamp > ('2016-03-25 15:23:30'::timestamp - INTERVAL '7 minutes') "))
        })
        dbDisconnect(con)

        if(nrow(data) > 0) {
          data <- data %>%
            load_format(., tz = "UTC", tz_disp = "America/Vancouver") %>%
            visits(.) %>%
            dplyr::group_by(bird_id, feeder_id, species, age, sex, lon, lat) %>%
            dplyr::summarize(n = length(bird_id),
                             time = round(sum(end - start)/60, 2)) %>%
            dplyr::group_by(feeder_id) %>%
            dplyr::do(circle(point = unique(.[, c("lat", "lon")]), data = ., radius = 0.01))
        } else data <- NULL
      })
    })
    data
  })

  output$current_time <- renderText(as.character(values$current_time))

  ## Map of current activity
  output$map_current <- renderLeaflet({
    req(current())
    cat("Initializing map of current activity (", as.character(Sys.time()), ") ...\n")
    isolate({
      map <- map_leaflet_base(locs = feeders_all %>% dplyr::filter(site_name == "Kamloops, BC")) %>%
        leaflet::addScaleBar(position = "bottomright") %>%
        leaflet::addAwesomeMarkers(data = current(),
                                   icon = ~sp_icons[species],
                                   popup = ~paste0("<strong>Species:</strong> ", species, "<br>",
                                                   "<strong>Bird ID:</strong> ", bird_id, "<br>",
                                                   "<strong>No. RFID reads:</strong> ", n, "<br>",
                                                   "<strong>Total time:</strong> ", time, "min <br>",
                                                   feedr:::get_image(current(), bird_id, 100, imgs, imgs_wiki)),
                                   lng = ~lon, lat = ~lat, group = "Activity")

    })
  })

  ## Add activity points
  # Add circle markers for sample sizes
  observeEvent(current(), {
    req(imgs, imgs_wiki, values$current_map)

    cat("Refreshing map of current activity (", as.character(Sys.time()), ") ...\n")
    if(nrow(current()) > 0) {
      leaflet::leafletProxy(ns("map_current")) %>%
        leaflet::clearGroup(group = "Activity") %>%
        leaflet::addAwesomeMarkers(data = current(),
                                   icon = ~sp_icons[species],
                                   popup = ~paste0("<strong>Species:</strong> ", species, "<br>",
                                                   "<strong>Bird ID:</strong> ", bird_id, "<br>",
                                                   "<strong>No. RFID reads:</strong> ", n, "<br>",
                                                   "<strong>Total time:</strong> ", time, "min <br>",
                                                   feedr:::get_image(current(), bird_id, 100, imgs, imgs_wiki)),
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
           "<strong>No. birds:</strong> ", length(unique(current()$bird_id)), "<br>",
           "<strong>No. feeders:</strong> ", length(unique(current()$feeder_id)), "<br>",
           "<strong>Total feeding time:</strong> ", sum(current()$time), " minutes", "<br>"
    )
  })

}
