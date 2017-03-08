cat("Starting server...\n")
library(feedr, lib.loc = "/usr/local/lib/R_exp/site-library/")
library(magrittr)
library(shiny)
library(shinyjs)
library(shinyBS)

cat("Add assets path\n")
addResourcePath("assets", system.file("shiny-examples", "app_files", package = "feedr"))

shinyServer(function(input, output, session) {


  # Check Internet connection -----------------------------------------------
  if(!curl::has_internet()) {
    showModal(modalDialog(
      title = "No Internet connection",
      "Animalnexus requires an internet connection for downloading database data, creating animations, and looking at maps. Many features are limited without an internet connection.",
      easyClose = TRUE
    ))
  }

  # Display package version -------------------------------------------------
  output$package_version <- renderText({
    paste0("Using <a href = 'http://github.com/animalnexus/feedr' target = 'blank'>feedr v", packageVersion("feedr"), "</a>")
  })


  # Values ------------------------------------------------------------------
  values <- reactiveValues(
    data_reset = TRUE,
    data_import = NULL,
    data_db = NULL)

  # Data base ---------------------------------------------------------------
  cat("Get Database access if we have it\n")
  db <- check_db()

  # Modules (Tabs) --------------------------------------------------------

  # Current Activity
  callModule(module = feedr:::mod_map_current, id = "current")

  # Database and Import
  data_db <- callModule(feedr:::mod_data_db, "access")
  data_import <- callModule(feedr:::mod_data_import, "import")

  # Visualizations
  callModule(feedr:::mod_map_animate, "anim", visits = v, verbose = TRUE)

  # Individuals
  callModule(feedr:::mod_indiv, id = "indiv", r = r)

  # Transformations
  trans <- callModule(feedr:::mod_trans, "trans", r = reactive({values$r}))


  # Getting data ------------------------------------------------------------
  observe({
    req(data_db$r())
    values$data_db <- data_db
  })

  observe({
    req(data_import$r())
    values$data_import <- data_import
  })

  # Which data?
  observe({
    req(!is.null(values$data_db) || !is.null(values$data_import))
    raw <- list(values$data_db, values$data_import)
    raw <- raw[sapply(raw, function(x) !is.null(x$r))]
    if(length(raw) > 1) raw <- raw[which.max(c(raw[[1]]$time(), raw[[2]]$time()))]
    if(length(raw) > 0){
      raw <- raw[[1]]
      values$r <- raw$r()
      values$data_name <- raw$name()[1]
      values$data_time <- raw$time()
    }
    values$data_db <- NULL
    values$data_import <- NULL
  })

  output$data_info <- renderText({
    cat("Update active dataset\n")
    t <- "Active dataset: "

    if(is.null(values$r)) {
      t <- paste0(t, "None")
    } else {
      t <- paste0(t, values$data_name, ". Loaded at ", values$data_time)
    }
    return(t)
  })


  # Transformed data --------------------------------------------------------
  r <- reactive({trans$r()})
  v <- reactive({trans$v()})

  # Loggers of current data
  loggers <- reactive({
    req(r())
    r() %>%
      dplyr::select(logger_id, site_name, lon, lat) %>%
      unique(.)
  })


  ## Add weather data
  #Get weather data
  # if(input$data_weather == "Yes" & any(unique(data$site_name) == "Kamloops, BC")){
  #   withProgress(message = "Adding Weather Data...",
  #                w <- weather(station_id = 51423, start = min(as.Date(data$time)), end = max(as.Date(data$time)), timeframe = "hour") %>%
  #                  dplyr::mutate(hour = format(time, "%Y-%m-%d %H"))
  #   )
  #   data <- data %>%
  #     dplyr::mutate(hour = format(time, "%Y-%m-%d %H")) %>%
  #     dplyr::left_join(w[, c("hour", "temp", "temp_dew", "rel_hum", "hmdx", "pressure", "visib", "wind_chill", "wind_dir", "wind_spd")], by = "hour")
  # }


  # Links to specific tabs --------------------------------------------------

  observeEvent(input$link_db, {
    updateTabsetPanel(session, "main", "Database")
  })

  observeEvent(input$link_import, {
    updateTabsetPanel(session, "main", "Import")
  })

  # Prevent tabs from loading -----------------------------------------------
  # Wait until current/home tab finished
  observe({
    req("current-map_current_bounds" %in% names(input))
    session$sendCustomMessage('activeNavs', 'Database')
    session$sendCustomMessage('activeNavs', 'Import')
    session$sendCustomMessage('activeNavs', 'Help')
    shinyjs::show("get-started")
    hide('loading_app')
  })

  # If no internet, show right away
  observe({
    req(!curl::has_internet())
    session$sendCustomMessage('activeNavs', 'Database')
    session$sendCustomMessage('activeNavs', 'Import')
    session$sendCustomMessage('activeNavs', 'Help')
    shinyjs::show("get-started")
    hide('loading_app')
  })

  # Wait until data loaded before loading the rest
  observe({
    req(r())
    session$sendCustomMessage('activeNavs', 'Visualizations')
    session$sendCustomMessage('activeNavs', 'Individuals')
    session$sendCustomMessage('activeNavs', 'Transformations')
  })
})
