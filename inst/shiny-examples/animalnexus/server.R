cat("Starting server...\n")
library(feedr, lib.loc = "/usr/local/lib/R_exp/site-library/")
library(magrittr)
library(shiny)
library(shinyjs)
library(shinyBS)

cat("Add assets path\n")
addResourcePath("assets", system.file("shiny-examples", "app_files", package = "feedr"))

shinyServer(function(input, output, session) {

  output$package_version <- renderText({
    paste0("Using <a href = 'http://github.com/animalnexus/feedr' target = 'blank'>feedr v", packageVersion("feedr"), "</a>")
  })

  ## Load reactive expressions
  #source("reactive.R", local = TRUE)

  values <- reactiveValues(
    data_reset = TRUE,
    data_import = NULL,
    data_db = NULL)

  cat("Get Database access if we have it\n")
  ## Get Database access if we have it
  if(file.exists("/usr/local/share/feedr/db_full.R")) {
    cat("  Getting credentials\n")
    source("/usr/local/share/feedr/db_full.R")
  } else {
    cat("  No credentials\n")
    db <- NULL
  }

  ## Current activity
  callModule(module = feedr:::mod_map_current, id = "current", db = db)

  ## Pause
  observeEvent(input$pause, browser())

  ## Individuals

  callModule(feedr:::mod_indiv, id = "indiv", r = r)

  ## Database or Import
  data_db <- callModule(feedr:::mod_data_db, "access", db = db)
  data_import <- callModule(feedr:::mod_data_import, "import")

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
    raw <- list(values$data_db, values$data_import)
    raw <- raw[sapply(raw, function(x) !is.null(x$r))]
    if(length(raw) > 1) raw <- raw[which.max(c(raw[[1]]$time(), raw[[2]]$time()))]
    if(length(raw) > 0){
      raw <- raw[[1]]
      values$r <- raw$r()
      values$data_name <- raw$name()
      values$data_time <- raw$time()
    }
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

  ## Transformations
  trans <- callModule(feedr:::mod_trans, "trans", r = reactive({values$r}))

  r <- reactive({trans$r()})
  v <- reactive({trans$v()})

  ## loggers of current data
  loggers <- reactive({
    req(r())
    r() %>%
      dplyr::select(logger_id, site_name, lon, lat) %>%
      unique(.)
  })

  ### Visualizations
  ## Animate Data
  callModule(feedr:::mod_map_animate, "anim", visits = v, verbose = TRUE)

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


  ## Links to panels
  observeEvent(input$link_db, {
    updateTabsetPanel(session, "main", "Database")
  })

  observeEvent(input$link_import, {
    updateTabsetPanel(session, "main", "Import")
  })

  observe({
    req("current-map_current_bounds" %in% names(input))
    session$sendCustomMessage('activeNavs', 'Database')
    session$sendCustomMessage('activeNavs', 'Import')
    session$sendCustomMessage('activeNavs', 'Help')
    shinyjs::show("get-started")
    hide('loading_app')
  })

  observe({
    req(r())
    session$sendCustomMessage('activeNavs', 'Visualizations')
    session$sendCustomMessage('activeNavs', 'Individuals')
    session$sendCustomMessage('activeNavs', 'Transformations')
  })
})
