shinyServer(function(input, output, session) {
  library(feedr)

  ## Load reactive expressions
  source("reactive.R", local = TRUE)

  values <- reactiveValues(
    current_map = NULL)

  ## Get Database access if we have it
  if(file.exists("/usr/local/share/feedr/db_full.R")) {
    source("/usr/local/share/feedr/db_full.R")
  } else db <- NULL

  imgs_wiki <- read.csv(system.file("extdata", "shiny-data", "species.csv", package = "feedr"), colClasses = c("factor", "character"))
  imgs <- read.csv(system.file("extdata", "shiny-data", "img_index.csv", package = "feedr"))

  ## Pause
  observeEvent(input$pause, browser())

  ## Database or Import
  data_db <- callModule(feedr:::mod_data_db, "access", db = db)
  data_import <- callModule(feedr:::mod_data_import, "import")

  observeEvent(data_db(), {
    values$data_db <- data_db()
  })

  observeEvent(data_import(), {
    values$data_import <- data_import()
  })

  data <- reactive({
    if(!is.null(values$data_db) & !is.null(values$data_import)) {
      if(values$data_db$time > values$data_import$time) {
        data <- values$data_db
        } else {
          data <- values$data_import
          data$data$dataaccess <- 0  ## Allow users to download their own data
        }
    } else if (!is.null(values$data_db) & is.null(values$data_import)) {
      data <- values$data_db
    } else if (is.null(values$data_db) & !is.null(values$data_import)) {
      data <- values$data_import
      data$data$dataaccess <- 0  ## Allow users to download their own data
    } else data <- NULL
    return(data)
  })

  raw <- reactive({
    req(data())
    data()$data
  })

  data_info <- reactive({
    t <- "Active dataset: "
    if(is.null(data())) {
      t <- paste0(t, "None")
    } else {
      t <- paste0(t, data()$name, ". Loaded at ", data()$time)
    }
    return(t)
  })

  output$data_info <- renderText({
    req(data_info())
    data_info()
  })

  output$package_version <- renderText({
   paste0("Using <a href = 'http://github.com/steffilazerte/feedr' target = 'blank'>feedr v", packageVersion("feedr"), "</a>")
  })


  ## Feeders of current data
  feeders <- reactive({
    raw() %>%
      dplyr::select(feeder_id, site_name, lon, lat) %>%
      unique(.)
  })

  ## Birds of current data
  birds <- reactive({
    cols <- names(raw())[names(raw()) %in% c("bird_id", "species", "age", "sex", "tagged_on", "site_name")]
    raw() %>%
      dplyr::select(one_of(cols)) %>%
      unique(.)
  })

  ## Current activity
  callModule(module = feedr:::mod_map_current, id = "current", db = db)

  ### Visualizations
  ## Animate Data
  observe({
    callModule(mod_map_animate, "anim", v = v())
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


  ## Look at birds
  output$img_birds <- renderText({
    req(imgs, birds())
    # Don't actually know what STRH stands for, assuming Sapphire-throated Hummingbird
    feedr:::get_image(birds(), input$dt_birds_rows_selected, 300, imgs, imgs_wiki)
    })


  ## Load transformation data tables
  source("output_data.R", local = TRUE)


})
