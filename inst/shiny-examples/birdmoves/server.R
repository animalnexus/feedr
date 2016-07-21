shinyServer(function(input, output, session) {
  values <- reactiveValues(
    current_map = NULL)

  ## Get Database access if we have it
  if(file.exists("/usr/local/share/feedr/db_full.R")) {
    source("/usr/local/share/feedr/db_full.R")
  } else db <- NULL

  imgs_wiki <- read.csv(system.file("extdata", "shiny-data", "species.csv", package = "feedr"), colClasses = c("factor", "character"))
  imgs <- read.csv(system.file("extdata", "shiny-data", "img_index.csv", package = "feedr"))

  ## Select Data
  raw <- callModule(feedr:::mod_db_data, "access", db = db)

  ## Feeders of current data
  feeders <- reactive({
    raw() %>%
      dplyr::select(feeder_id, site_name, lon, lat) %>%
      unique(.)
  })

  ## Birds of current data
  birds <- reactive({
    raw() %>%
      dplyr::select(bird_id, species, age, sex, tagged_on, site_name) %>%
      unique(.)
  })

  ## Current activity
  callModule(feedr:::mod_map_current, "current", db = db)

  ### Visualizations
 # source("map_paths.R", local = TRUE)
 # source("map_static.R", local = TRUE)

  ## Load reactive expressions
  source("reactive.R", local = TRUE)

  ## Load transformation data tables
  source("output_data.R", local = TRUE)

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


  ## Animate Data
  observe({
    r <- raw()
    callModule(mod_map_animate, "anim", v = v())
  })


  ## Look at birds

  output$img_birds <- renderText({
    req(imgs, birds())
    # Don't actually know what STRH stands for, assuming Sapphire-throated Hummingbird
    get_image(birds(), input$dt_birds_rows_selected, 300, imgs, imgs_wiki)
    })


})
