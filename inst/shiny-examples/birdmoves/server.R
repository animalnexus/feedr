shinyServer(function(input, output, session) {

  values <- reactiveValues(
    db_access = db_access,
    map_data = NULL,          # Stores values which displayed on map
    keep = NULL,              # Stores data selected for download
    input = list(),           # Stores selection options
    input_previous = list())  # Stores previous selection options (for comparison)

  ## Select Data
  source("select_data.R", local = TRUE)
  source("map_data.R", local = TRUE)

  ## Current activity
  source("map_current.R", local = TRUE)

  ### Visualizations
 # source("map_animated.R", local = TRUE)
 # source("map_paths.R", local = TRUE)
 # source("map_static.R", local = TRUE)

  ## Load reactive expressions
  source("reactive.R", local = TRUE)

  ## Load transformation data tables
  source("output_data.R", local = TRUE)

  ## Load inat
  #source("inat.R", local = TRUE)
  #source("homerange.R", local = TRUE)

  ## Shiny modules
  observe({
    r <- raw()
    callModule(mod_map_animate, "anim", v = v())
  })

  ## Get bird image

  get_image <- function(database, which, size, imgs){
    if(is.null(which) | is.null(database)) {
      temp <- imgs$url[imgs$species == "unknown"]
    } else if (is.numeric(which)) {
      temp <- imgs$url[imgs$species == as.character(database$species[which])]
      if(nchar(as.character(temp)) < 1) temp <- imgs$url[imgs$species == "unknown"]
    } else {
      temp <- imgs$url[imgs$species == as.character(unique(database$species[database$bird_id == which]))]
      if(nchar(as.character(temp)) < 1) temp <- imgs$url[imgs$species == "unknown"]
    }
    return(paste("<img src='",temp,"' height = ", size, ">"))
  }

  ## Look at birds

  output$img_birds <- renderText({
    req(imgs)
    # Don't actually know what STRH stands for, assuming Sapphire-throated Hummingbird
    get_image(birds(), input$dt_birds_rows_selected, 300, imgs)
    })


})
