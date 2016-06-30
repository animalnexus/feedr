shinyServer(function(input, output, session) {


  values <- reactiveValues(
    reset = FALSE,
    data = list(),
    data_initial = values_list(),
    #data_previous = NULL,
    hr_message = "",
    map_hr = FALSE,
    updateUI = FALSE)

  ## Select Data
  source("select_data.R", local = TRUE)
  source("map_data.R", local = TRUE)

  ### Visualizations
 # source("map_animated.R", local = TRUE)
 # source("map_paths.R", local = TRUE)
 # source("map_static.R", local = TRUE)

  ## Load reactive expressions
  #source("reactive.R", local = TRUE)

  ## Load transformation data tables
  source("output_data.R", local = TRUE)

  ## Load inat
  source("inat.R", local = TRUE)
  source("homerange.R", local = TRUE)

  ## Shiny modules
  observe({
    d <- data()
    callModule(mod_map_animate, "anim", raw = d)
  })

  ## Look at birds
  output$img_birds <- renderText({
    req(imgs)
    # Don't actually know what STRH stands for, assuming Sapphire-throated Hummingbird
    if(is.null(input$dt_birds_rows_selected) | is.null(birds_sub())) {
      temp <-imgs$url[imgs$species == "unknown"]
    } else {
      r <- input$dt_birds_rows_selected
      temp <- imgs$url[imgs$species == as.character(birds_sub()$species[r])]
      if(nchar(as.character(temp)) < 1) temp <-imgs$url[imgs$species == "unknown"]
    }
    paste("<img src='",temp,"' height = 300>")
    })


})
