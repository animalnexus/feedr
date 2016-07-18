shinyServer(function(input, output, session) {

  values <- reactiveValues(
    db_access = db_access,
    data_map = NULL,          # Stores values which displayed on map
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
    ## Get the bird_id
    if(is.null(which) | is.null(database)) {  # No ID
      bird <- data.frame(bird_id = NA, species = NA)
    } else if (is.numeric(which)) {  # ID by database location
      bird <- database[which, c("bird_id", "species")]
      if(nchar(as.character(bird$bird_id)) < 1) bird$bird_id <- NA
    } else {  # Actual ID
      bird <- database[database$bird_id %in% which, c("bird_id", "species")]
    }
    ## Keep row orders
    bird$id <- 1:nrow(bird)
    suppressWarnings(bird <- left_join(bird, imgs[, c("bird_id", "img")], by = "bird_id"))

    bird$img[!is.na(bird$img)] <- paste0("http://gaia.tru.ca/birdMOVES/img.kl/", bird$img[!is.na(bird$img)], ".jpg")

    ## No specific image, but we know the species:
    bird$species <- as.character(bird$species)
    bird$species[is.na(bird$species)] <- "unknown"
    suppressWarnings(bird <- left_join(bird, imgs_wiki, by = "species"))

    bird$css <- paste0("<div class = \"wiki-watermark\">Wiki: <a href = \"",bird$page,"\" target=\"blank\">",bird$author,"</a></div>")
    bird$css[!is.na(bird$img)] <- NA
    bird$img[is.na(bird$img)] <- bird$url[is.na(bird$img)]

    html <- paste("<div class = \"bird-img\"><img src='", bird$img, "' height = ", size, ">")
    html[!is.na(bird$css)] <- paste0(html, "\n", bird$css[!is.na(bird$css)], "</div>")
    html[is.na(bird$css)] <- paste0(html, "\n", "/div>")
    return(html)
  }

  ## Look at birds

  output$img_birds <- renderText({
    req(imgs)
    # Don't actually know what STRH stands for, assuming Sapphire-throated Hummingbird
    get_image(birds(), input$dt_birds_rows_selected, 300, imgs)
    })


})
