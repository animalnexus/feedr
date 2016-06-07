## shinyapps from github: devtools::install_github('rstudio/shinyapps')
## library(shinyapps); shinyapps::deployApp("~/Projects/BirdMoves/Scripts/birdMoves")


## devtools::install_github("steffilazerte/feedr")
## devtools::install_github("steffilazerte/envirocan")

## OR:
#library(rsconnect)

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source("startup.R")

shinyServer(function(input, output, session) {


  values <- reactiveValues(
    reset = FALSE,
    data = values_list(),
    data_initial = values_list(),
    data_previous = NULL,
    hr_message = "",
    map_hr = FALSE)

  ## Select Data
  source("select_data.R", local = TRUE)
  source("map_data.R", local = TRUE)

  ### Visualizations
  source("map_animated.R", local = TRUE)
  source("map_paths.R", local = TRUE)
  source("map_static.R", local = TRUE)

  ## Load reactive expressions
  source("reactive.R", local = TRUE)
  
  ## Load transformation data tables
  source("output_data.R", local = TRUE)
  
  ## Load inat
  source("inat.R", local = TRUE)
  source("homerange.R", local = TRUE)

  # Get initial Data
  #con <- dbConnect(drv,host=dbhost,port=dbport,dbname=dbname,user=dbuser,password=dbpass)

  # dates_all <- reactive({
  #   req(values$map_data)
  #     con <- dbConnect(drv,host=dbhost,port=dbport,dbname=dbname,user=dbuser,password=dbpass)
  #     d <- dbGetQuery(con,
  #                statement = paste(
  #                           "SELECT DISTINCT ON (raw.visits.time::date)
  #                           raw.visits.time::date, feeders.site_name",
  #                           "FROM raw.visits, feeders",
  #                           "WHERE raw.visits.feeder_id = feeders.feeder_id")) %>%
  #       rename(date = time) %>%
  #       load.format(tz = "")
  #     dbDisconnect(con)
  #     d
  # })


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
