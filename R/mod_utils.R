#' @import shiny
ui_app <- function(name, ..., launch.browser = getOption("shiny.launch.browser", interactive())) {

  addResourcePath("assets", system.file("shiny-examples", "app_files", package = "feedr"))

  app <- shiny::shinyApp(ui = shiny::fluidPage(includeCSS(system.file("shiny-examples", "app_files", "style.css", package = "feedr")),
                                               shinyjs::useShinyjs(),
                                               get(paste0("mod_UI_", name))("standalone"),
                                               mod_UI_stop("stp")),
                         server = function(input, output, session) {
                           shiny::callModule(get(paste0("mod_", name)), id = "standalone", ...)
                           shiny::callModule(mod_stop, id = "stp")  # Add Exit Buttons
                           session$onSessionEnded(stopApp)
                         }
  )
  shiny::runApp(app)
}

mod_UI_nav <- function(id, ...) {
  ns <- NS(id)
  addResourcePath("assets", system.file("shiny-examples", "app_files", package = "feedr"))
  navbarPage(title = a(href = "http://animalnexus.ca", HTML("animal<strong>nexus</strong>")),
             id = "main",
             position = "fixed-top",
             collapsible = TRUE,
             windowTitle = "animalnexus",
             header = includeCSS(system.file("shiny-examples", "app_files", "style.css", package = "feedr")),
             ...)
}

mod_UI_stop <- function(id) {
  ns <- NS(id)
  actionButton(ns("stop"), "X", class = "stop-button")
}

mod_stop <- function(input, output, session) {
  message("\nTo close the app:
  - Hit 'Esc', or
  - Click on the 'X' in the upper right-hand corner of the app, or
  - In Rstudio, click on the 'Stop' button above the console")
  observeEvent(input$stop, {
    stopApp()
  })
}

mod_UI_pause <- function(id) {
  ns <- NS(id)
  shinyjs::hidden(actionButton(ns("pause"), "Pause"))
}

mod_pause <- function(input, output, session, diagnostic) {
  shinyjs::toggle("pause", condition = diagnostic)
  observeEvent(input$pause, {
    browser()
  })
}


