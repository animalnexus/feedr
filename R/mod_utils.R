
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


