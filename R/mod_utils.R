
mod_UI_stop <- function(id) {
  ns <- NS(id)
  actionButton(ns("stop"), "X", class = "stop-button")
}

mod_stop <- function(input, output, session) {
  observeEvent(input$stop, {
    stopApp()
  })
}
