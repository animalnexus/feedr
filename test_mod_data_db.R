
if(file.exists("/usr/local/share/feedr/db_full.R")) {
  source("/usr/local/share/feedr/db_full.R")
} else db <- NULL

shiny::runApp(shiny::shinyApp(ui = shiny::fluidPage(shinyjs::useShinyjs(),
                                                    mod_UI_data_db("access")),
                              server = function(input, output, session) {
                                shiny::callModule(mod_data_db, "access", db = db)
                              }
), display.mode = "normal")


