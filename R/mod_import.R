## import module function
import_file <- function() {
  app <- shiny::shinyApp(ui = shiny::fluidPage(shinyjs::useShinyjs(),
                                               mod_UI_data_import("standalone")),
                         server = function(input, output, session) {
                           shiny::callModule(mod_data_import, "standalone", type = "standalone")
                         }
  )
  shiny::runApp(app, display.mode = "normal")
}

## Get current data
#' @import shiny
#' @import magrittr
mod_UI_data_import <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(4,
             fileInput(ns('file1'), 'Choose CSV File',
                       accept=c('text/csv',
                                'text/comma-separated-values,text/plain',
                                '.csv')),
             tags$hr(),
             textInput(ns("tz"), "Data Timezone", value = "America/Vancouver"),
             shinyBS::bsTooltip(ns("tz"), "Timezone that the data was recorded as. Should match timezone names from the Olson database.",
                       "right", options = list(container = "body")),
             radioButtons(ns("format"), "Data Format",
                          choices = c("TRU raw",
                                      "Custom")),
             tags$hr(),
             h4("Format details:"),
             p(textOutput(ns("format"))),
             uiOutput(ns("header")),
             uiOutput(ns("sep")),
             uiOutput(ns("skip")),
             shinyjs::disabled(actionButton(ns("get_data"), "Import")),
             actionButton(ns("pause"), "Pause")
             ),
      column(8,
             h3("File Preview"),
             verbatimTextOutput(ns("preview_file")),
             h3("Data Preview"),
             DT::dataTableOutput(ns('preview'))
             )
    ),
    p(),
    div("Need to import batches of raw data files? Check out the 'load_raw_all' function in the feedr package for R.", style = "color: grey")
  )
}


#' @import shiny
#' @import magrittr
mod_data_import <- function(input, output, session, type = NULL) {

  ns <- session$ns

  vars <- reactiveValues()

  output$format <- renderText({
    req(input$format)
    if(input$format == "TRU raw") t <- "This format is for raw data exported from TRU feeders. It expects the feeder_id in the file name (starting with GPR). It skips the first line of data, and each data row contains the bird_id followed by date and time, separated by whitespace."
    if(input$format == "Custom") t <- "This format expects at least three columns with headers: 'bird_id', 'feeder_id', and 'time'. 'time' should be both date and time in 2016-01-24 14:04:00 format. Separators and rows to skip can be specified below."
  return(t)
  })

  ## Get input figured out:
  output$sep <- renderUI({
    req(input$format == "Custom")
    radioButtons(ns('sep'), 'Separator',
                 choices = c(Comma = ',',
                             Semicolon = ';',
                             Tab = '\t'))
    })

  output$skip <- renderUI({
    req(input$format == "Custom")
    numericInput(ns('skip'), "Rows to skip before data (not including headers)", min = 0, max = 39, value = 0)
    })

  output$preview_file <- renderText({
    validate(need(input$file1$datapath, "No data"))
    l <- readLines(input$file1$datapath, n =  10)
    paste0(l, collapse = "\r\n")
  })


  preview_data <- reactive({
    req(input$file1, input$format, input$tz)
    vars$get_data <- FALSE

    validate(need(input$tz %in% OlsonNames(), "Timezone does not match any from Olson database. See OlsonNames() in R."))

    if(input$format == "TRU raw") {
      suppressWarnings({
        l <- try(load_raw(r_file = unlist(input$file1[c("datapath", "name")]), tz = input$tz), silent = TRUE)
        validate(need(class(l) != "try-error", "Error importing data, try a different format."))
      })
    }

    if(input$format == "Custom") {
      req(!is.null(input$sep), !is.null(input$skip))
      suppressWarnings({
        l <- read.csv(input$file1$datapath, sep = input$sep, skip = input$skip, nrows = 40)
        validate(need(sum(names(l) %in% c("time", "bird_id", "feeder_id")) == 3, "Error importing data, try a different format."))
        validate(need(try(l <- load_format(l, tz = input$tz), silent = TRUE), "Error importing data, try a different format."))
      })
    }

    vars$get_data <- TRUE
    return(l)
  })


  output$preview <- DT::renderDataTable({
    validate(need(preview_data(), "No data"))
    l <- preview_data()
    if(nrow(l) < 10) n <- nrow(l) else n <- 10
    if(any(names(l) == "time")) l$time <- as.character(l$time)
    DT::datatable(l[1:n,],
                  filter = "none",
                  rownames = FALSE, list(searching = FALSE,
                                         paging = FALSE,
                                         info = FALSE))

  }, server = FALSE)

  observe({
    req(input$file1)
    shinyjs::toggleState("get_data", vars$get_data)
  })

  # Select input$file1, reset
  observeEvent(input$file1, {
    vars$data <- NULL
  })

  observeEvent(input$get_data, {
    req(preview_data(), vars$get_data)
    if(input$format == "TRU raw") l <- load_raw(r_file = unlist(input$file1[c("datapath", "name")]), tz = input$tz)
    if(input$format == "Custom") l <- read.csv(input$file1$datapath, sep = input$sep, skip = input$skip) %>% load_format(tz = input$tz)
    if(!is.null(type) && type == "standalone") stopApp(returnValue = l) else vars$data <- l
  })

  observeEvent(input$pause, {
    browser()
  })


  data <- eventReactive(vars$data, {
    req(is.null(type), !is.null(vars$data), input$file1)
    list(data = vars$data, time = Sys.time(), name = input$file1$name)
  })

  return(data)

}
