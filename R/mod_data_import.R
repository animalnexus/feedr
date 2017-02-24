## import module function



#' User-interface for importing files
#'
#' Launches an interactive shiny app for importing data interactively. Also
#' available online at \url{http://animalnexus.ca} or by launching the local
#' animalnexus app through \code{\link{animalnexus}}. Users can also import data by hand using the R base \code{\link[utils]{read.csv}} function coupled with \code{feedr}'s \code{\link{load_format}} function. Alternatively, for raw data collected directly from data loggers, check out the \code{\link{load_raw}} and \code{\link{load_raw_all}} functions.
#'
#' @param diagnostic Logical. Display pause button for debugging
#'
#' @return An imported data frame formatted and ready to be transformed.
#'
#' @seealso \code{\link{load_format}}, \code{\link{load_raw}}, \code{\link{load_raw_all}}
#'
#' @examples
#' \dontrun{
#'   my_data <- ui_import()
#' }
#'
#' @export

ui_import <- function(diagnostic = FALSE) {
  ui_app(name = "data_import", diagnostic = diagnostic)
}

## Get current data
#' @import shiny
#' @import magrittr
mod_UI_data_import <- function(id) {
  ns <- NS(id)
  tz_sys <- check_tz(Sys.timezone())

  tagList(
    fluidRow(
      column(4,
             h4("File Setup", actionButton(ns("help_file"), "?", class = "help")),
             fileInput(ns('file1'), 'Choose CSV File(s)',
                       accept=c('text/csv',
                                'text/comma-separated-values,text/plain',
                                '.csv'),
                       multiple = TRUE),
             radioButtons(ns("format"), "Data Format",
                          choices = c("Preformatted" = "all",
                                      "Logger download" = "logger")),
             uiOutput(ns("UI_id_pattern")),
             uiOutput(ns("UI_details")),
             tags$hr(),
             h4("Options", actionButton(ns("help_options"), "?", class = "help")),
             uiOutput(ns("UI_time")),
             selectInput(ns("tz"), "Data Timezone", choices = OlsonNames(), selected = tz_sys, width = "200px"),
             radioButtons(ns("dst"), "Use Daylight Savings Time?", choices = c("No DST" = FALSE, "Use DST" = TRUE), selected = FALSE),
             uiOutput(ns("UI_sep")),
             uiOutput(ns("UI_skip")),
             shinyjs::disabled(actionButton(ns("get_data"), "Import"))
             ),
      column(8,
             h4("File Preview"),
             verbatimTextOutput(ns("preview_file")),
             textOutput(ns("validations")),
             h4("Data Preview"),
             DT::dataTableOutput(ns('preview'))
             )
    )
  )
}


#' @import shiny
#' @import magrittr
mod_data_import <- function(input, output, session, type = NULL) {

  ns <- session$ns

  options(shiny.maxRequestSize=10*1024^2)

  vars <- reactiveValues(data = NULL,
                         pre_data = NULL,
                         quality = FALSE)

  ## UIs ----------------------------------------------------------------------------
  output$UI_time <- renderUI({
    if(input$format == "all") s <- "ymd HMS" else s <- "mdy HMS"
    selectInput(ns('time'), "Date/Time format",
                choices = c("Year Month Day" = "ymd HMS",
                            "Month Day Year" = "mdy HMS",
                            "Day Month Year" = "dmy HMS"), selected = s,
                width = "150px")
  })

  output$UI_details <- renderUI({
    req(input$format == "logger")
    radioButtons(ns('details'), 'Lat/Lon Information',
                 choices = c("None" = "1",
                             "Logger index" = "file1",
                             "Data files" = "inline2"),
                 selected = "1", inline = TRUE)
  })

  output$UI_id_pattern <- renderUI({
    req(input$format == "logger")
    selectInput(ns("id_pattern"), "Logger id pattern",
                choices = c("As is" = NA,
                            "TRU loggers" = "[GPR]{2,3}[0-9]{1,2}"),
                selected = "", width = "150px")
  })
  #
  # output$UI_id_custom <- renderUI({
  #   req(input$id_pattern == "custom")
  #   textInput(ns("id_custom"),
  #             label = "Custom id regex pattern",
  #             value = "[GPR]{2,3}[0-9]{1,2}")
  # })

  output$UI_sep <- renderUI({
    req(input$format == "all")
    radioButtons(ns('sep'), 'Separator',
                 choices = c(Comma = ',',
                             Semicolon = ';',
                             Tab = '\t'))
    })

  output$UI_skip <- renderUI({
    numericInput(ns('skip'), "Skip", min = 0, max = 39, value = 0, width = "100px")
    })


  # Timezones ---------------------------------------------------------------
  tz <- reactive({
    req(input$tz)
    validate(need(input$tz %in% OlsonNames(), "Timezone does not match any from Olson database. See OlsonNames() in R."))
    tz <- check_tz(input$tz)
    return(tz)
  })


  # Preview File -----------------------------------------------------
  output$preview_file <- renderText({
    validate(need(path(), "No data"))
    d <- readLines(path()[1], n =  10)
    paste0(d, collapse = "\r\n")
  })


  ## File details ----------------------------------------------------
  path <- reactive({
    input$file1$datapath[!grepl("logger_index", input$file1$name)]
  })

  logger <- reactive({
    input$file1$datapath[grepl("logger_index", input$file1$name)]
  })

  ## Preview Data ----------------------------------------------------
  preview_data <- reactive({
    req(input$file1, input$format, tz(), path())
    vars$get_data <- FALSE

    ## Import previews
    if(input$format == "logger") d <- import_logger(path()[1], logger(), tz(), input)
    if(input$format == "all") d <- import_all(path()[1], tz(), input, nrows = 40)

    ## Check validations
    check_data(d)

    vars$get_data <- TRUE
    return(d)
  })

  output$preview <- DT::renderDataTable({
    validate(need(preview_data(), "No data"))
    req(is.null(vars$pre_data))
    d <- preview_data()
    if(nrow(d) < 10) n <- nrow(d) else n <- 10
    if(any(names(d) == "time")) d$time <- as.character(d$time)
    DT::datatable(d[1:n,],
                  filter = "none",
                  rownames = FALSE, list(searching = FALSE,
                                         paging = FALSE,
                                         info = FALSE))

  }, server = FALSE)

  ## Buttons and Reset ----------------------------------------------------
  ## Toggle get data button
  observe({
    req(input$file1)
    shinyjs::toggleState("get_data", vars$get_data)
  })

  ## Reset data selection if input$file1 changes
  observeEvent(input$file1, {
    vars$data <- NULL
    vars$pre_data <- NULL
  })


  ## Get Data ----------------------------------------------------

  ##Import data
  observeEvent(input$get_data, {
    req(preview_data(), vars$get_data)

    withProgress({
      if(input$format == "logger") vars$pre_data <- import_logger(path(), logger(), tz(), input)
      if(input$format == "all") vars$pre_data <- import_all(path(), tz(), input)
    }, message = "Importing...")
  })

  ## Check Data
  output$validations <- renderText({
    req(vars$pre_data)
    check_data(vars$pre_data)
    vars$quality <- TRUE
  })

  ## Export data if clears checks
  observeEvent(vars$quality, {
    req(vars$quality == TRUE, vars$pre_data)
    if(ns("") == "standalone-") {
      message("Data successfully imported")
      stopApp(returnValue = vars$pre_data)
    } else {
      vars$data = vars$pre_data
    }
    vars$pre_data <- NULL
  })

  ## Help dialogues ----------------------------------------------------
  observeEvent(input$help_file, {
    showModal(modalDialog(size = "l",
      title = "File setup",
      easyClose = TRUE,
      tagList(
        if(ns("") != "standalone-") {"After the session any data you have imported will be deleted (we do not keep your data)."},

        h4("Selecting files", style = "font-weight: bold;"),
        tags$ul(
          tags$li("Browse your local hard-drive for a file to import"),
          tags$li("Hold down SHIFT or CTRL to select more than one file")),

        hr(),
        h4("Data Format - Preformatted", style = "font-weight: bold;"),
        "This format is for data that has already been processed to some degree",
        tags$ul(
          tags$li("Expects at least 3 columns", strong("with headers"), ":", code("animal_id"),"*, ", code("logger_id"),"*, and", code("time"), "(may have more columns)"),
          tags$li("Multiple files will be joined together by column name. If a column does not not exist in one file, it will be filled with 'NA' values")),
        div(strong("Example of a", a("data file", href = "assets/preformatted_example.csv", target = "blank")), style = "text-align: center;"),
        pre(
          "animal_id,time,logger_id,species,sex,lon,lat
0620000500,2015-09-11 14:32:22,2100,House Finch,F,-120.3624278,50.66895556
0620000500,2015-09-11 14:32:25,2100,House Finch,F,-120.3624278,50.66895556
0620000500,2015-09-11 14:45:06,2100,House Finch,F,-120.3624278,50.66895556", style = "width:80%; margin: auto;"),

        hr(),

        h4("Data Format - Logger Download", style = "font-weight: bold;"),
        "This format is for raw data exported from RFID loggers.",
        tags$ul(
          tags$li("Each file corresponds to a different logger, but there can be multiple files per logger"),
          tags$li("Logger ids must be provided in the first line of each data file"),
          tags$li("Each data row must contain three columns", strong("without headers"), "corresponding to", code("animal_id, date"), "and", code("time"), "separated by whitespace"),
          tags$li("Multiple files will be joined together after adding a", code("logger_id"), "and, optionally,", code("lat"), "and", code("lon"), "columns."),
          tags$li("Logger ids can be extracted following a pattern specified by", strong("Logger id pattern")),
          tags$ul(
            tags$li("'As is' returns the logger id as is (matching the first line of the file)"),
            tags$li("'TRU loggers' retrun GPR or GP plus two digits (e.g. GPR10DATA becomes GPR10)"))),
        div(strong("Example of a", a("data file", href = "assets/logger_example1.txt", target = "blank")), style = "text-align: center;"),
        pre("GR10DATA
06200004BB 02/06/16 12:39:24
0700EE19CE 02/06/16 12:40:49
0700EE0E42 02/06/16 12:40:52", style = "width:80%; margin: auto;"),

        h4("Providing Lat/Lon for each logger"),
        strong("In a logger_index file"),
        tags$ul(
          tags$li("Choose 'logger_index file' under", strong("Lat/Lon Information")),
          tags$li("The file must be a comma-separated file called 'logger_index' with columns:", code("logger_id"), "*", code("lat"), "and", code("lon")),
          tags$li("Any other columns will be ignored")),
        div(strong("Example of", a("logger_index file", href = "assets/logger_index_example.csv", target = "blank")), style = "text-align: center;"),
        pre("logger_id, lat, lon
GR10, 53.914484, -122.769248
GR11, 53.88821,	-122.8205
GR13, 53.88689,	-122.8208", style = "width:80%; margin: auto;"),
        p("Note: These ids match the file ids only when", strong("Logger id pattern"), "is set to 'TRU loggers'"),

        strong("In the data file"),
        tags$ul(
          tags$li("Choose 'data file' under", strong("Lat/Lon Information")),
          tags$li("In addition to the logger id on the first line, the lat/lon information must be on the second line of each data file")),
        div(strong("Example of a", a("data file", href = "assets/logger_example2.txt", target = "blank"), "with inline lat/lon"), style = "text-align: center;"),
        pre("GR10DATA
53.914484, -122.769248
06200004BB 02/06/16 12:39:24
0700EE19CE 02/06/16 12:40:49
0700EE0E42 02/06/16 12:40:52", style = "width:80%; margin: auto;"),

      hr(),
      "* For compatibility with earlier versions,", code("bird_id"), "and", code("feeder_id"), "are also accepted, but will be renamed to", code("animal_id"), "and", code("logger_id"), "respectively")
    ))
  })

  observeEvent(input$help_options, {
    showModal(modalDialog(size = "m",
    title = "Other Options",
    easyClose = TRUE,
    tagList(
      tags$ul(
        tags$li(strong("Date/Time Format:"), "The order of Day, Month, Year in the data. The exact format doesn't matter"),
        tags$li(strong("Data Timezone:"),"Timezone that the data was recorded in."),
        tags$li(strong("Data DST:"), "Whether or not data includes daylight savings (assumed not)."),
        tags$li(strong("Separator:"), "For pre-formatted files, how are the columns separated?"),
        tags$li(strong("Skip:"), "Extra lines to skip at the top of all data files. For Logger files, this is", em("in addition"), "to the first (or first two) rows which will be automatically skipped.")))
    ))
  })

  # Return ----------------------------------------------------
  return(c(r = reactive({vars$data}),
           time = reactive({if(is.null(vars$data)) NULL else Sys.time()}),
           name = reactive({input$file1$name})))
}


import_logger <- function(path, logger, tz, input) {
  req(input$details)

  d <- try(load_raw_all(r_list = path,
                        tz = tz,
                        dst = as.logical(input$dst),
                        logger_pattern = if(input$id_pattern == "NA") NA else input$id_pattern,
                        time_format = input$time,
                        details = as.numeric(stringr::str_extract(input$details, "[012]")),
                        skip = input$skip),
           silent = TRUE)

  if(class(d) == "try-error") validate(need(!grepl("Expecting one pair of lat/lon", d), "Expecting one pair of lat/lon on second line of the file(s). Check format or change 'details' (Format should be e.g.,  53.91448, -122.76925)."))

  validate(need(class(d) != "try-error", "Error importing data, try a different format or settings."))

  if(input$details == "file1") {
    validate(need(logger, "Expected file 'logger_index' not in files. Re-select files or choose a different location for logger details."))
    ld <- utils::read.csv(logger)
    if("feeder_id" %in% names(ld)) ld <- dplyr::rename(ld, logger_id = feeder_id)
    validate(need("logger_id" %in% names(ld), "'logger_index' does not contain column logger_id (also accepts feeder_id"))
    suppressWarnings(d <- dplyr::left_join(d, ld, by = "logger_id"))
  }
  return(d)
}

import_all <- function(path, tz, input, nrows = -1) {
  req(!is.null(input$sep), !is.null(input$skip))

  d <- try(dplyr::bind_rows(lapply(path, utils::read.csv,
                                   colClasses = "character",
                                   sep = input$sep,
                                   skip = input$skip,
                                   nrows = nrows)) %>%
    load_format(tz = tz, dst = as.logical(input$dst), time_format = input$time), silent = TRUE)

 return(d)
}

check_data <- function(d) {
  validate(need(class(d) != "try-error", "Error importing data, try a different format or settings."))
  validate(need(sum(names(d) %in% c("time", "bird_id", "feeder_id")) == 3 |
                sum(names(d) %in% c("time", "animal_id", "logger_id")) == 3,
                "Error importing data, try a different format."))
  validate(need(all(!is.na(d$logger_id)), "Cannot proceed: Some or all of your logger ids are missing"))
  validate(need(all(!is.na(d$time)), "Cannot proceed: NA times detected, check your time format"))
}



