# Launch settings for testing
ui_settings <- function(verbose = FALSE){
  ui_app(name = "settings", verbose = verbose)
}

settings_preamble <- function(){
  settings_functions <- c("visits", "move", "presence", "disp", "dom", "activity", "daily")

  return(man %>%
           dplyr::filter(f %in% settings_functions,
                         !is.na(value),
                         !(arg %in% c("missing", "pass"))) %>%
           dplyr::mutate(f = factor(f, levels = settings_functions)) %>%
           dplyr::arrange(f))
}


## Settings
#' @import shiny
#' @import magrittr
mod_UI_settings <- function(id) {
  ns <- NS(id)

  manual <- settings_preamble()

  tagList(
    column(3,
           p(shinyjs::disabled(actionButton(ns("settings_get"), "Set Options"))),
           p(downloadButton(ns("settings_save"), "Save Settings to disk")),
           p(actionButton(ns("import_reveal"), "Import Settings from disk"),
             shinyjs::hidden(fileInput(ns('import_settings'), 'Choose Settings File',
                                       accept=c('text/csv',
                                                'text/comma-separated-values,text/plain',
                                                '.csv'),
                                       multiple = FALSE))),
           p(actionButton(ns("reset"), "Reset to default", class = "btn-danger"))
    ),
    column(9,
           h3("Settings for Transformations", actionButton(ns("help_settings"), "?", class = "help")),
           lapply(1:length(unique(manual$f)), function(x) {
             x <- manual[manual$f == unique(manual$f)[x], ]
             tagList(h3(x$title[1], actionButton(ns(paste0("help_", x$f[1])), "?", class = "help")),
                     lapply(1:nrow(x), function(y) {
                       y <- x[y, ]
                       uiOutput(ns(paste0("UI_set_", y$f, "_", y$arg)))
                     }),
                     tags$hr()
             )
           })
    )
  )
}


#' @import shiny
#' @import magrittr
mod_settings <- function(input, output, session, verbose = FALSE) {

  # Setup -------------------------------------------------------------------
  ns <- session$ns

  # Variables ---------------------------------------------------------------
  manual <- settings_preamble()

  values <- reactiveValues(
    settings = NULL,
    settings_default = dplyr::select(manual, id, value, class)
  )

  # Start with defaults
   observe({
     req(is.null(values$settings), settings())
     if(verbose) cat("Settings - Set starting values\n")
     values$settings <- settings()
   })

  # Settings ----------------------------------------------------------------

  # Function to create or update UIs
  settings_ui <- function(df, update = FALSE) {
    lapply(1:nrow(df), FUN = function(row) {
      x <- df[row, ]
      if(update){
        if(x$class == "Numeric") updateNumericInput(session = session,
                                                    inputId = x$id,
                                                    value = as.numeric(x$value))
        if(x$class == "Logical") updateRadioButtons(session = session,
                                                    inputId = x$id,
                                                    selected = as.logical(x$value))
      } else {
        if(x$class == "Numeric") ui <- numericInput(label = tagList(x$lab, code(x$arg)),
                                                    inputId = ns(x$id),
                                                    min = 1,
                                                    value = as.numeric(x$value))
        if(x$class == "Logical") ui <- radioButtons(label = tagList(x$lab, code(x$arg)),
                                                    inputId = ns(x$id),
                                                    choices = c("Yes" = TRUE, "No" = FALSE),
                                                    selected = as.logical(x$value),
                                                    inline = TRUE)
        return(output[[paste0("UI_", x$id)]] <- renderUI({ui}))
      }
    })
  }

  # Create UIs
   settings_ui(manual)

  # Render UIs even when hidden (or when Tab isn't in focus)
   lapply(manual$id, function(x) outputOptions(output, paste0("UI_", x), suspendWhenHidden = FALSE))

  # Reset ---------------------------------------------------
  observeEvent(input$reset, {
    req(stringr::str_detect(names(input), "set_"))
    settings_ui(values$settings_default, update = TRUE)
  })

  # Import ---------------------------------------------------
  observeEvent(input$import_reveal, {
    shinyjs::toggle("import_settings")
  })

  # Update UIs - Import Data
  observeEvent(input$import_settings, {
    req(input$import_settings)
    s <- utils::read.csv(input$import_settings$data, colClasses = "character") %>%
      dplyr::left_join(manual[, c("id", "class")], by = "id")

    # Require that coercible classes match colnames, ids, classes, and values
    if(!check_values(names(s), names(values$settings_default)) ||                   ## COLS
       !check_values(s$id, manual$id) ||                                            ## IDs
       !all(sapply(1:nrow(s), function(x) check_class(s$value[x], s$class[x]))) ||  ## CLASSES
       !all(sapply(s$value[s$class == "Numeric"], function(x) as.numeric(x) > 0))   ## VALUES
       ) {
      showModal(modalDialog(
        title = "Incorrect Settings",
        "Settings file should be one created in animalnexus.
        Try downloading a new settings file to get the correct format.
        Note that 0 values for numbers are not permitted.",
        easyClose = TRUE))
    } else {
      settings_ui(s, update = TRUE)
    }
  })

  # Save to disk ---------------------------------------------------
  output$settings_save <- downloadHandler(
    filename = paste0("animalnexus_settings_", Sys.Date(), '.csv'),
    content = function(file) {
      utils::write.csv(req(settings()), file, row.names = FALSE)
    })

  settings <- reactive({
    lapply(manual$id, function(x) req(!is.null(input[[x]])))
    if(verbose) cat("Settings - Recalculate\n")
    tibble::tibble(id = manual$id) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(value = as.character(input[[id]]))
  })

  # Save to shiny ---------------------------------------
  observe({
    req(values$settings, settings())

    shinyjs::toggleState("settings_get", condition = any(settings() != values$settings) & all(settings() != ""))
    shinyjs::toggleCssClass("settings_get", class = "btn-success", condition = any(settings() != values$settings))
    shinyjs::toggleCssClass("settings_get", class = "btn-default", condition = all(settings() == values$settings))
  })

  observeEvent(input$settings_get, {
    if(ns("") == "standalone-") {
      message("Settings tested")
      stopApp(returnValue = settings())
    } else {
      values$settings = settings()
    }
  })

  ## Help dialogues ----------------------------------------------------
  observeEvent(input$help_settings, {
    showModal(modalDialog(size = "m",
                          title = "Settings for Transformations",
                          easyClose = TRUE,
                          tagList("Each settings corresponds to an argument for the relevant function from the feedr package (e.g., visits(), move(), etc.). The argument name from the feedr function is shown as", code("argument"), ".")))
  })

  help_settings_args <- function(df) {
    lapply(unique(df$f), function(func) {
      x <- dplyr::filter(df, f == func)
      observeEvent(input[[paste0("help_", x$f[1])]], {
        showModal(modalDialog(size = "m",
                              title = paste(x$title[1]),
                              easyClose = TRUE,
                              tagList(
                                lapply(1:nrow(x), function(y) {
                                tagList(
                                  h4(x$lab[y], code(x$arg[y])),
                                  x$desc[y])
                              }))
        ))
      })
    })
  }

  help_settings_args(manual)

  # Return ----------------------------------------------------
  return(reactive({if(is.null(values$settings)) values$settings else tidyr::spread(values$settings, id, value)}))
}
