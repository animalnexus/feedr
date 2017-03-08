# Launch settings
ui_settings <- function(){
  ui_app(name = "settings")
}


settings_preamble <- function(){
  settings_functions <- c("visits", "move", "presence", "disp", "dom", "activity", "daily")

  labs <- data.frame(
    rbind(cbind(f = "activity",
                arg = c("by_logger", "sun", "keep_all", "res"),
                lab = c("Calculate activity by logger", "Calculate sunrise/sunset times",
                        "Keep all individuals", "Time resolution in minutes")),
          cbind(f = "disp", arg = "bw", lab = "Max seconds between two events"),
          cbind(f = "dom",
                arg = c("tries", "omit_cutoff"),
                lab = c("Number of iterations", "Min number of interactions per individual")),
          cbind(f = "move", arg = "all", lab = "Keep individuals which didn't move?"),
          cbind(f = "presence", arg = "bw", lab = "Max minutes between visits"),
          cbind(f = "visits",
                arg = c("allow_imp", "na_rm", "bw", "bw_imp"),
                lab = c("Allow impossible visits?", "Remove data with missing values", "Min seconds between visits", "Min seconds to travel between loggers"))), stringsAsFactors = FALSE)

  return(man %>%
           dplyr::filter(f %in% settings_functions,
                         !is.na(value),
                         !(arg %in% c("missing", "pass"))) %>%
           dplyr::left_join(labs, by = c("f", "arg")) %>%
           dplyr::mutate(id = paste0("set_", f, "_", arg),
                         f = factor(f, levels = settings_functions)) %>%
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
           lapply(1:length(unique(manual$f)), function(x) {
             x <- manual[manual$f == unique(manual$f)[x], ]
             tagList(h3(x$title[1], actionButton(ns(paste0("help_", x$f[1])), "?", class = "help")),
                     lapply(1:nrow(x), function(y) {
                       y <- x[y, ]
                       uiOutput(ns(paste0("ui_set_", y$f, "_", y$arg)))
                     }),
                     tags$hr()
             )
           })
    )
  )
}


#' @import shiny
#' @import magrittr
mod_settings <- function(input, output, session) {

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
     req(is.null(values$settings))
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
        if(x$class == "Numeric") ui <- numericInput(label = x$lab,
                                                    inputId = ns(x$id),
                                                    min = 1,
                                                    value = as.numeric(x$value))
        if(x$class == "Logical") ui <- radioButtons(label = x$lab,
                                                    inputId = ns(x$id),
                                                    choices = c("Yes" = TRUE, "No" = FALSE),
                                                    selected = as.logical(x$value))
        return(output[[paste0("ui_", x$id)]] <- renderUI({ui}))
      }
    })
  }

  # Create UIs
   settings_ui(manual)

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
    s <- read.csv(input$import_settings$data, colClasses = "character") %>%
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
    req(stringr::str_detect(names(input), "set_"))
    tibble::tibble(id = names(input)[stringr::str_detect(names(input), "set_")]) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(value = as.character(input[[id]]))
  })

  # Save to shiny ---------------------------------------
  observe({
    req(values$settings, settings())

    shinyjs::toggleState("settings_get", condition = any(settings() != values$settings))
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
  settings_help <- function(df) {
    lapply(unique(df$f), function(func) {
      x <- dplyr::filter(df, f == func)
      observeEvent(input[[paste0("help_", x$f[1])]], {
        showModal(modalDialog(size = "m",
                              title = paste(x$title[1]),
                              easyClose = TRUE,
                              tagList(lapply(1:nrow(x), function(y) {
                                tagList(
                                  h4(x$lab[y], "(", code(x$arg[y]), ")"),
                                  x$desc[y])
                              }))
        ))
      })
    })
  }

  observe({
    req(any(stringr::str_detect(names(input), "help_")))
    settings_help(manual)
  })

  # Return ----------------------------------------------------
  return(reactive(values$settings))
}
