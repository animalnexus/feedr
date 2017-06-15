#' User-interface for transforming data
#'
#' An interactive shiny app for transforming data. Also available online at <http://animalnexus.ca> or
#' by launching the local animalnexus app through \code{animalnexus()}.
#'
#' See individual data transformations for more details: \code{visits()}, \code{presence()}, \code{move()}, \code{disp()}, \code{activity()}, and \code{daily()}.
#'
#' @param r Data frame. Raw RFID data to transform
#' @param verbose Logical. Print log events to console.
#'
#' @return Returns nothing. Data can be saved to disk from the user-interface, but transformation are not returned to the current R session.
#'
#' @examples
#' \dontrun{
#'   ui_trans(r = my_data)
#' }
#'
#' @export
ui_trans <- function(r, verbose = FALSE) {
  if(missing(r)) stop("ui_trans() requires raw data to transform")

  addResourcePath("assets", system.file("shiny-examples", "app_files", package = "feedr"))

  app <- shiny::shinyApp(ui = shiny::fluidPage(includeCSS(system.file("shiny-examples", "app_files", "style.css", package = "feedr")),
                                               shinyjs::useShinyjs(),
                                               mod_UI_nav("standalone",
                                                          tabPanel("Transformations", icon = icon("exchange"),
                                                                   mod_UI_trans("standalone")),
                                                          tabPanel("Settings", icon = icon("cog"),
                                                                   mod_UI_settings("standalone_trans")),
                                                          mod_UI_stop("stp"))),
                         server = function(input, output, session) {
                           settings <- shiny::callModule(mod_settings, id = "standalone_trans", verbose = verbose)
                           shiny::callModule(mod_trans, id = "standalone",
                                             r = shiny::reactive({r}),
                                             settings = settings,
                                             verbose = verbose)
                           shiny::callModule(mod_stop, id = "stp")  # Add Exit Buttons
                           session$onSessionEnded(stopApp)
                         }
  )
  shiny::runApp(app)
}

trans_preamble <- function(args = TRUE) {
  trans_functions <- c("raw", "visits", "move", "presence", "disp", "activity", "daily")
  manual <- man %>%
    dplyr::right_join(
      tibble::tibble(
        f = trans_functions,
        req = c(NA, "raw", "visits", "visits", "visits", "presence", "activity"),
        p = round(seq(0, 1, length.out = 7), 2),
        details = c("<h3>Raw RFID data</h3> <p>Each row corresponds to an RFID 'read' event.</p>",
                    "<h3>Visits</h3> <p>Each row corresponds to a single 'visit' to the reader. Visits are defined as a series of consecutive RFID reads, with each read occurring within 3s of the next. See the visits() function in the feedr package for R to fine tune these settings.</p><p>Animal N and Logger N refer to the total number of individuals and readers in the data, respectively.</p>",
                    "<h3>Presence</h3> <p>Each row corresponds to a single 'presence event' at the reader if the reader is a logger, or a period of time spent near the reader otherwise. These are defined as a series of visits at a single logger separated by no more than 15min. See the presence() function in the feedr package for R to fine tune these settings.</p><p>Start and End reflect the start and end of the time present and length refers to the length in minutes.</p><p>Animal N and Logger N refer to the total number of individuals and readers in the data, respectively.</p>",
                    "<h3>Movements</h3> <p>Each two rows correspond to a single 'movement' from one reader to another. A movement is defined as the last and first consecutive visits made by an individual to two different readers.</p><p>Move Id refers to the unique identifier of each movement made by an individual. Move Path reflects the unique path between readers (without accounting for direction) whereas Move Dir reflect s the unique path between readers, including direction. Strength is a measure of how connected two readers are and is calculated as the inverse of time taken to move between the readers.</p>",
                    "<h3>Displacements</h3> <p>Each row corresponds to a single displacement event recorded at a particular RFID logger. Displacements are events when one animal leaves the logger within 5s of the arrival of another. In some species this can be used to infer dominance.</p>",
                    "<h3>Activity</h3> <p>Each row corresponds to a 15-min time period and is scored as active or inactive (activity_c) or 1 or 0 (activity). Activity is definied by whether or not the individual had a 'presence' bout (at any logger) which overlapped the 15-min time slot. Rise and set reflect the time of sunrise and sun set based on the lat/lon of the logger.</p>",
                    "<h3>Daily Activity</h3> <p>Each row corresponds to an average activity score for that 15-min period calculated across all days included in the activity dataset. Rise and set reflect the time of sunrise and sun set based on the lat/lon of the logger.</p>
                    <p> Note that as this represents an average 24-hr activity cycle, output dates are irrelevant, as the data is tied to times, not dates. Therefore the dates are all assigned to 1970-01-01.</p>")),
      by = "f") %>%
    dplyr::mutate(title = replace(title, f == "raw", "Raw"),
                  file_name = tolower(stringr::str_extract(title, "^[^ ]*")))

  if(args) return(dplyr::filter(manual, !is.na(value), !(arg %in% c("missing", "pass"))))
  if(!args) return(unique(dplyr::select(manual, -arg, -desc, -class, -value, -id, -lab)))
}

#' @import shiny
#' @import magrittr
mod_UI_trans <- function(id) {
  ns <- NS(id)

  tagList(
    column(3,
           htmlOutput(ns("data_desc")),
           hr(),
           h3("Downloads"),
           shinyjs::disabled(downloadButton(ns("data_dl"), "All")),
           hr(),
           uiOutput(ns("dl_buttons")),
           downloadButton(ns('data_dl_log'), label = "Log")
    ),
    column(9, uiOutput(ns("data_tables")))
  )
}

# Module server function
#' @import shiny
#' @import magrittr
mod_trans <- function(input, output, session, r, settings, verbose = FALSE) {

  ns <- session$ns

  # Values ---------------------------------------------
  trans <- reactiveValues()
  msg <- reactiveValues()
  all <- reactiveValues()

  types_args <- trans_preamble()
  types <- trans_preamble(args = FALSE)

  # Help ---------------------------------------------------
  # Data Descriptions
  output$data_desc <- renderText({
    req(input$data_tabs)
    types$details[types$title == input$data_tabs]
  })

  # Data Transformations --------------------------------
  observe({
    req(settings(), r())

    isolate({
      all$raw <- r()
      if(!("dataaccess" %in% names(all$raw))) all$raw$dataaccess <- 0
      trans$raw <- all$raw %>%
        dplyr::filter(dataaccess == 0) %>%
        dplyr::select(-dataaccess)

      withProgress(message = "Transforming Data", {
        lapply(2:nrow(types), function(x) { #Omit 'r'
          x <- types[x, ]
          if(verbose) cat("Trans -", x$f, "\n")
          setProgress(detail = x$title, value = x$p)
          sink(con <- textConnection("temp","w"), type = "message")
          if(is.null(trans[[x$req]])) message(paste0("No ", types$title[types$f == x$req], " data"))
          trans[[x$f]] <- switch(x$f,
                                 "visits" = try(visits(all$raw,
                                                  bw = as.numeric(settings()$set_visits_bw),
                                                  allow_imp = as.logical(settings()$set_visits_allow_imp),
                                                  na_rm = as.logical(settings()$set_visits_na_rm),
                                                  bw_imp = as.numeric(settings()$set_visits_bw_imp))),
                                 "move" = try(move(trans$visits, all = as.logical(settings()$set_move_all))),
                                 "presence" = try(presence(trans$visits, bw = as.numeric(settings()$set_presence_bw))),
                                 "disp" = try(disp(trans$visits, bw = as.numeric(settings()$set_disp_bw))$displacements),
                                 "activity" = try(activity(trans$presence,
                                                    res = as.numeric(settings()$set_activity_res),
                                                    by_logger = as.logical(settings()$set_activity_by_logger),
                                                    keep_all = as.logical(settings()$set_activity_keep_all),
                                                    sun = as.logical(settings()$set_activity_sun))),
                                 "daily" = try(daily(trans$activity)))
          if(any(class(trans[[x$f]]) == "try-error")) trans[[x$f]] <- NULL
          msg[[x$f]] <- temp
          sink(type = "message"); close(con)

          if(x$f == "visits") {
            all$visits <- trans[[x$f]]
            if(!is.null(trans[[x$f]])){
              trans[[x$f]] <- trans[[x$f]] %>%
                dplyr::filter(dataaccess == 0) %>%
                dplyr::select(-dataaccess)
            }
          }
        })
      })
    })
  })

  # Buttons ------------------------------------------------
  ## Create download buttons
  output$dl_buttons <- renderUI({
    lapply(1:nrow(types), function(x) {
      x <- types[x, ]
      p(downloadButton(ns(paste0('data_dl_', x$f)), x$title))
    })
  })

  ## Activate/deactivate buttons
  observe({
    req("data_tabs" %in% names(input))
    lapply(types$f, function(x) {
      shinyjs::toggleState(paste0("data_dl_", x), condition = !is.null(trans[[x]]) && nrow(trans[[x]]) > 0)
    })
    shinyjs::toggleState("data_dl", condition = ("data_tabs" %in% names(input)) && nrow(trans$raw) > 0)
  })

  # Messages -------------------------------------------------
  msg_select <- "Please select data through the Database or by Importing"
  msg_error <- "No data (see log for more details)"
  msg_private <- "None of the currently selected data is available for download.\n
  Some of data in our Database is restricted to visualizations only to protect the hard work of scientists until they've had a chance to publish their findings."


  # Table Output ---------------------------------------------
  observe({
    lapply(types$f, function(x) trans[[x]])
    isolate({
      if(verbose) cat("Trans - Update table output\n")
      lapply(types$f, function(x) {
        temp <- as.data.frame(trans[[x]])
        output[[paste0("dt_", x)]] <- DT::renderDataTable({
          validate(need(!is.null(all$raw), msg_select))
          validate(need(!is.null(all$raw) && !is.null(trans$raw) && nrow(trans$raw) > 0, msg_private))
          validate(need(nrow(temp) > 0, msg_error))

            t <- names(which(sapply(temp, lubridate::is.POSIXct)))
            for(i in t) temp[, i] <- as.character(temp[, i])
            DT::datatable(temp,
                          filter = "top",
                          options = list(pageLength = 100),
                          rownames = FALSE,
                          colnames = gsub("_", " ", names(temp)) %>% gsub("\\b(\\w)", "\\U\\1", ., perl=TRUE))
        })
      })
    })
  })

  # Downloads ---------------------------------------------------

  # Download Individual
  observeEvent(trans$raw, {
    lapply(1:nrow(types), function(x) {
      x <- types[x, ]
      output[[paste0("data_dl_", x$f)]] <- downloadHandler(
        filename = paste0(x$file_name, "_", Sys.Date(), '.csv'),
        content = function(file) {
          utils::write.csv(req(trans[[x$f]]), file, row.names = FALSE)
        })
    })
  })

  # Download Log
  output$data_dl_log <- downloadHandler(
    filename = paste0("feedr_log_", Sys.Date(), ".txt"),
    content = function(file) {
      s <- sapply(log(), function(x) c(sapply(x, function(y) paste0(paste0(y, collapse = "\n"), "\n")), "\n"))
      write(unlist(s), file)
    })

  # Download All
  output$data_dl <- downloadHandler(
    filename = paste0("feedr_all_", Sys.Date(), ".zip"),
    content = function(file) {
      tmpdir <- tempdir()
      setwd(tempdir())
      cat(tempdir())

      ## Regular files
      fs <- c(paste0(types$file_name, "_", Sys.Date(), ".csv"),
              paste0("feedr_log_", Sys.Date(), ".txt"))
      for(d in 1:nrow(types)){
        utils::write.csv(trans[[types$f[d]]], file = fs[d], row.names = FALSE)
      }

      ## Log file
      s <- sapply(log(), function(x) c(sapply(x, function(y) paste0(paste0(y, collapse = "\n"), "\n")), "\n"))
      write(unlist(s), fs[length(fs)])

      cat(fs)

      utils::zip(zipfile = file, files = fs)
    },
    contentType = "application/zip"
  )

  # Tabs -------------------------------------------------------------
  output$data_tables <- renderUI({
    if(verbose) cat("Rendering data tabs\n")
    tabs <- lapply(1:nrow(types), function(x) {
      x <- types[x, ]
      tabPanel(x$title, DT::dataTableOutput(ns(paste0("dt_", x$f))))
    })
    tabs[[length(tabs) + 1]] <- tabPanel("Log", value = "log", htmlOutput(ns("log")))
    tabs$id = ns("data_tabs")
    do.call(tabsetPanel, tabs)
  })

  # Log -------------------------------------------------------------
  log <- reactive({
    req("visits" %in% names(msg))

    validate(need(!is.null(all$raw), msg_select))
    validate(need(!is.null(all$raw) && !is.null(trans$raw) && nrow(trans$raw) > 0, msg_private))

    l <- lapply(unique(types$f)[-1], function(x) {

      #Get title
      ti <- types$title[types$f == x]

      # Get settings
      s <- types_args[types_args$f == x, ]
      if(nrow(s) > 0) {
        s$settings <- t(settings()[, s$id])
        s$settings <- stringr::str_replace_all(s$settings, c("FALSE" = "No", "TRUE" = "Yes"))
        s <- paste0(s$lab, " = ", s$settings)
      } else s <- "Nothing to set"

      # Get log messages
      m <- if(length(msg[[x]]) > 0) msg[[x]] else "No messages"
      list("title" = ti, "settings" = s, "messages" = m)
    })
    c(list(c(paste0("Date: ", Sys.Date()),
             paste0("feedr version: ", utils::packageVersion("feedr")))),
      l)
  })

  output$log <- renderText({
    as.character(tagList(
      lapply(log(), function(x) {
        if(is.null(names(x))) {
          return(tagList(HTML(paste0(x, collpase = "<br>"))))
        } else {
          tagList(h3(x$title),
                  p(strong(HTML("Settings: <br>")),
                    HTML(paste0(x$settings, collapse = "<br>"))),
                  p(strong("Log messages: "),
                    code(lapply(x$messages, br))))
        }
      })
    ))
  })


# Return ------------------------------------------------------------------
  return(c(raw = reactive({all$raw}),
           visits = reactive({all$visits})))
}
