
# Launch current

# ui_trans <- function(r) {
#   app <- shiny::shinyApp(ui = shiny::fluidPage(shinyjs::useShinyjs(),
#                                                includeCSS(system.file("extra", "style.css", package = "feedr")),
#                                                mod_UI_trans("standalone"),
#                                                mod_UI_stop("stp")),
#                          server = function(input, output, session) {
#                            shiny::callModule(mod_trans, "standalone", r = reactive({r}))
#                            shiny::callModule(mod_stop, "stp")
#                          }
#   )
#   shiny::runApp(app, launch.browser = TRUE)
# }

ui_trans <- function(r) {
  ui_app(name = "trans", r = reactive({r}), launch.browser = TRUE)
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
           uiOutput(ns("dl_buttons"))
    ),
    column(9, uiOutput(ns("data_tables")))
    )
}

# Module server function
#' @import shiny
#' @import magrittr
mod_trans <- function(input, output, session, r, verbose = FALSE) {

  ns <- session$ns

  types <- tibble::tibble(
    names = c("Raw", "Visit", "Presence", "Movement", "Displacement", "Dominance", "Activity", "Daily Activity"),
    f_name = c("raw", "visits", "presence", "movements", "displacments", "dominance", "activity", "daily_activity"),
    n = c("r", "v", "f", "m", "disp", "dom", "a", "da"),
    req = c(NA, "r", "v", "v", "v", "disp", "f", "a"),
    p = round(seq(0, 1, length.out = 8), 2),
    details = c("<h3>Raw RFID data</h3> <p>Each row corresponds to an RFID 'read' event.</p>",
                "<h3>Visits</h3> <p>Each row corresponds to a single 'visit' to the reader. Visits are defined as a series of consecutive RFID reads, with each read occurring within 3s of the next. See the visits() function in the feedr package for R to fine tune these settings.</p><p>Bird N and Feeder N refer to the total number of individuals and readers in the data, respectively.</p>",
                "<h3>Feeding bouts</h3> <p>Each row corresponds to a single 'feeding bout' at the reader if the reader is a feeder, or a period of time spent near the reader otherwise. Feeding bouts are defined as a series of visits at a single feeder separated by no more than 15min. See the feeding() function in the feedr package for R to fine tune these settings.</p><p>Feed start and end reflect the start and end of the feeding bout and feed length refers to the length in minutes of the feeding bout.</p><p>Bird N and Feeder N refer to the total number of individuals and readers in the data, respectively.</p>",
                "<h3>Movements</h3> <p>Each two rows correspond to a single 'movement' from one reader to another. A movement is defined as the last and first consecutive visits made by an individual to two different readers.</p><p>Move Id refers to the unique identifier of each movement made by an individual. Move Path reflects the unique path between readers (without accounting for direction) whereas Move Dir reflect s the unique path between readers, including direction. Strength is a measure of how connected two readers are and is calculated as the inverse of time taken to move between the readers.</p>",
                "<h3>Displacements</h3> <p>Each row corresponds to a single displacement event recorded at a particular RFID logger. Displacements are events when one bird leaves the feeder within 5s of the arrival of another. In some species this can be used to infer dominance.</p>",
                "<h3>Dominance</h3> <p>Assuming that displacements reflect dominance, calculate a dominance matrix. The function attempts to determine dominance rank for all individuals by order individuals based individual win/loss interactions and minimizing reversals (situations in which A beats B, B beats C, and C beats A). This matrix reflects a starting point and may require more work by the research. Displacers are across the top, displacees down the side. Values reflect wins in the upper triangle, losses in the lower triangle.</p>",
                "<h3>Activity</h3> <p>Each row corresponds to a 15-min time period and is scored as active or inactive (activity_c) or 1 or 0 (activity). Activity is definied by whether or not the individual had a 'presence' bout (at any logger) which overlapped the 15-min time slot. Rise and set reflect the time of sunrise and sun set based on the lat/lon of the logger.</p>",
                "<h3>Daily Activity</h3> <p>Each row corresponds to an average activity score for that 15-min period calculated across all days included in the activity data. Rise and set reflect the time of sunrise and sun set based on the lat/lon of the logger.</p>"))

  trans <- reactiveValues()
  msg <- reactiveValues()
  all <- reactiveValues()
  
  ## Transform data
  observeEvent(r(), {
    req(r())
    
    all$r <- r()
    if(!("dataaccess" %in% names(all$r))) all$r$dataaccess <- 0
    trans$r <- all$r %>%
      dplyr::filter(dataaccess == 0) %>%
      dplyr::select(-dataaccess)

    withProgress(message = "Transforming Data", {
      lapply(2:nrow(types), function(x) { #Omit 'r'
        x <- types[x, ]
        if(verbose) cat(x$n, "\n")
        setProgress(detail = x$names, value = x$p)
        sink(con <- textConnection("temp","w"), type = "message")
        if(is.null(trans[[x$req]])) message(paste0("No ", types$names[types$n == x$req], " data"))
        if(x$n == "dom") message("Note that only first matrix returned (there may be alternatives)")
        trans[[x$n]] <- switch(x$n, 
                               "v" = try(visits(all$r, allow_imp = TRUE)),
                               "m" = try(move(trans$v)),
                               "f" = try(feeding(trans$v)),
                               "disp" = try(disp(trans$v)$displacements),
                               "dom" = try(dom(disp(trans$v))$matrices[[1]]),
                               "a" = try(activity(trans$f)),
                               "da" = try(daily(trans$a)))
        if(any(class(trans[[x$n]]) == "try-error")) trans[[x$n]] <- NULL
        msg[[x$n]] <- temp
        sink(type = "message"); close(con)
        
        if(x$n == "v") {
          all$v <- trans[[x$n]]
          if(!is.null(trans[[x$n]])){
            trans[[x$n]] <- trans[[x$n]] %>%
              dplyr::filter(dataaccess == 0) %>%
              dplyr::select(-dataaccess)
          }
        }
      })
    })
  })

  ## Create download buttons 
  output$dl_buttons <- renderUI({
    lapply(1:nrow(types), function(x) {
      x <- types[x, ]
      p(downloadButton(ns(paste0('data_dl_', x$n)), x$names))
    })
  })
  
  ## Activate/deactivate buttons
  observe({
    req("data_tabs" %in% names(input))
    lapply(types$n, function(x) {
      shinyjs::toggleState(paste0("data_dl_", x), condition = !is.null(trans[[x]]) && nrow(trans[[x]]) > 0)
    })
    shinyjs::toggleState("data_dl", condition = "data_tabs" %in% names(input))
  })

  msg_select <- "Please select data through the Database or by Importing"
  msg_error <- "No data (see log for more details)"
  msg_private <- "None of the currently selected data is available for download.\n
  Some of data in our Database is restricted to visualizations only to protect the hard work of scientists until they've had a chance to publish their findings."


  ## Output Data Tables
  observeEvent(trans$r, {
    lapply(types$n, function(x) {
      temp <- trans[[x]]
      output[[paste0("dt_", x)]] <- DT::renderDataTable({
        validate(need(!is.null(all$r), msg_select))
        validate(need(!is.null(trans$r) && nrow(temp) > 0, msg_error))
        validate(need(!is.null(temp) && nrow(temp) > 0, msg_private))

        if(x == "dom"){
          DT::datatable(temp, filter = "none",
                        options = list(ordering = FALSE,
                                       pageLength = 100))
        } else {
          DT::datatable(temp,
                        filter = "top",
                        options = list(pageLength = 100),
                        rownames = FALSE,
                        colnames = gsub("_", " ", names(temp)) %>% gsub("\\b(\\w)", "\\U\\1", ., perl=TRUE))
        }
      })
    })
  })
  
  ## Setup downloads
  observeEvent(trans$r, {
    lapply(1:nrow(types), function(x) {
      x <- types[x, ]
      output[[paste0("data_dl_", x$n)]] <- downloadHandler(
        filename = paste0(x$f_name, "_", Sys.Date(), '.csv'),
        content = function(file) {
          write.csv(req(trans[[x$n]]), file, row.names = FALSE)
        })
    })
  })
  
  
  ## Download All
  output$data_dl <- downloadHandler(
    filename = paste0("feedr_all_", Sys.Date(), ".zip"),
    content = function(file) {
      tmpdir <- tempdir()
      setwd(tempdir())
      cat(tempdir())
      
      fs <- paste0(types$f_name, "_", Sys.Date(), ".csv")
      for(d in 1:nrow(types)){
        write.csv(trans[[types$n[d]]], file = fs[d], row.names = FALSE)
      }
      cat(fs)
      
      zip(zipfile = file, files = fs)
    },
    contentType = "application/zip"
  )

  ## Prepare tabs
  output$data_tables <- renderUI({
    tabs <- lapply(1:nrow(types), function(x) {
      x <- types[x, ]
      tabPanel(x$names, DT::dataTableOutput(ns(paste0("dt_", x$n))))
    })
    tabs[[length(tabs) + 1]] <- tabPanel("Log", htmlOutput(ns("log")))
    tabs$id = ns("data_tabs")
    do.call(tabsetPanel, tabs)
  })

 ## Prepare Log Tab
  output$log <- renderText({
    req("v" %in% names(msg))
    l <- lapply(2:nrow(types), function(x) {
      x <- types[x, ]
      tagList(h3(x$names), code(lapply(msg[[x$n]], br)))
    })
    as.character(do.call("tagList", l))
  })

  ## Data Descriptions

  output$data_desc <- renderText({
    req(input$data_tabs)
    types$details[types$names == input$data_tabs]
  })


  return(c(r = reactive({all$r}),
           v = reactive({all$v})))
}
