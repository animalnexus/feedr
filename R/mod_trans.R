
# Launch current

ui_trans <- function(r) {
  app <- shiny::shinyApp(ui = shiny::fluidPage(shinyjs::useShinyjs(),
                                               includeCSS(system.file("extra", "style.css", package = "feedr")),
                                               mod_UI_trans("standalone"),
                                               mod_UI_stop("stp")),
                         server = function(input, output, session) {
                           shiny::callModule(mod_trans, "standalone", r = reactive({r}))
                           shiny::callModule(mod_stop, "stp")
                         }
  )
  shiny::runApp(app, launch.browser = TRUE)
}

#' @import shiny
#' @import magrittr
mod_UI_trans <- function(id) {
  ns <- NS(id)

  tagList(
    column(3,
           htmlOutput("data_desc"),
           hr(),
           h3("Downloads"),
           p(shinyjs::disabled(downloadButton('data_dl', 'All'))),
           p(shinyjs::disabled(downloadButton("data_dl_r", "Raw"))),
           p(shinyjs::disabled(downloadButton("data_dl_v", "Visits"))),
           p(shinyjs::disabled(downloadButton("data_dl_f", "Feeding"))),
           p(shinyjs::disabled(downloadButton("data_dl_m", "Movements"))),
           p(shinyjs::disabled(downloadButton("data_dl_disp", "Displacements"))),
           p(shinyjs::disabled(downloadButton("data_dl_dom", "Dominance")))
    ),
    column(9,
           tabsetPanel(type = "tabs", id = "data_tabs",
                       tabPanel("Raw Data", DT::dataTableOutput(ns("dt_r"))),
                       tabPanel("Visits Data", DT::dataTableOutput(ns("dt_v"))),
                       tabPanel("Feeding Data", DT::dataTableOutput(ns("dt_f"))),
                       tabPanel("Movement Data", DT::dataTableOutput(ns("dt_m"))),
                       tabPanel("Displacement Data", DT::dataTableOutput(ns("dt_disp"))),
                       tabPanel("Dominance Data", DT::dataTableOutput(ns("dt_dom"))),
                       tabPanel("Activity Data", DT::dataTableOutput(ns("dt_a"))),
                       tabPanel("Daily Activity Data", DT::dataTableOutput(ns("dt_da")))
           )
    )

  )

}

# Module server function
#' @import shiny
#' @import magrittr
mod_trans <- function(input, output, session, r, verbose = FALSE) {

  ns <- session$ns

  types <- c("r", "v", "f", "m", "disp", "dom", "a", "da")

  trans <- reactiveValues()

  observeEvent(r(), {
    req(r())
    trans$r <- r()
    withProgress(message = "Transforming Data", {
      setProgress(detail = "Visits", value = 0); trans$v <- try(visits(trans$r, allow_imp = TRUE), silent = TRUE)
      setProgress(detail = "Movements", value = 0.15); trans$m <- try(move(trans$v), silent = TRUE)
      setProgress(detail = "Feeding time", value = 0.3); trans$f <- try(feeding(trans$v), silent = TRUE)
      setProgress(detail = "Displacements", value = 0.45); trans$disp <- try(disp(v = trans$v), silent = TRUE)
      setProgress(detail = "Dominance", value = 0.6); trans$dom <- try(dom(trans$disp), silent = TRUE)
      setProgress(detail = "Activity", value = 0.75); trans$a <- try(activity(trans$f), silent = TRUE)
      setProgress(detail = "Daily Activity", value = 0.9); trans$da <- try(daily(trans$a), silent = TRUE)
    })

    lapply(names(trans), function(x) {
      if("try-error" %in% class(trans[[x]])) trans[[x]] <- NULL
      })

    #trans$data_reset <- FALSE
  })



  observeEvent(trans, {
    req(names(trans))
    lapply(types, function(x) {
      if(!is.null(trans[[x]])){
        if(!("dataaccess" %in% names(trans[[x]]))) trans[[x]]$dataaccess <- 1
        trans[[paste0(x, "_dl")]] <- trans[[x]] %>%
          dplyr::filter(dataaccess == 0) %>%
          dplyr::select(-dataaccess)
      } else trans[[paste0(x, "_dl")]] <- NULL
    })
    #trans$all_dl <- reactiveValuesToList(trans[[grepl("_dl", names(trans))]])
  })

  ## Activate/deactivate buttons depending on whether there is any data to download:
  observe({
    req("r_dl" %in% names(trans))

    lapply(types, function(x) {
      shinyjs::toggleState(paste0("data_dl_", x), condition = nrow(trans[[paste0(x, "_dl")]]) > 0)
    })
  })

  msg_select <- c("Please select data through the Database or by Importing")

  msg_private <- "None of the currently selected data is available for download.\n
  Some of data in our Database is restricted to visualizations only to protect the hard work of scientists until they've had a chance to publish their findings."


  observeEvent(trans$r_dl, {

    lapply(types, function(x) {
      temp <- req(trans[[x]])
      temp_dl <- req(trans[[paste0(x, "_dl")]])

      output[[paste0("dt_", x)]] <- DT::renderDataTable({
        validate(need(try(nrow(temp) > 0, silent = TRUE), msg_select))
        validate(need(try(nrow(temp_dl) > 0, silent = TRUE), msg_private))

        DT::datatable(temp_dl,
                      filter = "top",
                      options = list(pageLength = 100),
                      rownames = FALSE,
                      colnames = gsub("_", " ", names(temp_dl)) %>% gsub("\\b(\\w)", "\\U\\1", ., perl=TRUE))
      })

      output[[paste0("data_dl_", x)]] <- downloadHandler(
        filename = paste0(x, "_", Sys.Date(), '.csv'),
        content = function(file) {
          write.csv(temp_dl, file, row.names = FALSE)
        })
    })
  })

  # ## Download All
  #
  # output$data_dl <- downloadHandler(
  #   filename = paste0("feedr_all_", Sys.Date(), ".zip"),
  #   content = function(file) {
  #     tmpdir <- tempdir()
  #     setwd(tempdir())
  #     cat(tempdir())
  #
  #     fs <- paste0(names(all()), "_", Sys.Date(), ".csv")
  #     for(d in 1:length(all())){
  #       write.csv(all()[[d]], file = fs[d], row.names = FALSE)
  #     }
  #     cat(fs)
  #
  #     zip(zipfile = file, files = fs)
  #   },
  #   contentType = "application/zip"
  # )

  ## Data Descriptions

  output$data_desc <- renderText({
    req(input$data_tabs)
    t <- ""
    if(input$data_tabs == "Raw Data") t <- "<h3>Raw RFID data</h3> <p>Each row corresponds to an RFID 'read' event.</p>"
    if(input$data_tabs == "Visits Data") t <- "<h3>Visits</h3> <p>Each row corresponds to a single 'visit' to the reader. Visits are defined as a series of consecutive RFID reads, with each read occurring within 3s of the next. See the visits() function in the feedr package for R to fine tune these settings.</p><p>Bird N and Feeder N refer to the total number of individuals and readers in the data, respectively.</p>"
    if(input$data_tabs == "Feeding Data") t <- "<h3>Feeding bouts</h3> <p>Each row corresponds to a single 'feeding bout' at the reader if the reader is a feeder, or a period of time spent near the reader otherwise. Feeding bouts are defined as a series of visits at a single feeder separated by no more than 15min. See the feeding() function in the feedr package for R to fine tune these settings.</p><p>Feed start and end reflect the start and end of the feeding bout and feed length refers to the length in minutes of the feeding bout.</p><p>Bird N and Feeder N refer to the total number of individuals and readers in the data, respectively.</p>"
    if(input$data_tabs == "Movement Data") t <- "<h3>Movements</h3> <p>Each two rows correspond to a single 'movement' from one reader to another. A movement is defined as the last and first consecutive visits made by an individual to two different readers.</p><p>Move Id refers to the unique identifier of each movement made by an individual. Move Path reflects the unique path between readers (without accounting for direction) whereas Move Dir reflect s the unique path between readers, including direction. Strength is a measure of how connected two readers are and is calculated as the inverse of time taken to move between the readers.</p>"
    return(t)
  })


  return(c(r = reactive({trans$r}),
           v = reactive({trans$v}),
           f = reactive({trans$f}),
           m = reactive({trans$m})))
}
