#' Create figures
#'
#' @export
figures <- function(r) {

  app <- shiny::shinyApp(ui = shiny::fluidPage(mod_UI_figures("standalone")),
                         server = function(input, output, session) {
                           shiny::callModule(mod_figures, "standalone", r = r)
                         }
  )
  shiny::runApp(app, display.mode = "normal")
}

## Get figures
#' @import shiny
#' @import magrittr
#' @export
mod_UI_figures <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(3,
             h3("Variables"),
             radioButtons(ns("data"), label = "Data type", choices = c("Raw RFID Reads", "Visits", "Feeding Bouts", "Movements"), selected = "Visits"),
             uiOutput(ns("UI_var_y")),
             uiOutput(ns("UI_var_x")),
             uiOutput(ns("UI_var_fill")),
             uiOutput(ns("UI_var_facet")),
             radioButtons(ns("plot_type"), label = "Type of plot", choices = c("Scatterplot", "Boxplot", "Barplot")),
             actionButton(ns("figure_update"), "Update Figure"),
             actionButton(ns("pause"), "Pause")
      ),
      column(9
             ,
             plotOutput(ns("figure"), height = 400)
      )
    )
  )
}


#' @import shiny
#' @import magrittr
#' @import ggplot2
#' @export
mod_figures <- function(input, output, session, r, v, p, m) {

  ns <- session$ns

  observeEvent(input$pause, {
    browser()
  })

  data <- reactive({
    req(input$data)
    if(input$data == "Raw RFID Reads") {
      d <- r
    } else {
      d <- visits(r)
      if(input$data == "Visits") d <- dplyr::rename(d, time = start)
      if(input$data == "Feeding Bouts") d <- presence(d) %>% dplyr::rename(d, time = start)
      if(input$data == "Movements") d <- move(d)
    }
    d
  })

  # Which variables?
  output$UI_var_y <- renderUI({
    req(data(), input$data)

    if(input$data == "Raw RDIF Reads") c <- c("Total Animals", "Total Reads", "Total Loggerss")
    if(input$data == "Visits") c <- c("Avg. Visit Duration", "Total Animals", "Total Visits", "Total Loggers", "Total Visit Duration")
    if(input$data == "Presence") c <- c("Avg. Presence Duration", "Total Animals", "Total # times Present", "Total Loggers", "Total Presence Duration")
    if(input$data == "Movements") c <- c("Total Movements")

    selectizeInput(ns("var_y"), label = "Plot:",
                 choices = c)
  })

  output$UI_var_x <- renderUI({
    req(data(), input$data)
    c <- c("Time" = "time", "Date" = "date", "Species" = "species", "Age" = "age", "Sex" = "sex", "Animal ID" = "animal_id", "Logger ID" = "logger_id")
    if(input$data == "Movements") c <- c(c, "Path", "Path/Direction")

    selectizeInput(ns("var_x"), label = "By:",
                 choices = c)
  })

  output$UI_var_fill <- renderUI({
    req(data())
    c <- c("None" = "none", "Species" = "species", "Age" = "age", "Sex" = "sex", "Animal ID" = "animal_id", "Logger ID" = "logger_id")
    selectizeInput(ns("var_fill"), label = "Colour by:",
                 choices = c)
  })

  # Which facet?
  output$UI_var_facet <- renderUI({
    req(data())
    c <- c("None" = "none", "Species" = "species", "Age" = "age", "Sex" = "sex", "Animal ID" = "animal_id", "Logger ID" = "logger_id")
    radioButtons(ns("var_facet"), label = "Facet by:",
                       choices = c)
  })

  plot_data <- reactive({
    req(data, input$plot_type, input$var_x, input$var_y)

    d <- data() %>%
      dplyr::mutate(date = as.Date(time)) %>%
      dplyr::mutate_('x' = input$var_x) %>%
      dplyr::group_by(x)

    if(input$var_x == "time" & input$plot_type != "Scatterplot") d$x <- lubridate::floor_date(d$x, unit = "hour")

    if(!is.null(input$var_fill) && input$var_fill != "none") {
      d <- d %>%
        dplyr::mutate_('fill' = input$var_fill) %>%
        dplyr::group_by(fill, add = TRUE)
    }
    if(!is.null(input$var_facet) && input$var_facet != "none") {
      d <- d %>%
        dplyr::mutate_('facet' = input$var_facet) %>%
        dplyr::group_by(facet, add = TRUE)
    }

    if(input$var_y == "Total Animals") d <- dplyr::summarize(d, y = length(unique(animal_id)))
    if(input$var_y == "Total Loggers") d <- dplyr::summarize(d, y = length(unique(logger_id)))
    if(input$var_y %in% c("Total Reads", "Total Visits", "Total Feeding Bouts")) d <- dplyr::summarize(d, y = length(animal_id))

    if(input$var_y == "Total Visit Duration") d <- dplyr::summarize(d, y = as.numeric(sum(difftime(end, time, units = "min"))))
    if(input$var_y == "Total Feeding Duration") d <- dplyr::summarize(d, y = as.numeric(sum(difftime(end, time, units = "min"))))
    if(input$var_y == "Avg. Visit Duration") d <- dplyr::summarize(d, y = as.numeric(mean(difftime(end, time, units = "min"))))
    if(input$var_y == "Avg. Feeding Duration") d <- dplyr::summarize(d, y = as.numeric(mean(difftime(end, time, units = "min"))))

    d
  })


  # Plot
  figure <- eventReactive(input$figure_update , {
    req(plot_data(), input$var_x, input$var_y, input$plot_type, input$var_facet, input$var_fill)

    g <- ggplot(data = plot_data()) + theme_bw(base_size = 16)
    if(input$plot_type == "Scatterplot"){
      if(input$var_fill != "none") g <- g + geom_point(aes(x = x, y = y, colour = fill)) else g <- g + geom_point(aes(x = x, y = y))
    }
    if(input$plot_type == "Boxplot") {
      if(input$var_fill != "none") g <- g + geom_boxplot(aes(x = factor(x), y = y, fill = fill)) else g <- g + geom_boxplot(aes(x = factor(x), y = y))
    }
    if(input$plot_type == "Barplot") {
      if(input$var_fill != "none") g <- g + geom_bar(stat = "identity", position = "dodge", aes(x = factor(x), y = y, fill = fill)) else g <- g + geom_bar(stat = "identity", aes(x = factor(x), y = y))
    }
    if(input$var_facet != "none") g <- g + facet_wrap(~ facet)
    g <- g + labs(x = input$var_x, y = input$var_y)
    g
  })

  output$figure <- renderPlot({
    figure()
  })

}

