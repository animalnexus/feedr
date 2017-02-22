
#' User-interface for downloading from the animalnexus database
#'
#' Launches an interactive shiny app for downloading data interactively. Also
#' available online at \url{http://animalnexus.ca} or by launching the local
#' animalnexus app through \code{\link{animalnexus}}. See the \code{\link{dl_data}}
#' function for a non-interactive method.
#'
#' @param verbose Logical. Print log events to console.
#' @param diagnostic Logical. Display pause button for debugging
#'
#' @return  Downloaded data frame formatted and ready to be transformed
#'
#' @seealso \code{\link{dl_data}}
#'
#' @examples
#'
#' \dontrun{
#'   my_data <- ui_db()
#' }
#'
#' @export
ui_db <- function(verbose = FALSE, diagnostic = FALSE){
  if(file.exists("/usr/local/share/feedr/db_full.R")) {
    source("/usr/local/share/feedr/db_full.R")
  } else db <- NULL
  ui_app(name = "data_db", db = db, verbose = verbose, diagnostic = diagnostic)
}



# Database data selector - UI

#' @import shiny
#' @import magrittr
#' @import shinyBS
mod_UI_data_db <- function(id) {

  ns <- shiny::NS(id)

  tagList(
    tags$style(HTML(paste0(
      "div#", ns("plot_data_ggplot")," {
      text-align: center;
      }"))),
    tags$style(HTML(paste0(
      "div#", ns("plot_data_ggplot")," img {
      max-width: 100%;
      }"))),

    fluidRow(
      column(4,
             #div(img(src = "logo.jpg", width = 400), style="text-align: left;"),
             h3("Select Data", actionButton(ns("help_data"), "?", class = "help")),
             div(id = "selection",
                 uiOutput(ns("UI_data_site_name")),
                 uiOutput(ns("UI_data_date")),
                 uiOutput(ns("UI_data_species")),
                 hr(),
                 h3("Selected Data", actionButton(ns("help_selected"), "?", class = "help")),
                 strong("Data Access: "), textOutput(ns("data_access"), inline = TRUE),
                 tableOutput(ns("data_selection")),
                 p(),
                 shinyjs::disabled(actionButton(ns("data_get"), "Get Data", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                 p(),
                 shinyjs::disabled(actionButton(ns("data_reset"), "Reset inputs")),
                 hr(),
                 div(actionButton(ns("showadv"), "Show Advanced Options"), actionButton(ns("help_adv"), "?", class = "help"))
             )
      ),
      column(8,
             div(shinyjs::disabled(actionButton(ns("map_update"), "Update map")), style = "text-align: center", actionButton(ns("help_map"), "?", class = "help")),
             p(),
             leafletOutput(ns("map_data"), height = 600),
             plotOutput(ns("plot_data_ggplot"),
                        brush = brushOpts(
                          id = ns("plot_data_brush"),
                          direction = "x",
                          delay = 700,
                          delayType = "debounce",
                          resetOnNew = TRUE), height = "100%"),
             div(strong(textOutput(ns("text_time"))), style = "text-align: center;")
      )
    ),
    fluidRow(
      shinyjs::hidden(div(id = ns("advanced"),
                          h3("Advanced Options"),
                          uiOutput(ns("UI_data_animal_id")),
                          uiOutput(ns("UI_data_logger_id"))))
    )
  )
}

# Module server function
#' @import shiny
#' @import magrittr
#' @import lubridate
#' @import RPostgreSQL
#' @import DBI
mod_data_db <- function(input, output, session, db, verbose = TRUE) {

  ns <- session$ns

  # Values ----------------------------------------------------

  values <- reactiveValues(
    pre_data = NULL,          # Stores download values before returned
    data_map = NULL,          # Stores values which displayed on map
    data_time = NULL          # Stores values which displayed on time plot
  )

  debounce_int <- 700 # Debounce interval

  # Database ----------------------------------------------------

  if(!is.null(db) && !curl::has_internet()) db <- NULL

  ## Get data base details
  if(!is.null(db)) {

    withProgress(message = "Loading...", detail = "Connecting to server...", value = 0, {
    suppressWarnings({
      if(verbose) cat("Connecting to server...\n")
      con <- dbConnect(dbDriver("PostgreSQL"), host = db$host, port = db$port, dbname = db$name, user = db$user, password = db$pass)

      setProgress(value = 0.15, detail = "Getting logger data..")
      if(verbose) cat("Getting logger data...\n")
      #   incProgress(1/5)
      loggers_all <- dbGetQuery(con, statement = paste("SELECT feeders.feeder_id, feeders.site_name, feeders.loc, fieldsites.dataaccess",
                                                       "FROM feeders, fieldsites",
                                                       "WHERE (fieldsites.site_name = feeders.site_name)")) %>%
        load_format() %>%
        dplyr::mutate(site_name = factor(site_name))

      setProgress(value = 0.30, detail = "Getting site data..")
      if(verbose) cat("Getting site data...\n")
      #    incProgress(2/5)
      sites_all <- loggers_all %>%
        dplyr::group_by(site_name) %>%
        dplyr::summarize(lon = mean(lon), lat = mean(lat), dataaccess = unique(dataaccess)) %>%
        dplyr::mutate(site_name = factor(site_name))

      setProgress(value = 0.45, detail = "Getting animal data..")
      if(verbose) cat("Getting animal data...\n")
      #  incProgress(3/5)

      animals_all <- dbGetQuery(con, statement = paste("SELECT bird_id, species, site_name, age, sex, tagged_on FROM birds",
                                                      "WHERE birds.species NOT IN ('XXXX')")) %>%
        load_format() %>%
        dplyr::mutate(species = factor(species),
                      site_name = factor(site_name),
                      animal_id = factor(animal_id))

      setProgress(value = 0.60, detail = "Getting sample information..")
      if(verbose) cat("Getting sample information...\n")
      #    incProgress(4/5)
      counts <- dbGetQuery(con,
                           statement = paste0("SELECT raw.visits.bird_id, raw.visits.feeder_id, DATE(raw.visits.time), ",
                                              "COUNT(*) ",
                                              "FROM raw.visits ",
                                              "GROUP BY DATE(raw.visits.time), raw.visits.feeder_id, raw.visits.bird_id"#,
                           )) %>%
        load_format() %>%
        dplyr::inner_join(animals_all[, c("site_name", "species", "animal_id")], by = "animal_id") %>%
        dplyr::mutate(count = as.numeric(count),
                      date = as.Date(date),
                      species = factor(species, levels = sort(unique(animals_all$species))),
                      site_name = factor(site_name, levels = sort(sites_all$site_name)),
                      animal_id = factor(animal_id, levels = sort(unique(animals_all$animal_id))),
                      logger_id = factor(logger_id, levels = sort(unique(loggers_all$logger_id))))
      dbDisconnect(con)
    })


    #  incProgress(5/5)
    setProgress(value = 0.75, detail = "Summarizing samples..")
    if(verbose) cat("Summarizing samples...\n")
    counts_sum <- dplyr::bind_rows(
      get_counts(counts, summarize_by = "site_name"),
      get_counts(counts, summarize_by = "species"),
      get_counts(counts, summarize_by = "date"),
      get_counts(counts, summarize_by = "animal_id"),
      get_counts(counts, summarize_by = "logger_id"))

    })
  }

  # Update selection ----------------------------------------------------

  # Update date selection based on plot brush
  observeEvent(input$plot_data_brush, {
    if(verbose) cat("Get time brush input\n")

    cnts <- get_counts(c = counts_site(), summarize_by = "date") %>%
      dplyr::mutate(choices = as.Date(choices))

    dates <- c(as.Date(input$plot_data_brush$xmin, lubridate::origin),
               as.Date(input$plot_data_brush$xmax, lubridate::origin))
    if(dates[1] < min(cnts$choices)) dates[1] <- min(cnts$choices)
    if(dates[2] > max(cnts$choices)) dates[2] <- max(cnts$choices)
    updateDateRangeInput(session, "data_date",
                         start = dates[1],
                         end = dates[2])
  })

  # Update all selections based on date
  observeEvent(input$data_date, {
    if(verbose) cat("Update UI selections based on dates\n")

    sel <- counts_site() %>%
      dplyr::filter(date %within% interval(input$data_date[1], input$data_date[2]))

    if(!compare_values(sel$species, input$data_species)) {
      if(verbose) cat("  - species\n")
      updateCheckboxGroupInput(session, "data_species",
                               selected = unique(sel$species))
    }
    if(!compare_values(sel$animal_id, input$data_animal_id)) {
      if(verbose) cat("  - animal_id\n")
      updateCheckboxGroupInput(session, "data_animal_id",
                               selected = unique(sel$animal_id))
    }
    if(!compare_values(sel$logger_id, input$data_logger_id)) {
      if(verbose) cat("  - logger_id\n")
      updateCheckboxGroupInput(session, "data_logger_id",
                               selected = unique(sel$logger_id))
    }
  }, priority = 10)

  # Update selections based on species
  observeEvent(input$data_species, {
    sel <- counts_site() %>%
      dplyr::filter(date %within% interval(input$data_date[1], input$data_date[2]),
                    species %in% input$data_species)

    if(verbose) cat("Update UI selections based on species\n")

    if(!compare_values(sel$animal_id, input$data_animal_id)) {
      if(verbose) cat("  - animal_id\n")
      updateCheckboxGroupInput(session, "data_animal_id",
                               selected = unique(sel$animal_id))
    }
    if(!compare_values(sel$logger_id, input$data_logger_id)) {
      if(verbose) cat("  - logger_id\n")
      updateCheckboxGroupInput(session, "data_logger_id",
                               selected = unique(sel$logger_id))
    }
  }, priority = 5)

  input_selection <- throttle(reactive({
    req(input$data_animal_id, input$data_logger_id, !is.null(db))
    if(verbose) cat("Update input selections\n")

    list('species' = as.character(unique(input$data_species)),
         'date' = c(min(input$data_date), max(input$data_date)),
         'animal_id' = as.character(unique(input$data_animal_id)),
         'logger_id' = as.character(unique(input$data_logger_id)))
  }), debounce_int)

  # Input variables
  data_selection <- reactive({
    if(verbose) cat("Update data selections\n")
    get_counts(counts_site(), filter = input_selection())
  })

  # Counts and info ----------------------------------------------------

  ## Subset of counts reflecting site
  counts_site <- reactive({
    req(input$data_site_name)
    droplevels(counts[counts$site_name == input$data_site_name, ])
  })

  ## Table showing current selection
  output$data_selection <- renderTable({
    req(data_selection())
    c <- get_counts(data_selection(), summarize_by = "species")
    if(!is.null(c)) return(dplyr::select(c, "Species" = choices, "Total" = sum))
    if(is.null(c)) return(data.frame(Species = levels(data_selection()$species), Total = 0))
  }, digits = 0, include.rownames = FALSE)

  output$data_access <- renderText({
    req(input$data_site_name)
    req(input$data_site_name != "")
    if(sites_all$dataaccess[sites_all$site_name == input$data_site_name] == 0) return("Fully Public")
    if(sites_all$dataaccess[sites_all$site_name == input$data_site_name] == 1) return("Visualizations Only")
  })

  # Resets ----------------------------------------------------

  # Reset all with reset button
  observeEvent(input$data_reset, {
    req(input$data_site_name)
    if(verbose) cat("Reset data selection...\n")
    #values$input <- values$input_previous <- values$keep <- NULL
    updateSelectInput(session, "data_site_name",
                      selected = c("Choose site" = ""))
  })

  # Reset values with site selection
  observeEvent(input$data_site_name, {
    req(counts_site())

    #values$input <- values$input_previous <- NULL
    #values$keep <- counts_site()
  }, priority = 100)


  # Buttons  ---------------------------------------------

  ## Get data when Selection made
  observe({
    if(is.null(input$data_site_name) || input$data_site_name != ""){
      shinyjs::enable(id = "data_get")
      shinyjs::enable(id = "data_reset")
    } else {
      shinyjs::disable(id = "data_get")
      shinyjs::disable(id = "data_reset")
    }
  })


  observeEvent(data_selection(), {
    if(identical(data_selection(), values$data_map)) {
      shinyjs::disable(id = "map_update")
    } else {
      shinyjs::enable(id = "map_update")
    }
  }, priority = 25)

  # Output UIs ---------------------------------------------

  ## UI Site_name
  output$UI_data_site_name <- renderUI({
    req(!is.null(db))
    selectInput(ns("data_site_name"), "Sites:",
                choices = c(c("Choose site" = ""), choices(counts_sum, "site_name")))
  })

  ## UI Species
  output$UI_data_species <- renderUI({
    req(input$data_site_name)
    cnts <- get_counts(c = counts_site(), summarize_by = "species")
    checkboxGroupInput(ns("data_species"), "Species",
                       choices = choices(cnts, "species"),
                       selected = selected(cnts, "species"))
  })

  ## UI Date range - initialization
  output$UI_data_date <- renderUI({
    req(input$data_site_name)
    cnts <- get_counts(c = counts_site(), summarize_by = "date")
    dateRangeInput(ns("data_date"), "Dates to include:",
                   min = min(as.Date(cnts$choices[cnts$variable == "date"])),
                   max = max(as.Date(cnts$choices[cnts$variable == "date"])),
                   start = min(as.Date(cnts$choices[cnts$variable == "date"])),
                   end = max(as.Date(cnts$choices[cnts$variable == "date"])))
  })


  ## UI animal_id
  output$UI_data_animal_id <- renderUI({
    req(input$data_site_name)
    cnts <- get_counts(c = counts_site(), summarize_by = "animal_id")
    checkboxGroupInput(ns("data_animal_id"), "Select animal ids",
                       choices = choices(cnts, "animal_id"),
                       selected = selected(cnts, "animal_id"), inline = TRUE)
  })



  ## UI logger_id
  output$UI_data_logger_id <- renderUI({
    req(input$data_site_name)
    cnts <- get_counts(c = counts_site(), summarize_by = "logger_id")
    checkboxGroupInput(ns("data_logger_id"), "Select logger ids",
                       choices = choices(cnts, "logger_id"),
                       selected = selected(cnts, "logger_id"), inline = TRUE)
  })

  ## Toggle advanced options
  observeEvent(input$showadv, {
    shinyjs::toggle(id = "advanced")
  })

  ## Render UIs even when hidden
  outputOptions(output, 'UI_data_species', suspendWhenHidden = FALSE)
  outputOptions(output, 'UI_data_date', suspendWhenHidden = FALSE)
  outputOptions(output, 'UI_data_animal_id', suspendWhenHidden = FALSE)
  outputOptions(output, 'UI_data_logger_id', suspendWhenHidden = FALSE)


  ## Get data ---------------------------------------------

  ## Download Selected Data
  observeEvent(input$data_get, {
    req(data_selection(), !is.null(db))
    if(verbose) cat("Downloading selected data...\n")

    con <- dbConnect(dbDriver("PostgreSQL"),host = db$host, port = db$port, dbname = db$name, user = db$user, password = db$pass)

    d <- data_selection()

    dates <- c(min(d$date), max(d$date) + lubridate::days(1))
    if(dates[2] > dates[1]) {
      dates <- paste0("AND raw.visits.time >= '", dates[1], "' AND raw.visits.time <= '", dates[2], "'")
    } else {
      dates <- paste0("AND raw.visits.time >= '", dates[1], "' AND raw.visits.time < ('", dates[2], "'::date + '1 day'::interval)")
    }

    withProgress(message = "Retrieving Data...",
                 suppressWarnings(
                   data <- dbGetQuery(con,
                                      statement = paste0("SELECT raw.visits.bird_id, raw.visits.feeder_id, raw.visits.time ",
                                                         "FROM raw.visits ",
                                                         "WHERE raw.visits.bird_id IN ( '", paste0(unique(d$animal_id), collapse = "', '"), "' ) ",
                                                         dates
                                      ))
                 )
    )
    dbDisconnect(con)

    if(nrow(data) > 0) {
      if(verbose) cat("Formatting selected data...")
      tz_disp <- tz_offset(ifelse(d$site_name[1] == "Kamloops, BC", "America/Vancouver", "America/Costa_Rica"), tz_name = TRUE)
      data <- data %>%
        dplyr::mutate(time = lubridate::with_tz(time, "UTC")) %>% # Because otherwise has system timezone
        load_format(tz = "UTC", tz_disp = tz_disp) %>%
        dplyr::mutate(animal_id = factor(animal_id, levels = sort(unique(animals_all$animal_id))),
               logger_id = factor(logger_id, levels = sort(unique(loggers_all$logger_id)))) %>%
        dplyr::left_join(animals_all, by = c("animal_id")) %>%
        dplyr::left_join(loggers_all, by = c("logger_id", "site_name")) %>%
        dplyr::arrange(time)
    } else data <- NULL

    values$pre_data <- data
  })

  # Where to send data ---------------------------------------
  observeEvent(values$pre_data, {
    req(values$pre_data)
    if(ns("") == "standalone-") {
      message("Data successfully downloaded")
      stopApp(returnValue = dplyr::select(values$pre_data, -dataaccess))
    } else {
      values$data = values$pre_data
    }
    values$pre_data <- NULL
  })


  ## Map ----------------------------------------------------

  ## Render Map
  output$map_data <- renderLeaflet({
    validate(need(!is.null(db), message = "No Database access. To work with local data, use the 'Import' tab. To work with the Database check out animalnexus.ca"))
    req(!is.null(db))
    if(verbose) cat("Initializing data map...\n")

    #Get counts summed across all dates
    suppressWarnings(
      s <- get_counts(counts, summarize_by = "site_name") %>%
        dplyr::left_join(sites_all, by = c("choices" = "site_name"))
    )

    leaflet(data = sites_all) %>%
        addTiles(group = "Open Street Map") %>%
        addProviderTiles("Stamen.Toner", group = "Black and White") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
        addMarkers(~lon, ~lat,
                   popup  = htmltools::htmlEscape(sites_all$site_name),
                   group = "Sites") %>%
        addLayersControl(baseGroups = c("Satellite", "Terrain", "Open Street Map", "Black and White"),
                         overlayGroups = "Sites",
                         options = layersControlOptions(collapsed = TRUE)) %>%
        addScaleBar(position = "bottomright") %>%
        setView(lng = -98.857903, lat = 21.363297, zoom = 2) %>%
        addCircleMarkers(data = s, lng = ~lon, lat = ~lat, group = "Points",
                         radius = ~scale_area(sum, val_min = 0),
                         fillOpacity = 0.7,
                         fillColor = "orange",
                         popup = ~htmltools::htmlEscape(round(sum, 0)))
  })

  ## Reset map on Reset Button
  observeEvent(input$data_reset, {
    req(!is.null(db))
    if(verbose) cat("Reset map\n")
    leafletProxy(ns("map_data")) %>%
      clearGroup(group = "Points") %>%
      clearGroup(group = "Sites") %>%
      setView(lng = -98.857903, lat = 21.363297, zoom = 2) %>%
      addMarkers(data = sites_all,
                 lng = ~lon, lat = ~lat,
                 popup  = htmltools::htmlEscape(sites_all$site_name),
                 group = "Sites") %>%
      addCircleMarkers(data = suppressWarnings(get_counts(counts, summarize_by = "site_name") %>%
                                                 dplyr::left_join(sites_all, by = c("choices" = "site_name"))),
                       lng = ~lon, lat = ~lat,
                       group = "Points",
                       radius = ~scale_area(sum, val_min = 0),
                       fillOpacity = 0.7,
                       fillColor = "orange",
                       popup = ~htmltools::htmlEscape(round(sum, 0)))
  })


  # Update map logger sites automatically on site selection
  observeEvent(input$data_site_name, {
    req(!is.null(db), input$data_site_name != "")
    if(verbose) cat("Updating markers...\n")
    f <- loggers_all[loggers_all$site_name == input$data_site_name, ]
    if(nrow(f) > 0) {
      if(unique(f$site_name) == "Kamloops, BC") zoom <- 17
      if(unique(f$site_name) == "Costa Rica") zoom <- 12
      leafletProxy(ns("map_data")) %>%
        clearGroup(group = "Sites") %>%
        addMarkers(data = f, lng = ~lon, lat = ~lat, group = "Sites", popup = ~htmltools::htmlEscape(logger_id)) %>%
        setView(lat = mean(f$lat, na.rm = TRUE), lng = mean(f$lon, na.rm = TRUE), zoom = zoom, options = list('animate' = TRUE))
    }
  }, priority = 50)

  # Add circle markers for sample sizes
  observe({

    ## Watch for changes in either of these
    input$map_update
    input$data_site_name

    isolate({
      req(input$data_site_name != "")
      values$data_map <- data_selection()  ## Keep track of current map values
      c <- data_selection()

      if(verbose) cat("Refreshing Map...\n")
      if(nrow(c) > 0) {
        #Get counts summed across all dates
        if(length(unique(c$site_name)) > 1) {
          suppressWarnings({
            s <- get_counts(c, summarize_by = "site_name") %>%
              dplyr::left_join(sites_all, by = c("choices" = "site_name"))
          })
        } else {
          suppressWarnings(
            s <- get_counts(c, summarize_by = "logger_id") %>%
              dplyr::left_join(loggers_all, by = c("choices" = "logger_id"))
          )
        }
        s <- s[s$sum > 0, ]
        leafletProxy(ns("map_data")) %>%
          clearGroup(group = "Points") %>%
          addCircleMarkers(data = s, lng = ~lon, lat = ~lat, group = "Points",
                           radius = ~scale_area(sum, val_min = 0),
                           fillOpacity = 0.7,
                           fillColor = "orange")
      } else {
        leafletProxy(ns("map_data")) %>%
          clearGroup(group = "Points")
      }
    })
  }, priority = 50)


  ## Time plot ----------------------------------------------------

  ## Plot of counts overtime
  plot_data_ggplot <- reactive({
    req(input_selection())
    if(verbose) cat("Refreshing Time Plot...\n")

    isolate({

      i <- input_selection()

      date <- c(min(i$date), max(i$date))
      total <- counts_site() %>%
        dplyr::mutate(selected = factor("unselected", levels = c("unselected", "selected")),
                      selected = replace(selected,
                                         species %in% i$species &
                                           date %within% interval(as.Date(i$date[1]), as.Date(i$date[2])) &
                                           animal_id %in% i$animal_id &
                                           logger_id %in% i$logger_id,
                                         "selected")) %>%
        dplyr::group_by(species, date, selected) %>%
        dplyr::summarize(count = sum(count))


      if(nrow(total) > 0) {
        g <- ggplot2::ggplot(data = total, ggplot2::aes(x = date, y = count, fill = species, alpha = selected)) +
          ggplot2::annotate(geom = "ribbon",
                            ymin = -Inf, ymax = +Inf,
                            x = seq(date[1], date[2], by = "1 day"),
                            fill = "grey", alpha = 0.4) +
          ggplot2::geom_bar(stat = "identity")
      } else {
        g <- ggplot2::ggplot()
      }
      g <- g +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "top") +
        ggplot2::scale_y_continuous(expand = c(0,0)) +
        ggplot2::scale_x_date(expand = c(0,0), date_labels = "%Y %b %d") +
        ggplot2::scale_alpha_manual(values = c(0.4, 1), drop = FALSE, guide = FALSE) +
        ggplot2::labs(x = "Date", y = "No. RFID Reads")
    })
    return(g)
  })

  ## Output plot
  output$plot_data_ggplot <- renderPlot({
    freezeReactiveValue(input, "plot_data_brush")
    plot_data_ggplot()
  }, height = 200)

  ## For time plot tooltip
  output$text_time <- renderText({
    req(input_selection())
    "Drag and select a date range to further refine the data selection"
  })

  # Help ----------------------------------------------------
  observeEvent(input$help_data, {
    showModal(modalDialog(size = "m",
                          title = "Data selection",
                          easyClose = TRUE,
                          tagList(h4("Site selection"),
                                  tags$ul(
                                    tags$li("First select a site by which to filter the data."),
                                    tags$li("You may only select data from one site at a time.")),
                                  h4("Dates"),
                                  tags$ul(
                                    tags$li("Next, select date range with slider bar, OR"),
                                    tags$li("By clicking and dragging over the time plot"),
                                    tags$ul(tags$li("The time plot actively updates to show the current data selection (dark colours are selected, pale are not)"))),
                                  h4("Species"),
                                  tags$ul(
                                    tags$li("Refine your selection by choosing which species to include/exclude"),
                                    tags$li("The selection will update depending on your other selections (i.e. if you select a date range with no visits by a particular species, that species will be deselected"),
                                    tags$li("Numbers in brackets reflect the total number of RFID reads per species.")),
                                  h4("Advanced Options"),
                                  tags$ul(
                                    tags$li("Further refine your selection by choosing which animal_ids or logger_ids to include/exclude"),
                                    tags$li("The selection of available ids will update depending on your other selections (i.e. if you deselect a species, all animal_ids associated with that species will disappear"),
                                    tags$li("Numbers in brackets reflect the total number of RFID reads per id.")),
                                  strong("Note:"), "If you find you have 0 observations selected, try broadening your time range."
                          )
    ))
  })

  observeEvent(input$help_selected, {
    showModal(modalDialog(size = "m",
                          title = "Selected data",
                          easyClose = TRUE,
                          tagList(h4("Data Access:"),
                                  "Some data in the Database is restricted to visualizations only to protect the hard work of scientists until they've had a chance to publish their findings.",
                                  tags$ul(
                                    tags$li(strong("Fully Public:"), "Users may visualize and download the data"),
                                    tags$li(strong("Visualizations Only:"), "Users may visualize but not download the data")),
                                  h4("Selected Data Table"),
                                  tags$ul(tags$li("Current number of reads per species in the selected data")),
                                  h4("Get Data"),
                                  tags$ul(tags$li("Load the selected data set")),
                                  h4("Reset Data"),
                                  tags$ul(tags$li("Resets data selection to default"))
                          )))
  })

  observeEvent(input$help_map, {
    showModal(modalDialog(size = "m",
                          title = "Map of data available",
                          easyClose = TRUE,
                          tagList(tags$ul(
                            tags$li("Circle area depicts the amount of visits recorded per site or logger given the options selected"),
                            tags$li("Update Map button can be used to activily update the map of selected data"))
                          )))
  })

  observeEvent(input$help_adv, {
    showModal(modalDialog(size = "m",
                          title = "Advanced Options",
                          easyClose = TRUE,
                          tagList(tags$ul(
                            tags$li("Select specific animal ids or logger ids"),
                            tags$li("Numbers in brackets indicate the total number of reads in the database for each individual or logger"))
                          )))
  })

  # Return ----------------------------------------------------
  return(c(r = reactive({values$data}),
           time = reactive({if(is.null(values$data)) NULL else Sys.time()}),
           name = reactive({values$data$site_name[1]})))
}
