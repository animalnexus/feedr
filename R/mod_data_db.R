


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
             h3("Select Data"),
             div(id = "selection",
                 tipify(uiOutput(ns("UI_data_site_name")),
                           title = "Filter by site",
                           placement = "right", options = list(container = "body")),
                 tipify(uiOutput(ns("UI_data_species")),
                           title = "Species to include/exclude",
                           placement = "right", options = list(container = "body")),
                 tipify(uiOutput(ns("UI_data_date")),
                           title = "Filter by date range",
                           placement = "right", options = list(container = "body")),
                 hr(),
                 h3("Selected Data"),
                 strong("Data Access: "), textOutput(ns("data_access"), inline = TRUE),
                 tableOutput(ns("data_selection")),
                 bsTooltip(ns("data_selection"), "Number of visits per species given the options selected.",
                           "right", options = list(container = "body")),
                 p(),
                 shinyjs::disabled(actionButton(ns("data_get"), "Get Data", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                 p(),
                 shinyjs::disabled(actionButton(ns("data_reset"), "Reset inputs")),
                 hr()
                 #actionButton(ns("showadv"), "Show Advanced Options")
             )
      ),
      column(8,
             div(shinyjs::disabled(actionButton(ns("map_update"), "Update map")), style = "text-align: center"),
             p(),
             leafletOutput(ns("map_data"), height = 600),
             bsTooltip(ns("map_data"), "Circle area depicts the amount of visits recorded per site or logger given the options selected", placement = "top"),
             plotOutput(ns("plot_data_ggplot"),
                        brush = brushOpts(
                          id = ns("plot_data_brush"),
                          direction = "x",
                          resetOnNew = TRUE), height = "100%"),
             div(strong(textOutput(ns("text_time"))), style = "text-align: center;")
             #ggvisOutput("plot_data_ggvis")
      )
    ),
    fluidRow(
      #actionButton(ns("pause"), "Pause"),
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
mod_data_db <- function(input, output, session, db) {

  ns <- session$ns

  if(!is.null(db) && !curl::has_internet()) db <- NULL

  ## Get data base details
  if(!is.null(db)) {

    withProgress(message = "Loading...", detail = "Connecting to server...", value = 0, {
    suppressWarnings({
      cat("Connecting to server...\n")
      con <- dbConnect(dbDriver("PostgreSQL"), host = db$host, port = db$port, dbname = db$name, user = db$user, password = db$pass)

      setProgress(value = 0.15, detail = "Getting logger data..")
      cat("Getting logger data...\n")
      #   incProgress(1/5)
      loggers_all <- dbGetQuery(con, statement = paste("SELECT feeders.feeder_id, feeders.site_name, feeders.loc, fieldsites.dataaccess",
                                                       "FROM feeders, fieldsites",
                                                       "WHERE (fieldsites.site_name = feeders.site_name)")) %>%
        load_format(tz = "") %>%
        dplyr::mutate(site_name = factor(site_name))

      setProgress(value = 0.30, detail = "Getting site data..")
      cat("Getting site data...\n")
      #    incProgress(2/5)
      sites_all <- loggers_all %>%
        dplyr::group_by(site_name) %>%
        dplyr::summarize(lon = mean(lon), lat = mean(lat), dataaccess = unique(dataaccess)) %>%
        dplyr::mutate(site_name = factor(site_name))

      setProgress(value = 0.45, detail = "Getting animal data..")
      cat("Getting animal data...\n")
      #  incProgress(3/5)

      animals_all <- dbGetQuery(con, statement = paste("SELECT bird_id, species, site_name, age, sex, tagged_on FROM birds",
                                                      "WHERE birds.species NOT IN ('XXXX')")) %>%
        load_format(tz = "") %>%
        dplyr::mutate(species = factor(species),
                      site_name = factor(site_name),
                      animal_id = factor(animal_id))

      setProgress(value = 0.60, detail = "Getting sample information..")
      cat("Getting sample information...\n")
      #    incProgress(4/5)
      counts <- dbGetQuery(con,
                           statement = paste0("SELECT raw.visits.bird_id, raw.visits.feeder_id, DATE(raw.visits.time), ",
                                              "COUNT(*) ",
                                              "FROM raw.visits ",
                                              "GROUP BY DATE(raw.visits.time), raw.visits.feeder_id, raw.visits.bird_id"#,
                           )) %>%
        load_format(tz = "UTC") %>%
        dplyr::inner_join(animals_all[, c("site_name", "species", "animal_id")], by = "animal_id") %>%
        dplyr::mutate(date = as.Date(date),
               count = as.numeric(count),
               species = factor(species, levels = sort(unique(animals_all$species))),
               site_name = factor(site_name, levels = sort(sites_all$site_name)),
               animal_id = factor(animal_id, levels = sort(unique(animals_all$animal_id))),
               logger_id = factor(logger_id, levels = sort(unique(loggers_all$logger_id))))

      dbDisconnect(con)
    })


    #  incProgress(5/5)
    setProgress(value = 0.75, detail = "Summarizing samples..")
    cat("Summarizing samples...\n")
    counts_sum <- dplyr::bind_rows(
      get_counts(counts, summarize_by = "site_name"),
      get_counts(counts, summarize_by = "species"),
      get_counts(counts, summarize_by = "date"),
      get_counts(counts, summarize_by = "animal_id"),
      get_counts(counts, summarize_by = "logger_id"))

    })
  }

  values <- reactiveValues(
    data_map = NULL,          # Stores values which displayed on map
    keep = NULL,              # Stores data selected for download
    input = NULL,           # Stores selection options
    input_previous = NULL)   # Stores previous selection options (for comparison)


  ## UPDATE SELECTION
  # Fires when selection complete
  observe({
    req(startup(input), !is.null(db))
    ## Invalidate only on the following inputs
    input$data_species
    input$data_date
    input$plot_data_brush
    input$data_animal_id
    input$data_logger_id

    isolate({
      values$input_previous <- values$input  ## Save old inputs
      values$input <- values_list(input, counts)     ## Get new inputs

      if(!is.logical(all.equal(values$input, values$input_previous))) {
        values$keep <- get_counts(counts_site(), filter = values_list(input, counts))

        ## Added to current selection:
        added <- list(
          "species" = setdiff(values$input$species, values$input_previous$species),
          "animal_id" = setdiff(values$input$animal_id, values$input_previous$animal_id),
          "logger_id" = setdiff(values$input$logger_id, values$input_previous$logger_id))
        added <- added[sapply(added, length) > 0]

        ## Force added back in:
        if(length(added) > 0) values$keep <- unique(rbind(values$keep, get_counts(counts_site(), filter = added)))

        ## Update time (but only if needs to be updated)
        if(min(values$keep$date) != input$data_date[1] | max(values$keep$date) != input$data_date[2]){
          updateDateRangeInput(session, "data_date",
                               start = min(values$keep$date),
                               end = max(values$keep$date))
        }

        ## Uncheck species boxes if counts == 0 (But only if still checked)
        cnts <- get_counts(c = values$keep, summarize_by = "species")
        if(any(cnts$choices[cnts$sum == 0] %in% input$data_species)){
          updateCheckboxGroupInput(session, "data_species",
                                   selected = selected(cnts, "species"))
        }
      }
    })
  }, priority = 100)

  ## Subset of counts reflecting site
  counts_site <- reactive({
    req(input$data_site_name)
    droplevels(counts[counts$site_name == input$data_site_name, ])
  })

  ## Subset of counts reflecting species
  counts_species <- reactive({
    req(input$data_species, counts_site())
    cat("Calculating counts_species()...\n")
    if(is.null(values$input)) species <- unique(values$keep$species) else species <- input$data_species
    droplevels(counts_site()[counts_site()$species %in% species, ])
  })

  ## Table showing current selection
  output$data_selection <- renderTable({
    req(startup(input), !is.null(db), values$keep)
    get_counts(values$keep, summarize_by = "species") %>%
      dplyr::select("Species" = choices, "Total" = sum)
  }, digits = 0, include.rownames = FALSE)

  output$data_access <- renderText({
    req(startup(input), !is.null(db), input$data_site_name != "")
    if(sites_all$dataaccess[sites_all$site_name == input$data_site_name] == 0) return("Fully Public")
    if(sites_all$dataaccess[sites_all$site_name == input$data_site_name] == 1) return("Visualizations Only")
  })

  observeEvent(input$pause, {
    browser()
  })

  ####################
  ## Reset all with reset button
  ####################
  observeEvent(input$data_reset, {
    req(startup(input), !is.null(db))
    cat("Reset data selection...\n")
    values$input <- values$input_previous <- values$keep <- NULL
    updateSelectInput(session, "data_site_name",
                      selected = c("Choose site" = ""))
  })


  ####################
  ## Reset values with site selection
  ####################
  observeEvent(input$data_site_name, {
    req(counts_site())

    values$input <- values$input_previous <- NULL
    values$keep <- counts_site()
  }, priority = 100)

  ####################
  ## Format buttons
  ####################

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

  observeEvent(values$keep, {
    req(startup(input))
    if(isTRUE(all.equal(values$keep, values$data_map))) {
      shinyjs::disable(id = "map_update")
    } else {
      shinyjs::enable(id = "map_update")
    }
  }, priority = 25)

  ####################
  ## Output UIs
  ####################

  ## UI Site_name
  output$UI_data_site_name <- renderUI({
    req(!is.null(db))
    selectInput(ns("data_site_name"), "Sites:",
                choices = c(c("Choose site" = ""), choices(counts_sum, "site_name")))
  })

  ## UI Species
  output$UI_data_species <- renderUI({
    req(input$data_site_name)
    #if(is.null(input$data_species)) c <- counts_site() else c <- values$keep
    cnts <- get_counts(c = counts_site(), summarize_by = "species")
    #sel <- get_counts(c = values$keep, summarize_by = "species")
    checkboxGroupInput(ns("data_species"), "Species",
                       choices = choices(cnts, "species"),
                       selected = selected(cnts, "species"))
  })

  ## UI Date range
  output$UI_data_date <- renderUI({
    req(input$data_site_name)
    #if(is.null(input$data_date) | nrow(values$keep) == 0) c <- counts_site() else c <- values$keep
    c <- counts_site()
    cnts <- get_counts(c = c, summarize_by = "date")
    dateRangeInput(ns("data_date"), "Dates to include:",
                   min = min(as.Date(cnts$choices[cnts$variable == "date"])),
                   max = max(as.Date(cnts$choices[cnts$variable == "date"])),
                   start = min(as.Date(cnts$choices[cnts$variable == "date"])),
                   end = max(as.Date(cnts$choices[cnts$variable == "date"])))
  })


  ## UI animal_id
  output$UI_data_animal_id <- renderUI({
    req(counts_species())
    cnts <- get_counts(c = counts_species(), summarize_by = "animal_id")
    checkboxGroupInput(ns("data_animal_id"), "Select animal ids",
                       choices = choices(cnts, "animal_id"),
                       selected = selected(cnts, "animal_id"), inline = TRUE)
  })



  ## UI logger_id
  output$UI_data_logger_id <- renderUI({
    req(counts_species())
    cnts <- get_counts(c = counts_species(), summarize_by = "logger_id")
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

  ####################
  ## Get DATA
  ####################

  ## Download Selected Data
  r <- eventReactive(input$data_get, {
    req(values$keep, !is.null(db))
    cat("Downloading selected data...\n")

    con <- dbConnect(dbDriver("PostgreSQL"),host = db$host, port = db$port, dbname = db$name, user = db$user, password = db$pass)

    d <- values$keep


    dates <- c(min(d$date), max(d$date))
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
      cat("Formatting selected data...")
      data <- data %>%
        load_format(., tz = "") %>%
        dplyr::mutate(animal_id = factor(animal_id, levels = sort(unique(animals_all$animal_id))),
               logger_id = factor(logger_id, levels = sort(unique(loggers_all$logger_id)))) %>%
        dplyr::left_join(animals_all, by = c("animal_id")) %>%
        dplyr::left_join(loggers_all, by = c("logger_id", "site_name"))
    } else data <- NULL

    return(data)
  })

  ## Render Map
  output$map_data <- renderLeaflet({
    validate(need(!is.null(db), message = "No Database access. To work with local data, use the 'Import' tab. To work with the Database check out animalnexus.ca"))
    req(!is.null(db))
    cat("Initializing data map...\n")

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
                         fillColor = "orange")
  })

  ## Reset map on Reset Button
  observeEvent(input$data_reset, {
    req(!is.null(db))
    cat("Reset map")
    leafletProxy(ns("map_data")) %>%
      clearGroup(group = "Points") %>%
      clearGroup(group = "Sites") %>%
      setView(lng = -98.857903, lat = 21.363297, zoom = 2) %>%
      addMarkers(data = sites_all,
                 lng = ~lon, lat = ~lat,
                 popup  = htmltools::htmlEscape(sites_all$site_name),
                 group = "Sites") %>%
      addCircleMarkers(data = suppressWarnings(get_counts(counts, summarize_by = "site_name") %>% dplyr::left_join(sites_all, by = c("choices" = "site_name"))),
                       lng = ~lon, lat = ~lat,
                       group = "Points",
                       radius = ~scale_area(sum, val_min = 0),
                       fillOpacity = 0.7,
                       fillColor = "orange")
  })


  # Update map logger sites automatically on site selection
  observeEvent(input$data_site_name, {
    req(!is.null(db), input$data_site_name != "")
    cat("Updating markers...\n")
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
      req(startup(input), !is.null(db), input$data_site_name != "")
      values$data_map <- values$keep  ## Keep track of current map values
      c <- values$keep

      cat("Refreshing Map...\n")
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


  ## GGPLOT: Plot of counts overtime
  plot_data_ggplot <- reactive({
    #req(startup(input), !is.null(db), input$data_animal_id, input$data_logger_id)
    req(startup(input), !is.null(db), values$input)
    cat("Refreshing Time Plot...\n")

    i <- values$input

    isolate({
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
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::scale_alpha_manual(values = c(0.4, 1), drop = FALSE, guide = FALSE)
      }# else {
      #  g <- ggplot(data = data.frame(date = values$input$date, count = 0), aes(x = date, y = count)) +
      #    geom_blank() +
      #    scale_y_continuous(limits = c(0, 1))
      #}

      g <- g +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "top") +
        ggplot2::labs(x = "Date", y = "No. RFID Reads")
    })
    g
  })

  ## For data selection
  output$plot_data_ggplot <- renderPlot({
    plot_data_ggplot() +
      ggplot2::scale_x_date(date_labels = "%Y %b %d")
  }, height = 200)

  ## For time plot tooltip
  output$text_time <- renderText({
    req(startup(input), !is.null(db), values$input)
    "Drag and select a date range to further refine the data selection"
  })

  return(c(r = r,
           time = reactive({if(is.null(r())) NULL else Sys.time()}),
           name = reactive({r()$site_name[1]})))
}
