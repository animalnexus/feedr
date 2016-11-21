


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
                 popify(uiOutput(ns("UI_data_site_name")),
                           title = "Site name", content = "Filter by site",
                           placement = "right", options = list(container = "body", offset = c(0,0))),
                 popify(uiOutput(ns("UI_data_species")),
                           title = "Species", content = "Species to include/exclude",
                           placement = "right", options = list(container = "body")),
                 popify(uiOutput(ns("UI_data_date")),
                           title = "Dates", content = "Filter by date range",
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
                 shinyjs::disabled(actionButton(ns("data_reset"), "Reset inputs"))
             )
      ),
      column(8,
             div(shinyjs::disabled(actionButton(ns("map_update"), "Update map")), style = "text-align: center"),
             p(),
             leafletOutput(ns("map_data"), height = 600),
             bsPopover(ns("map_data"), "Sampling", "Circle area depicts the amount of visits recorded per site or feeder given the options selected", placement = "top", trigger = "hover"),
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
      hr(),
      h3("Advanced Options"),
      #actionButton(ns("pause"), "Pause"),
      actionButton(ns("showadv"), "Show Advanced Options"),
      shinyjs::hidden(div(id = "advanced",
                          uiOutput(ns("UI_data_bird_id")),
                          uiOutput(ns("UI_data_feeder_id"))))
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

      setProgress(value = 0.15, detail = "Getting feeder data..")
      cat("Getting feeder data...\n")
      #   incProgress(1/5)
      feeders_all <- dbGetQuery(con, statement = paste("SELECT feeders.feeder_id, feeders.site_name, feeders.loc, fieldsites.dataaccess",
                                                       "FROM feeders, fieldsites",
                                                       "WHERE (fieldsites.site_name = feeders.site_name)")) %>%
        load_format(tz = "") %>%
        dplyr::mutate(site_name = factor(site_name))

      setProgress(value = 0.30, detail = "Getting site data..")
      cat("Getting site data...\n")
      #    incProgress(2/5)
      sites_all <- feeders_all %>%
        dplyr::group_by(site_name) %>%
        dplyr::summarize(lon = mean(lon), lat = mean(lat), dataaccess = unique(dataaccess)) %>%
        dplyr::mutate(site_name = factor(site_name))

      setProgress(value = 0.45, detail = "Getting bird data..")
      cat("Getting bird data...\n")
      #  incProgress(3/5)
      birds_all <- dbGetQuery(con, statement = paste0("SELECT DISTINCT raw.visits.bird_id FROM raw.visits")) %>%
        dplyr::left_join(dbGetQuery(con, statement = paste0("SELECT bird_id, species, site_name, age, sex, tagged_on FROM birds")), by = "bird_id") %>%
        load_format(tz = "") %>%
        dplyr::mutate(species = factor(species),
               site_name = factor(site_name),
               bird_id = factor(bird_id))

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
        dplyr::left_join(birds_all[, c("site_name", "species", "bird_id")], by = "bird_id") %>%
        dplyr::mutate(date = as.Date(date),
               count = as.numeric(count),
               species = factor(species, levels = sort(unique(birds_all$species))),
               site_name = factor(site_name, levels = sort(sites_all$site_name)),
               bird_id = factor(bird_id, levels = sort(unique(birds_all$bird_id))),
               feeder_id = factor(feeder_id, levels = sort(unique(feeders_all$feeder_id))))
      dbDisconnect(con)
    })


    #  incProgress(5/5)
    setProgress(value = 0.75, detail = "Summarizing samples..")
    cat("Summarizing samples...\n")
    counts_sum <- dplyr::bind_rows(
      get_counts(counts, summarize_by = "site_name"),
      get_counts(counts, summarize_by = "species"),
      get_counts(counts, summarize_by = "date"),
      get_counts(counts, summarize_by = "bird_id"),
      get_counts(counts, summarize_by = "feeder_id"))

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
    #browser()
    ## Invalidate only on the following inputs
    input$data_species
    input$data_date
    input$plot_data_brush
    input$data_bird_id
    input$data_feeder_id

    isolate({
      values$input_previous <- values$input  ## Save old inputs
      values$input <- values_list(input, counts)     ## Get new inputs

      if(!is.logical(all.equal(values$input, values$input_previous))) {
        values$keep <- get_counts(counts_site(), filter = values_list(input, counts))

        ## Added to current selection:
        added <- list(
          "species" = setdiff(values$input$species, values$input_previous$species),
          "bird_id" = setdiff(values$input$bird_id, values$input_previous$bird_id),
          "feeder_id" = setdiff(values$input$feeder_id, values$input_previous$feeder_id))
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
    #browser()
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


  ## UI bird_id
  output$UI_data_bird_id <- renderUI({
    req(counts_species())
    #browser()
    cnts <- get_counts(c = counts_species(), summarize_by = "bird_id")
    checkboxGroupInput(ns("data_bird_id"), "Select bird ids",
                       choices = choices(cnts, "bird_id"),
                       selected = selected(cnts, "bird_id"), inline = TRUE)
  })



  ## UI feeder_id
  output$UI_data_feeder_id <- renderUI({
    req(counts_species())
    cnts <- get_counts(c = counts_species(), summarize_by = "feeder_id")
    checkboxGroupInput(ns("data_feeder_id"), "Select feeder ids",
                       choices = choices(cnts, "feeder_id"),
                       selected = selected(cnts, "feeder_id"), inline = TRUE)
  })

  ## Toggle advanced options
  observeEvent(input$showadv, {
    shinyjs::toggle(id = "advanced")
  })

  ## Render UIs even when hidden
  outputOptions(output, 'UI_data_species', suspendWhenHidden = FALSE)
  outputOptions(output, 'UI_data_date', suspendWhenHidden = FALSE)
  outputOptions(output, 'UI_data_bird_id', suspendWhenHidden = FALSE)
  outputOptions(output, 'UI_data_feeder_id', suspendWhenHidden = FALSE)

  ####################
  ## Get DATA
  ####################

  ## Download Selected Data
  raw <- eventReactive(input$data_get, {
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
                                                         "WHERE raw.visits.bird_id IN ( '", paste0(unique(d$bird_id), collapse = "', '"), "' ) ",
                                                         dates
                                      ))
                 )
    )
    dbDisconnect(con)

    if(nrow(data) > 0) {
      cat("Formatting selected data...")
      data <- data %>%
        load_format(., tz = "") %>%
        dplyr::mutate(bird_id = factor(bird_id, levels = sort(unique(birds_all$bird_id))),
               feeder_id = factor(feeder_id, levels = sort(unique(feeders_all$feeder_id)))) %>%
        dplyr::left_join(birds_all, by = c("bird_id")) %>%
        dplyr::left_join(feeders_all, by = c("feeder_id", "site_name"))
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

    #browser()
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
                         radius = ~feedr:::scale_area(sum, val_min = 0),
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
                       radius = ~feedr:::scale_area(sum, val_min = 0),
                       fillOpacity = 0.7,
                       fillColor = "orange")
  })


  # Update map feeder sites automatically on site selection
  observeEvent(input$data_site_name, {
    req(!is.null(db), input$data_site_name != "")
    cat("Updating markers...\n")
    #browser()
    f <- feeders_all[feeders_all$site_name == input$data_site_name, ]
    if(nrow(f) > 0) {
      if(unique(f$site_name) == "Kamloops, BC") zoom <- 17
      if(unique(f$site_name) == "Costa Rica") zoom <- 12
      leafletProxy(ns("map_data")) %>%
        clearGroup(group = "Sites") %>%
        addMarkers(data = f, lng = ~lon, lat = ~lat, group = "Sites", popup = ~htmltools::htmlEscape(feeder_id)) %>%
        setView(lat = mean(f$lat, na.rm = TRUE), lng = mean(f$lon, na.rm = TRUE), zoom = zoom, options = list('animate' = TRUE))
    }
  }, priority = 50)

  # Add circle markers for sample sizes
  observe({
    req(startup(input), !is.null(db), input$data_site_name != "")

    ## Watch for changes in either of these
    input$map_update
    input$data_site_name

    isolate({
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
            s <- get_counts(c, summarize_by = "feeder_id") %>%
              dplyr::left_join(feeders_all, by = c("choices" = "feeder_id"))
          )
        }
        s <- s[s$sum > 0, ]
        leafletProxy(ns("map_data")) %>%
          clearGroup(group = "Points") %>%
          addCircleMarkers(data = s, lng = ~lon, lat = ~lat, group = "Points",
                           radius = ~feedr:::scale_area(sum, val_min = 0),
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
    #req(startup(input), !is.null(db), input$data_bird_id, input$data_feeder_id)
    req(startup(input), !is.null(db), values$input)
    cat("Refreshing Time Plot...\n")

    i <- values$input

    isolate({
      total <- counts_site() %>%
        dplyr::mutate(selected = factor("unselected", levels = c("unselected", "selected")),
                      selected = replace(selected,
                                         species %in% i$species &
                                           date %within% interval(as.Date(i$date[1]), as.Date(i$date[2])) &
                                           bird_id %in% i$bird_id &
                                           feeder_id %in% i$feeder_id,
                                         "selected")) %>%
        dplyr::group_by(species, date, selected) %>%
        dplyr::summarize(count = sum(count))

      if(nrow(total) > 0) {
        g <- ggplot2::ggplot(data = total, ggplot2::aes(x = date, y = count, fill = species, alpha = selected)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::scale_alpha_manual(values = c(0.1, 1), drop = FALSE, guide = FALSE)
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

  data <- reactive({
    req(raw())
    list(data = raw(), time = Sys.time(), name = raw()$site_name[1])
  })

  return(data)
}
