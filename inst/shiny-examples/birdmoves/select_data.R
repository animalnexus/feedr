####################
## Reactive data selections
###################

# Fires when selection complete
observe({
  req(startup(input), db_access)

  ## Invalidate only on the following inputs
  input$data_species
  input$data_date
  input$plot_data_brush
  input$data_bird_id
  input$data_feeder_id

  #browser()

  isolate({
    values$input_previous <- values$input  ## Save old inputs
    values$input <- values_list(input)     ## Get new inputs

    if(!is.logical(all.equal(values$input, values$input_previous))) {
      values$keep <- get_counts(counts_site(), filter = values_list(input))

      ## Added to current selection:
      added <- list(
        "species" = setdiff(values$input$species, values$input_previous$species),
        "bird_id" = setdiff(values$input$bird_id, values$input_previous$bird_id),
        "feeder_id" = setdiff(values$input$feeder_id, values$input_previous$feeder_id))
      added <- added[sapply(added, length) > 0]

      ## Force added back in:
      if(length(added) > 0) values$keep <- unique(rbind(values$keep, get_counts(counts_site(), filter = added)))

      #browser()

      ## Reset all values
      #values$input <- values$input_previous <- values_list(input)

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
})

## Subset of counts reflecting site
counts_site <- reactive({
  req(input$data_site_name)
  droplevels(counts[counts$site_name == input$data_site_name, ])
})

## Subset of counts reflecting species (FOR ADVANCED ONLY)
counts_species <- reactive({
  req(input$data_species)
  cat("Calculating counts_species()...\n")
  droplevels(counts_site()[counts_site()$species %in% input$data_species, ])
})

## Table showing current selection
output$data_selection <- renderTable({
  req(startup(input), db_access)
  get_counts(values$keep, summarize_by = "species") %>%
    select("Species" = choices, "Total" = sum)
}, digits = 0, include.rownames = FALSE)

output$data_access <- renderText({
  req(startup(input), db_access, input$data_site_name != "")
  if(sites_all$dataaccess[sites_all$site_name == input$data_site_name] == 0) return("Fully Public")
  if(sites_all$dataaccess[sites_all$site_name == input$data_site_name] == 1) return("Visualizations Only")
})


## Subset of counts reflecting ALL selections
#counts_sub <- reactive({
#  req(startup(input), db_access)
#  cat("Calculating counts_sub()...\n")
#  get_counts(counts_species(), filter = values_list(input))
#})

####################
## Reset all with reset button
####################
observeEvent(input$data_reset, {
  req(startup(input), db_access)
  cat("Reset data selection...\n")
  values$input <- values$input_previous <- list()
  updateSelectInput(session, "data_site_name",
                    selected = c("Choose site" = ""))
})


observeEvent(input$data_pause, {
  browser()
})

####################
## Reset values with site selection
####################
observeEvent(input$data_site_name, {
  req(counts_site())

  values$input <- list()
  values$input_previous <- values$input

  values$keep <- counts_site()
})

####################
## Format buttons
####################

observe({
  req(startup(input), counts_site())
  if(!is.logical(all.equal(values_list(input), values$input))) {
    updateButton(session, "data_get", disabled = TRUE)
  } else {
    updateButton(session, "data_get", disabled = FALSE)
  }
})

####################
## Output UIs
####################

## UI Site_name
output$UI_data_site_name <- renderUI({
  req(db_access)
  selectInput("data_site_name", "Sites:",
              choices = c(c("Choose site" = ""), choices(counts_sum, "site_name")))
})

## UI Species
output$UI_data_species <- renderUI({
  req(input$data_site_name)
  #if(is.null(input$data_species)) c <- counts_site() else c <- values$keep
  cnts <- get_counts(c = counts_site(), summarize_by = "species")
  #sel <- get_counts(c = values$keep, summarize_by = "species")
  checkboxGroupInput("data_species", "Species",
                     choices = choices(cnts, "species"),
                     selected = selected(cnts, "species"))
})

## UI Date range
output$UI_data_date <- renderUI({
  req(input$data_site_name)
  #if(is.null(input$data_date) | nrow(values$keep) == 0) c <- counts_site() else c <- values$keep
  c <- counts_site()
  cnts <- get_counts(c = c, summarize_by = "date")
  dateRangeInput("data_date", "Dates to include:",
                 min = min(as.Date(cnts$choices[cnts$variable == "date"])),
                 max = max(as.Date(cnts$choices[cnts$variable == "date"])),
                 start = min(as.Date(cnts$choices[cnts$variable == "date"])),
                 end = max(as.Date(cnts$choices[cnts$variable == "date"])))
})


## UI bird_id
output$UI_data_bird_id <- renderUI({
  req(counts_species())
  cnts <- get_counts(c = counts_species(), summarize_by = "bird_id")
  checkboxGroupInput("data_bird_id", "Select bird ids",
                     choices = choices(cnts, "bird_id"),
                     selected = selected(cnts, "bird_id"))
})



## UI feeder_id
output$UI_data_feeder_id <- renderUI({

  req(counts_species())

  cnts <- get_counts(c = counts_species(), summarize_by = "feeder_id")
  checkboxGroupInput("data_feeder_id", "Select feeder ids",
                     choices = choices(cnts, "feeder_id"),
                     selected = selected(cnts, "feeder_id"))
})

## Toggle advanced options
observeEvent(input$data_showadv, {
  toggle(id = "advanced")
})

## Render UIs even when hidden
outputOptions(output, 'UI_data_species', suspendWhenHidden=FALSE)
outputOptions(output, 'UI_data_date', suspendWhenHidden=FALSE)
outputOptions(output, 'UI_data_bird_id', suspendWhenHidden=FALSE)
outputOptions(output, 'UI_data_feeder_id', suspendWhenHidden=FALSE)


####################
## Update UIs
####################

## Update date/time with plot selection
#observeEvent(input$plot_data_brush, {
#  req(input$plot_data_brush)
#  browser()
#  new_dates <- c(as.Date(input$plot_data_brush$xmin, lubridate::origin),
#                 as.Date(input$plot_data_brush$xmax, lubridate::origin))
#
#  ## Can't select less than available
#  if(new_dates[1] < min(counts$date)) new_dates[1] <- min(counts$date)
#  if(new_dates[2] > max(counts$date)) new_dates[2] <- max(counts$date)
#  updateDateRangeInput(session, "data_date", start = new_dates[1], end = new_dates[2])
#})

####################
## Get DATA
####################

## Download Selected Data
raw <- eventReactive(input$data_get, {
  req(values$keep)
  cat("Downloading selected data...\n")

  con <- dbConnect(drv,host=dbhost,port=dbport,dbname=dbname,user=dbuser,password=dbpass)

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
    data <- data %>%
      load_format(., tz = "") %>%
      mutate(bird_id = factor(bird_id, levels = sort(unique(birds_all$bird_id))),
             feeder_id = factor(feeder_id, levels = sort(unique(feeders_all$feeder_id)))) %>%
      left_join(birds_all, by = c("bird_id")) %>%
      left_join(feeders_all, by = c("feeder_id", "site_name"))
  } else data <- NULL

  #Get weather data
  if(input$data_weather == "Yes" & any(unique(data$site_name) == "Kamloops, BC")){
    withProgress(message = "Adding Weather Data...",
                 w <- weather(station_id = 51423, start = min(as.Date(data$time)), end = max(as.Date(data$time)), timeframe = "hour") %>%
                   mutate(hour = format(time, "%Y-%m-%d %H"))
    )
    data <- data %>%
      mutate(hour = format(time, "%Y-%m-%d %H")) %>%
      left_join(w[, c("hour", "temp", "temp_dew", "rel_hum", "hmdx", "pressure", "visib", "wind_chill", "wind_dir", "wind_spd")], by = "hour")
  }

  data
})

## Feeders of current data
feeders <- reactive({
  raw() %>%
    dplyr::select(feeder_id, site_name, lon, lat) %>%
    unique(.)
})

## Birds of current data
birds <- reactive({
  raw() %>%
    dplyr::select(bird_id, species, age, sex, tagged_on, site_name) %>%
    unique(.)
})
