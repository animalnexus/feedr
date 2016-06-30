####################
## Reactive data selections
###################

## Subset of counts reflecting site
counts_site <- reactive({
  req(input$data_site_name)
  droplevels(counts[counts$site_name == input$data_site_name, ])
})

## Subset of counts reflecting species (FOR ADVANCED ONLY)
counts_species <- reactive({
  req(input$data_species)
  droplevels(counts_site()[counts_site()$species %in% input$data_species, ])
})

## Subset of counts reflecting ALL selections
counts_sub <- reactive({
  cat("Calculating counts_sub()...\n")
  get_counts(counts_site(), filter = values$input)
})

####################
## Reset all with reset button
####################
observeEvent(input$data_reset, {
  cat("Reset data selection...\n")
  updateRadioButtons(session, "data_site_name",
                     choices = choices(counts_sum, "site_name"),
                     selected = character(0))
  values$input <- NULL
})

####################
## Reset values with site selection
####################
observeEvent(input$data_site_name, {
  req(counts_site())
  values$input <- values_list(counts_site())
  values$input_previous <- values$input
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

## Initial UI for Site_name
output$UI_data_site_name <- renderUI({
  #Site name
  radioButtons("data_site_name", "Sites:",
               choices = choices(counts_sum, "site_name"),
               selected = character(0))
})

## Initial UI for species, date
output$UI_data <- renderUI({
  cat("Initializing data selection UI...\n")
  req(input$data_site_name, counts_site())

  cnts <- droplevels(counts_site()) %>%
    do(bind_rows(get_counts(c = ., summarize_by = "species"),
                 get_counts(c = ., summarize_by = "date")))
  list(
    #Species
    checkboxGroupInput("data_species", "Species to include:",
                       choices = choices(cnts, "species"),
                       selected = selected(cnts, "species")),
    # Date Range
    dateRangeInput("data_date", "Dates to include:",
                   min = min(as.Date(cnts$choices[cnts$variable == "date"])),
                   max = max(as.Date(cnts$choices[cnts$variable == "date"])),
                   start = min(as.Date(cnts$choices[cnts$variable == "date"])),
                   end = max(as.Date(cnts$choices[cnts$variable == "date"])))
  )
})

## Toggle advanced options
observeEvent(input$data_showadv, {
  toggle(id = "advanced")
})

## Advanced UI
output$UI_data_adv <- renderUI({
  req(input$data_site_name, counts_site())

  cnts <- rbind(get_counts(c = counts_site(), summarize_by = "bird_id"),
                get_counts(c = counts_site(), summarize_by = "feeder_id"))

  list(
    checkboxGroupInput("data_bird_id", "Select bird(s)",
                       choices = choices(cnts, "bird_id"),
                       selected = selected(cnts, "bird_id")),
    checkboxGroupInput("data_feeder_id", "Select feeder(s)",
                       choices = choices(cnts, "feeder_id"),
                       selected = selected(cnts, "feeder_id")))
})
outputOptions(output, 'UI_data_adv', suspendWhenHidden=FALSE)


####################
## Update UIs
####################

## Update date/time with plot selection
observeEvent(input$plot_data_brush, {
  req(input$plot_data_brush)
  new_dates <- c(as.Date(input$plot_data_brush$xmin, lubridate::origin),
                 as.Date(input$plot_data_brush$xmax, lubridate::origin))
  if(new_dates[1] < min(counts$date)) new_dates[1] <- min(counts$date)
  if(new_dates[2] > max(counts$date)) new_dates[2] <- max(counts$date)
  updateDateRangeInput(session, "data_date", start = new_dates[1], end = new_dates[2])
})


## If update or get data buttons are pressed, reset UI and redo submitted values
observeEvent(input$data_update, {
  req(startup(input))
  cat("Update values\n")

  ## Actual input values
  values$input_previous <- values$input # Save old
  values$input <- list('species' = input$data_species,
                       'date' = input$data_date,
                       'feeder_id' = input$data_feeder_id,
                       'bird_id' = input$data_bird_id) # Save new
  values$updateUI <- TRUE
})

## Update advanced UI
observeEvent(counts_species(), {
  cnts <- rbind(get_counts(c = counts_species(), summarize_by = "bird_id"),
                get_counts(c = counts_species(), summarize_by = "feeder_id"))

  list(updateCheckboxGroupInput(session, "data_bird_id",
                                choices = choices(cnts, "bird_id"),
                                selected = selected(cnts, "bird_id")),
       updateCheckboxGroupInput(session, "data_feeder_id",
                                choices = choices(cnts, "feeder_id"),
                                selected = selected(cnts, "feeder_id"))
  )
})

## Update UI
observeEvent(values$updateUI, {
  req(startup(input), values$updateUI == TRUE)
  cat("Update data UI...\n")

  browser()
  #Save values
  v <- counts_sub() ## Get new counts
  ## Get options that were 're-checked'
  if(!is.logical(all.equal(values$input, values$input_previous))){
    added <- list('species' = setdiff(values$input$species, values$input_previous$species),
                  'bird_id' = setdiff(values$input$bird_id, values$input_previous$bird_id),
                  'feeder_id' = setdiff(values$input$feeder_id, values$input_previous$feeder_id))
    added <- added[!sapply(added, is.null)]

    ## Add re-checked items back in
    for(i in 1:length(added)) v <- unique(rbind(v, get_counts(c = counts_site(), filter = added[i])))
  }

  ## Get new totals for UI
  sum <- rbind(
    get_counts(c = v, summarize_by = "species"),
    get_counts(c = v, summarize_by = "date"),
    get_counts(c = v, summarize_by = "bird_id"),
    get_counts(c = v, summarize_by = "feeder_id"))

  # Update values directly for mapping etc.
  values$input <- values_list(sum)

  values$updateUI <- FALSE

  # Update UIs to match
  list(
    updateCheckboxGroupInput(session, "data_species",
                             choices = choices(sum, "species"),
                             selected = selected(sum, "species")),
    updateDateRangeInput(session, "data_date",
                         start = min(as.Date(sum$choices[sum$variable == "date"])),
                         end = max(as.Date(sum$choices[sum$variable == "date"])))
  )
})



####################
## Get DATA
####################

## Download Selected Data
data <- eventReactive(input$data_get, {
  req(counts_sub())
  cat("Downloading selected data...\n")

  con <- dbConnect(drv,host=dbhost,port=dbport,dbname=dbname,user=dbuser,password=dbpass)

  d <- counts_sub()


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
feeders_sub <- reactive({
  data() %>%
    dplyr::select(feeder_id, site_name, lon, lat) %>%
    unique(.)
})

## Birds of current data
birds_sub <- reactive({
  data() %>%
    dplyr::select(bird_id, species, age, sex, tagged_on, site_name) %>%
    unique(.)
})
