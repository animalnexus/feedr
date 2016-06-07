####################
## Output UIs
####################

observe({
  req(startup(input))
  if(!is.logical(all.equal(values_list(input), values$data))) {
    updateButton(session, "data_get", disabled = TRUE)
  } else {
    updateButton(session, "data_get", disabled = FALSE)
  }
})

## Reset UI
observeEvent(input$data_reset, {
  updateCheckboxGroupInput(session, "data_site_name",
                           choices = choices(counts_sum, "site_name"),
                           selected = selected(counts_sum, "site_name"))
  updateCheckboxGroupInput(session, "data_species",
                           choices = choices(counts_sum, "species"),
                           selected = selected(counts_sum, "species"))
  updateDateRangeInput(session, "data_date",
                       min = min(as.Date(counts_sum$choice[counts_sum$variable == "date"])),
                       max = max(as.Date(counts_sum$choice[counts_sum$variable == "date"])),
                       start = min(as.Date(counts_sum$choice[counts_sum$variable == "date"])),
                       end = max(as.Date(counts_sum$choice[counts_sum$variable == "date"])))
  updateCheckboxGroupInput(session, "data_bird_id",
                           choices = choices(counts_sum, "bird_id"),
                           selected = selected(counts_sum, "bird_id"))
  updateCheckboxGroupInput(session, "data_feeder_id",
                           choices = choices(counts_sum, "feeder_id"),
                           selected = selected(counts_sum, "feeder_id"))
  cat("Reset data selection...\n")
  values$data <- values_list()
})


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
  
  values$data_previous <- values$data ## Save old
  values$data <- values_list(input)   ## Save new
})

## Subset of counts reflecting selections
counts_sub <- reactive({
  #req(startup(input))
  cat("Calling counts_sub()...\n")
  get_counts(counts, filter = values$data)
})

## Update UI
observeEvent(values$data, {
  req(startup(input))
  cat("Update data UI...\n")
  
  #input$data_update

  #if(isolate(!is.logical(all.equal(values_list(input), values$data)))){
    isolate({
      
      #Save values
      
      v <- counts_sub() ## Get new counts
      
      ## Get options that were 're-checked'
      added <- list('site_name' = setdiff(values$data$site_name, values$data_previous$site_name),
                    'species' = setdiff(values$data$species, values$data_previous$species),
                    'date' = if(any(values$data$date != values$data_previous$date)) values$data$date,
                    'bird_id' = setdiff(values$data$bird_id, values$data_previous$bird_id),
                    'feeder_id' = setdiff(values$data$feeder_id, values$data_previous$feeder_id))
      added <- added[!sapply(added, is.null)]
      
      ## Add re-checked items back in
      for(i in 1:length(added)) v <- unique(rbind(v, get_counts(c = counts, filter = added[i])))
      
      ## Get new totals for UI
      sum <- rbind(
        get_counts(c = v, summarize_by = "site_name"),
        get_counts(c = v, summarize_by = "species"),
        get_counts(c = v, summarize_by = "date"),
        get_counts(c = v, summarize_by = "bird_id"),
        get_counts(c = v, summarize_by = "feeder_id"))
      
      # Update values directly for mapping etc.
      values$data <- values_list(sum)
      values$reset <- FALSE
      
      # Update UIs to match
      list(
        updateCheckboxGroupInput(session, "data_site_name",
                                 choices = choices(sum, "site_name"),
                                 selected = selected(sum, "site_name")),
        updateCheckboxGroupInput(session, "data_species",
                                 choices = choices(sum, "species"),
                                 selected = selected(sum, "species")),
        updateDateRangeInput(session, "data_date",
                             start = min(as.Date(sum$choices[sum$variable == "date"])),
                             end = max(as.Date(sum$choices[sum$variable == "date"])))
      )
    })
})



  

## Initial UI for Site_name, species, date
output$UI_data <- renderUI({
  cat("Initializing data selection UI...\n")
  list(
  #Site name
  checkboxGroupInput("data_site_name", "Sites:",
                     choices = choices(counts_sum, "site_name"),
                     selected = selected(counts_sum, "site_name")),
  
  #Species
  checkboxGroupInput("data_species", "Species to include (select all that apply):",
                     choices = choices(counts_sum, "species"),
                     selected = selected(counts_sum, "species")),
  
  # Date Range
  dateRangeInput("data_date", "Dates to include:",
                   min = min(as.Date(counts_sum$choice[counts_sum$variable == "date"])),
                   max = max(as.Date(counts_sum$choice[counts_sum$variable == "date"])),
                   start = min(as.Date(counts_sum$choice[counts_sum$variable == "date"])),
                   end = max(as.Date(counts_sum$choice[counts_sum$variable == "date"])))
  )
})


## Look for button to toggle showing advanced options:

observeEvent(input$data_showadv, {
  toggle(id = "advanced")
})


## Advanced UI 
output$UI_data_adv <- renderUI({
  checkboxGroupInput("data_bird_id", "Select bird(s)",
              choices = choices(counts_sum, "bird_id"),
              selected = selected(counts_sum, "bird_id"))
  checkboxGroupInput("data_feeder_id", "Select feeder(s)",
                     choices = choices(counts_sum, "feeder_id"),
                     selected = selected(counts_sum, "feeder_id"))
})

#########################
## Monitor selected input
#########################
## Monitor which Data is currently selected
# observeEvent(input$data_update, {
#   req(counts_sub())
#   values$current <- list(site_name = unique(counts_sub()$site_name),
#                          species = unique(counts_sub()$species),
#                          dates = c(min(counts_sub()$date), max(counts_sub()$date)),
#                          bird_id = unique(counts_sub()$bird_id),
#                          feeder_id = unique(counts_sub()$feeder_id))
# })

## Render which data is currently selected
# output$data_current <- renderText({
#   req(values$current)
#   paste0(
#     "<strong>Site:</strong> ", paste0(as.character(values$current$site_name), collapse = ", "), "<br>",
#     "<strong>Species:</strong> ", paste0(as.character(values$current$species), collapse = ", "), "<br>",
#     "<strong>Dates:</strong> ", paste0(as.character(values$current$dates[1]), " to ", as.character(values$current$dates[2])), "<br>",
#     "<strong>Bird Ids:</strong> ", paste0(as.character(values$current$bird_id), collapse = ", "), "<br>",
#     "<strong>Feeder Ids:</strong> ", paste0(as.character(values$current$feeder_id), collapse = ", ")
#   )
# })


####################
## Reactive values
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
      load.format(., tz = "") %>%
      mutate(bird_id = factor(bird_id, levels = sort(unique(birds_all$bird_id))),
             feeder_id = factor(feeder_id, levels = sort(unique(feeders_all$feeder_id)))) %>%
      left_join(birds_all, by = c("bird_id")) %>%
      left_join(feeders_all, by = c("feeder_id", "site_name"))
  } else data <- NULL
  
  #Get weather data 
  if(input$data_weather == "Yes" & any(unique(data$site_name) == "Kamloops, BC")){
    withProgress(message = "Adding Weather Data...",
                 w <- weather(station_ID = 51423, start = min(as.Date(data$time)), end = max(as.Date(data$time)), by = "hour") %>%
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
