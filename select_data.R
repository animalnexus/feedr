####################
## Output UIs
####################





## Timer to allow user time to select multiple UI elements before updating

observe({ ## If inputs change, reset submission
  req(startup(input))
  
  input$data_sitename
  input$data_species
  input$data_date
  input$data_birdid
  input$data_feederid
  
  values$data_submit <- FALSE
})

observe({
  req(startup(input))
  
  ## Fire after 1 second
  invalidateLater(500, session)
  
  ## Fire (and reset) any time the inputs change (set FALSE above, then TRUE here)
  input$data_sitename
  input$data_species
  input$data_date
  input$data_birdid
  input$data_feederid
  
  input$data_get # Also update selection if "Get Data" button pressed
  
  if(isolate(values$data_submit)) {
    #If data set to submit, send input values off (this happens at start)
    values$data$sitename <- input$data_sitename
    values$data$species <- input$data_species
    values$data$date <- input$data_date
    if("data_birdid" %in% names(input)) values$data$birdid <- input$data_birdid else values$data$birdid <- unique(counts$bird_id)
    if("data_feederid" %in% names(input)) values$data$feederid <- input$data_feederid else values$data$feederid <- unique(counts$feeder_id)
  } else {
    # If data NOT set to submit, set it to TRUE...
    # This will refire 1 second later and will submit it then (as long as no inputs change)
    isolate(values$data_submit <- TRUE)
  }
})


## Subset of counts reflecting selections
counts_sub <- reactive({
  req(startup(input))
  
  d <- values$data
  
  counts %>% filter(site_name %in% d$sitename,
                    species %in% d$species,
                    date %within% interval(d$date[1], d$date[2]),
                    bird_id %in% d$birdid,
                    feeder_id %in% d$feederid)
})
  



## Site_name
output$UI_data_sitename <- renderUI({
  sum <- counts %>% 
    group_by(site_name) %>% 
    summarize(sum = sum(count)) %>%
    mutate(count = paste0(site_name, " (", sum, ") "))
  
  choices <- as.character(sum$site_name)
  names(choices) <- sum$count
  
  checkboxGroupInput("data_sitename", "Sites:",
                     choices = choices,
                     selected = isolate(values$data$sitename))
})

# Update site count totals but nothing else
observe({
  req(startup(input))
  if(nrow(counts_sub()) > 0){
    sum <- counts_sub()  %>%
      group_by(site_name) %>%
      summarize(sum = sum(count)) %>%
      complete(site_name, fill = list(sum = 0)) %>%
      arrange(site_name) %>%
      mutate(count = paste0(site_name, " (", sum, ")"))
  } else {
    sum <- data.frame(site_name= unique(counts$site_name), sum = 0) %>% 
      mutate(count = paste0(site_name, " (", sum, ")"))
  }
  
  choices <- as.character(sum$site_name)
  names(choices) <- sum$count
  
  updateCheckboxGroupInput(session, "data_sitename", choices = choices, selected = isolate(values$data$sitename))
})


## Species
output$UI_data_species <- renderUI({
  sum <- counts %>% 
    group_by(species) %>% 
    summarize(sum = sum(count)) %>%
    mutate(count = paste0(species, " (", sum, ")")) %>%
    arrange(species)
  
  choices <- as.character(sum$species)
  names(choices) <- sum$count
  
  checkboxGroupInput("data_species", "Species to include (select all that apply):",
                     choices = choices,
                     selected = isolate(values$data$species))
})

# Update species count totals but nothing else
observe({
  req(startup(input))
  if(nrow(counts_sub()) > 0){
    sum <- counts_sub()  %>%
      group_by(species) %>%
      summarize(sum = sum(count)) %>%
      complete(species, fill = list(sum = 0)) %>%
      arrange(species) %>%
      mutate(count = paste0(species, " (", sum, ")"))
  } else {
    sum <- data.frame(species = unique(counts$species), sum = 0) %>% 
      mutate(count = paste0(species, " (", sum, ")"))
  }

  choices <- as.character(sum$species)
  names(choices) <- sum$count

  updateCheckboxGroupInput(session, "data_species", choices = choices, selected = isolate(values$data$species))
})


## Date Range
output$UI_data_date <- renderUI({
  sum <- counts %>% 
    group_by(date) %>% 
    summarize(sum = sum(count)) %>%
    mutate(count = paste0(date, " (", sum, ")"))

  #Starting values
  isolate(values$data$date <- c(min(sum$date), max(sum$date)))
  
  dateRangeInput("data_date", "Dates to include:",
                 min = min(sum$date),
                 max = max(sum$date),
                 start = min(sum$date),
                 end = max(sum$date))
})

## Look for button to toggle showing advanced options:

observeEvent(input$data_showadv, {
  toggle(id = "advanced")
})


## Bird_ids
output$UI_data_birdid <- renderUI({
  isolate(values$data$birdid <- as.character(unique(counts$bird_id)))
  checkboxGroupInput("data_birdid", "Select bird(s)",
              choices = as.character(unique(counts$bird_id)),
              selected = as.character(unique(counts$bird_id)))
})

# observe({
#   sel <- as.character(intersect(input$data_birdid, unique(counts_sub()$bird_id)))
#   updateSelectInput(session, "data_birdid", 
#                     selected = sel, 
#                     choices = as.character(unique(counts_sub()$bird_id)))
# })

## Feeder_ids
output$UI_data_feederid <- renderUI({
  isolate(values$data$feederid <- as.character(unique(counts$feeder_id)))
  checkboxGroupInput("data_feederid", "Select feeder(s)",
              choices = as.character(unique(counts$feeder_id)),
              selected = as.character(unique(counts$feeder_id)))
})

# observe({
#   sel <- as.character(intersect(input$data_feederid, unique(counts_sub()$feeder_id)))
#   updateSelectInput(session, "data_feederid", 
#                     selected = sel, 
#                     choices = as.character(unique(counts_sub()$feeder_id)))
# })

#########################
## Monitor selected input
#########################
## Monitor which Data is currently selected
observeEvent(input$data_get, {
  req(counts_sub())
  values$current <- list(sitename = unique(counts_sub()$site_name),
                         species = unique(counts_sub()$species),
                         dates = c(min(counts_sub()$date), max(counts_sub()$date)),
                         bird_id = unique(counts_sub()$bird_id),
                         feeder_id = unique(counts_sub()$feeder_id))
})

## Render which data is currently selected
output$data_current <- renderText({
  req(values$current)
  paste0(
    "<strong>Site:</strong> ", paste0(as.character(values$current$sitename), collapse = ", "), "<br>",
    "<strong>Species:</strong> ", paste0(as.character(values$current$species), collapse = ", "), "<br>",
    "<strong>Dates:</strong> ", paste0(as.character(values$current$dates[1]), " to ", as.character(values$current$dates[2])), "<br>",
    "<strong>Bird Ids:</strong> ", paste0(as.character(values$current$bird_id), collapse = ", "), "<br>",
    "<strong>Feeder Ids:</strong> ", paste0(as.character(values$current$feeder_id), collapse = ", ")
  )
})


####################
## Reactive values
####################

## Download Selected Data
data <- eventReactive(input$data_get, {
  req(counts_sub())
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
  
  data
})

## Feeders of current data
feeders_sub <- reactive({
  data() %>%
    select(feeder_id, site_name, lon, lat) %>%
    unique(.)
})

## Birds of current data
birds_sub <- reactive({
  data() %>%
    select(bird_id, species, age, sex, tagged_on, site_name) %>%
    unique(.)
})
