values <- reactiveValues()

data <- eventReactive(input$data_get, {
  req(input$data_birdid, input$data_species, input$data_sitename)
  con <- dbConnect(drv,host=dbhost,port=dbport,dbname=dbname,user=dbuser,password=dbpass)
  
  if(input$data_birdid[1] == "All") bird_ids <- unique(birds_all$bird_id) else bird_ids <- input$data_birdid
  withProgress(message = "Retrieving Data...",
    suppressWarnings(
     data <- dbGetQuery(con, statement = paste0("SELECT raw.visits.bird_id, ",
                                                       "raw.visits.feeder_id, ",
                                                       "raw.visits.time, ",
                                                       "birds.species, ",
                                                       "feeders.site_name, ",
                                                       "feeders.loc ",
                                               "FROM raw.visits, feeders, birds ",
                                               "WHERE raw.visits.feeder_id = feeders.feeder_id ",
                                                 "AND birds.bird_id = raw.visits.bird_id ",
                                                 "AND feeders.site_name LIKE '%", input$data_sitename, "%' ",
                                                 "AND raw.visits.bird_id IN ( '", paste0(bird_ids, collapse = "', '"),"' )",
                                                 "AND birds.species IN ( '", paste0(input$data_species, collapse = "', '"), "' )"#,
                                                 #"AND raw.visits.time > CURRENT_TIMESTAMP - INTERVAL '2 month'"
                                              ))
    )
  )
     if(nrow(data) > 0) {
        data <- data %>% left_join(birds_all, by = c("bird_id", "site_name", "species")) %>%
          load.format(., tz = "")
     }
  dbDisconnect(con)
  data
})

feeders_sub <- reactive({
  data() %>%
    select(feeder_id, site_name, lon, lat) %>%
    unique(.)
})

birds_sub <- reactive({
  data() %>%
    select(bird_id, species, age, sex, tagged_on, site_name) %>%
    unique(.)
})

# Data for animated maps
v <- reactive({
  visits(data(), allow.imp = TRUE) %>% 
    mutate(day = as.Date(start)) %>%
    group_by(feeder_id, lat, lon)
})

t_visits <- reactive({
  v() %>%
    group_by(day, add = TRUE) %>%
    summarize(n = length(start))
})

b_visits <- reactive({
  v() %>%
    group_by(bird_id, day, add = TRUE) %>%
    summarize(n = length(start)) %>%
    group_by(feeder_id, lat, lon) %>%
    summarize(n = max(n))
})

t_birds <- reactive({
  v() %>%
    group_by(day, add = TRUE) %>%
    summarize(n = length(unique(bird_id)))
})

# Data for static maps
m <- reactive({
  v() %>% 
    group_by(bird_id) %>% 
    do(move(.)) %>%
    group_by(bird_id, feeder_id, move_path) %>%
    summarize(path_use = length(move_path)) %>%
    arrange(bird_id, move_path)
})

f <- reactive({
  v() %>% 
    group_by(bird_id) %>% 
    do(feeding(.)) %>%
    group_by(bird_id, feeder_id) %>%
    summarize(feed_length = sum(feed_length))
})

map_static <- reactive({
  req(input$static_birdid)
  withProgress(message = "Calculating visits and movement paths...",
               map.leaflet(f = f()[f()$bird_id == input$static_birdid, ], m = m()[m()$bird_id == input$static_birdid,], locs = feeders_sub())
  )
})


############


v_info <- reactive({
  req(input$birds)
  temp <- NULL
  if(input$birds == "visits") temp <- v() %>% mutate(n = 100)
  if(input$birds == "t_visits") temp <- t_visits()
  if(input$birds == "b_visits") temp <- b_visits()
  if(input$birds == "t_birds") temp <- t_birds()
  temp
})

v_points <- reactive({
  req(input$time, input$interval, input$birds)
  
  temp <- v() %>% filter(start >= input$time[1], start < input$time[1] + 60 * 60 * input$interval)
  if(input$birds == "t_visits") temp <- temp %>% summarize(n = length(start))
  if(input$birds == "b_visits") {
    temp <- temp %>% 
      group_by(bird_id, add = TRUE) %>% 
      summarize(n = length(start)) %>%
      group_by(feeder_id, lat, lon) %>%
      summarize(n = mean(n))
  }
  if(input$birds == "t_birds") temp <- temp %>% summarize(n = length(unique(bird_id)))
  temp
  
})

m_paths <- reactive({
  req(input$bird_id)
  m() %>% filter(bird_id == input$bird_id) %>% arrange(move_path)
})