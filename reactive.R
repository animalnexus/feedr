
# Data for animated maps
v <- reactive({
  req(data())
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



############


v_info <- reactive({
  req(input$anim_type)
  temp <- NULL
  if(input$anim_type == "visits") temp <- v() %>% mutate(n = 100)
  if(input$anim_type == "t_visits") temp <- t_visits()
  if(input$anim_type == "b_visits") temp <- b_visits()
  if(input$anim_type == "t_birds") temp <- t_birds()
  temp
})

v_points <- reactive({
  req(input$anim_time, input$anim_interval, input$anim_type)
  
  temp <- v() %>% filter(start >= input$anim_time[1], start < input$anim_time[1] + 60 * 60 * input$anim_interval)
  if(input$anim_type == "t_visits") temp <- temp %>% summarize(n = length(start))
  if(input$anim_type == "b_visits") {
    temp <- temp %>% 
      group_by(bird_id, add = TRUE) %>% 
      summarize(n = length(start)) %>%
      group_by(feeder_id, lat, lon) %>%
      summarize(n = mean(n))
  }
  if(input$anim_type == "t_birds") temp <- temp %>% summarize(n = length(unique(bird_id)))
  temp
  
})

m_paths <- reactive({
  req(input$bird_id)
  m() %>% filter(bird_id == input$bird_id) %>% arrange(move_path)
})