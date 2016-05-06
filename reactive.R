v_info <- reactive({
  req(v, t_visits, b_visits, t_birds, input$birds)
  temp <- NULL
  if(input$birds == "visits") temp <- v %>% mutate(n = 100)
  if(input$birds == "t_visits") temp <- t_visits
  if(input$birds == "b_visits") temp <- b_visits
  if(input$birds == "t_birds") temp <- t_birds
  temp
})

v_points <- reactive({
  req(v, input$time, input$interval, input$birds)
  
  temp <- v %>% filter(start >= input$time[1], start < input$time[1] + 60 * 60 * input$interval)
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
  req(m, input$bird_id)
  m %>% filter(bird_id == input$bird_id) %>% arrange(move_path)
})