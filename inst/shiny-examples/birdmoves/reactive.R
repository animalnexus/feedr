



############






m_paths <- reactive({
  req(input$bird_id)
  m() %>% filter(bird_id == input$bird_id) %>% arrange(move_path)
})

f <- reactive({
  req(v())
  v() %>%
    group_by(bird_id) %>%
    do(feeding(.))
})
