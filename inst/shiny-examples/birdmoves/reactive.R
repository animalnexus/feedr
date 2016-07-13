



############



m <- reactive({
  req(v())
  withProgress({
    v() %>%
      group_by(bird_id) %>%
      do(move(.))
  }, message = "Calculating movements")
})

f <- reactive({
  req(v())
  v() %>%
    group_by(bird_id) %>%
    do(feeding(.))
})
