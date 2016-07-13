



############

v <- reactive({
  visits(raw(), allow_imp = TRUE)
})


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
  withProgress({
    v() %>%
      group_by(bird_id) %>%
      do(feeding(.))
  }, message = "Calculating feeding time")
})
