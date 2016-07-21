



############

v <- reactive({
  visits(raw(), allow_imp = TRUE)
})


m <- reactive({
  req(v())
  withProgress({
    v() %>%
      dplyr::group_by(bird_id) %>%
      dplyr::do(move(.))
  }, message = "Calculating movements")
})

f <- reactive({
  req(v())
  withProgress({
    v() %>%
      dplyr::group_by(bird_id) %>%
      dplyr::do(feeding(.))
  }, message = "Calculating feeding time")
})
