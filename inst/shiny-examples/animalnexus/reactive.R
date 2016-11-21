
############

observeEvent(data_info(), {
  req(data())
  values$raw <- data()$data

  withProgress(message = "Transforming Data", detail = "Visits", value = 0, {

    values$v <- visits(values$raw, allow_imp = TRUE)

    setProgress(detail = "Movements", value = 0.33)
    values$m <- values$v %>%
      dplyr::group_by(bird_id) %>%
      dplyr::do(move(.))

    setProgress(detail = "Feeding time", value = 0.66)
    values$f <- values$v %>%
      dplyr::group_by(bird_id) %>%
      dplyr::do(feeding(.))
  })
  values$data_reset <- FALSE
})

raw <- reactive(values$raw)
v <- reactive(values$v)
m <- reactive(values$m)
f <- reactive(values$f)

# v <- reactive({
#   req(nchar(data_info()) > 16) ##check that active data set message is updated first
#   withProgress({
#     visits(raw(), allow_imp = TRUE)
#   }, message = "Calculating Visits")
# })
#
#
# m <- reactive({
#   req(v())
#   withProgress({
#     v() %>%
#       dplyr::group_by(bird_id) %>%
#       dplyr::do(move(.))
#   }, message = "Calculating movements")
# })
#
# f <- reactive({
#   req(v())
#   withProgress({
#     v() %>%
#       dplyr::group_by(bird_id) %>%
#       dplyr::do(feeding(.))
#   }, message = "Calculating feeding time")
# })
