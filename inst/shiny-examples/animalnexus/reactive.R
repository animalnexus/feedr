
############

trans <- reactiveValues()

observeEvent(data_info(), {
  req(data())
  trans$raw <- data()$data

  withProgress(message = "Transforming Data", detail = "Visits", value = 0, {

    trans$v <- visits(trans$raw, allow_imp = TRUE)

    setProgress(detail = "Movements", value = 0.15)
    trans$m <- trans$v %>%
      dplyr::group_by(bird_id) %>%
      dplyr::do(move(.))

    setProgress(detail = "Feeding time", value = 0.3)
    trans$f <- trans$v %>%
      dplyr::group_by(bird_id) %>%
      dplyr::do(feeding(.))

    setProgress(detail = "Displacements", value = 0.45)
    trans$disp <- disp(v = trans$v)

    setProgress(detail = "Dominance", value = 0.6)
    trans$dom <- dom(trans$disp)

    setProgress(detail = "Activity", value = 0.75)
    trans$a <- activity(trans$f)

    setProgress(detail = "Daily Activity", value = 0.9)
    trans$da <- daily(trans$a)
  })
  trans$data_reset <- FALSE
})

raw <- reactive(trans$raw)
v <- reactive(trans$v)
m <- reactive(trans$m)
f <- reactive(trans$f)

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
