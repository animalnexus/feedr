context("ui_animate() locally")

# test_that("Launches", {
#   expect_error(ui_animate(), "argument \"v\" is missing, with no default")
#   expect_error(ui_animate(finches), "You should be using 'visit' data.")
# })

test_that("Options set", {
  remDr <- shiny_test_startup(f_animate, appURL, args = "feedr::visits(feedr::finches)",
                              browserName = "chrome")
  Sys.sleep(5)

  # Check different settings
  s <- list(c(""),  #start up
            c("cumulative", "sum_indiv", "15"),
            c("cumulative", "sum", "360", TRUE),
            c("instant", "sum_indiv", "180", FALSE),
            c("cumulative", "sum_indiv", "15"),
            c("instant", "sum", "60"),
            c("TRUE")) #sunrise/set

  for(i in 1:length(s)) {
    if(s[[i]][1] != "") for(a in s[[i]]) click_setting(remDr, paste0("[value = '", a, "']"))
    take_screenshot(remDr, file = paste0(test_dir, "/screenshots/anim_kam_", i))
    expect_lt(99, compare_screenshot(file = paste0(test_dir, "/screenshots/anim_kam_", i)))
  }

  # Check random settings
  for(i in 1:10) {
    s <- rand_opts()
    for(a in 1:length(s)) {
      if(names(s)[a] == "id"){
        remDr$findElement("css selector", "[class = 'selectize-control single']")$clickElement()
        Sys.sleep(0.25)
        remDr$findElement("css selector", paste0("[data-value = '", s[[a]], "']"))$clickElement()
      } else if(names(s)[a] %in% c("time_range", "anim_speed")) {
        slider_move(remDr, id = names(s)[a], dist = s[[a]])
      } else click_setting(remDr, paste0("[value = '", s[[a]], "']"))
    }
  }

  shiny_test_cleanup(remDr, f_animate)
})
