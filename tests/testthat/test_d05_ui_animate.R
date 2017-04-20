context("ui_animate() locally")

# test_that("Launches", {
#   expect_error(ui_animate(), "argument \"v\" is missing, with no default")
#   expect_error(ui_animate(finches), "You should be using 'visit' data.")
# })

test_that("Options set", {
  remDr <- shiny_test_startup(f_animate, appURL, args = "feedr::visits(feedr::finches)", browserName = "chrome")

  # Take and compare screenshots
  #take_screenshot(remDr, file = paste0(test_dir, "/screenshots/anim_kam_"), ref = TRUE)
  take_screenshot(remDr, file = paste0(test_dir, "/screenshots/anim_kam_"))
  expect_lt(99, compare_screenshot(file = paste0(test_dir, "/screenshots/anim_kam_")))

  # Change settings
  click_setting(remDr, "[value = 'instant']")
  click_setting(remDr, "[value = 'cumulative']")
  click_setting(remDr, "[value = 'sum_indiv']")
  click_setting(remDr, "[value = 'sum']")

  click_setting(remDr, "[value = 'instant']")
  click_setting(remDr, "[value = 'sum_indiv']")
  click_setting(remDr, "[value = 'sum']")

  e <- remDr$findElement("css", "div[id $= 'UI_time'] [class = 'irs-slider single']")
  remDr$mouseMoveToLocation(webElement = e)
  buttondown()
  buttonup()
  shiny_test_cleanup(remDr, f_animate)
})
