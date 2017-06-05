context("Online animalnexus")

test_that("Connect to animalnexus", {

  remDr <- shiny_test_startup(appURL = siteURL, type = "remote")
  expect_true(app_loaded(remDr))   #Loading message gone

  # Tabs
  tabs <- remDr$findElements(using = "css selector", value = "[data-toggle = 'tab']")
  tabs <- data.frame(names = unlist(sapply(tabs, function(x) x$getElementAttribute("data-value"))),
                     state = unlist(sapply(tabs, function(x) x$isElementDisplayed())))

  expect_true(all(c("Home", "Database", "Import", "Visualizations", "Individuals",
                    "Transformations", "Settings") %in% tabs$names))
  expect_true(all(tabs$state[tabs$names %in% c("Home", "Database", "Import")]))

  # Active data set message
  v <- remDr$findElement(using = "css", value = "[id = 'package_version']")
  expect_match(unlist(v$getElementText()), "Using feedr v[0-9.]+")
  ad <- remDr$findElement(using = "css selector", value = "[id = 'data_info']")
  expect_equivalent(unlist(ad$getElementText()), "Active dataset: None")

  shiny_test_cleanup(remDr)
})


test_that("Data selection", {
  remDr <- shiny_test_startup(appURL = siteURL, type = "remote", browserName = "chrome")

  # Select Database
  nav_tab(remDr, "Database")
  test_db_site(remDr, site = "Kamloops, BC")
  test_db_dates(remDr, dates = c("2017-01-01", "2017-03-02"))
  Sys.sleep(2)
  test_db_species(remDr, species = c("Mountain Chickadee"))

  click_button(remDr, "data_get")
  ui_loaded(remDr)
  expect_false(test_error(remDr))

  ad <- remDr$findElement(using = "css selector", value = "[id = 'data_info']")
  expect_match(unlist(ad$getElementText()), "Active dataset: Kamloops, BC")

  # Test animate, indiv, trans
  nav_tab(remDr, "Visualizations")
  ui_loaded(remDr)
  expect_false(test_error(remDr))
  #take_screenshot(remDr, file = paste0(test_dir, "/screenshots/anim_kam_remote_"), ref = TRUE)
  take_screenshot(remDr, file = paste0(test_dir, "/screenshots/anim_kam_remote_"))
  expect_lt(99, compare_screenshot(file = paste0(test_dir, "/screenshots/anim_kam_remote_")))

  nav_tab(remDr, "Individuals")
  ui_loaded(remDr)
  expect_false(test_error(remDr))
  e <- remDr$findElements("css", "tr[role = 'row']")
  e_txt <- which(unlist(lapply(e, function(x) stringr::str_detect(unlist(x$getElementText()), "041868FF93"))))
  e[[e_txt]]$clickElement()
  #take_screenshot(remDr, file = paste0(test_dir, "/screenshots/indiv_kam_remote_"), ref = TRUE)
  take_screenshot(remDr, file = paste0(test_dir, "/screenshots/indiv_kam_remote_"))
  expect_lt(99, compare_screenshot(file = paste0(test_dir, "/screenshots/indiv_kam_remote_")))

  nav_tab(remDr, "Transformations")
  ui_loaded(remDr)
  expect_false(test_error(remDr))
  #take_screenshot(remDr, file = paste0(test_dir, "/screenshots/trans_kam_remote_"), ref = TRUE)
  take_screenshot(remDr, file = paste0(test_dir, "/screenshots/trans_kam_remote_"))
  expect_lt(99, compare_screenshot(file = paste0(test_dir, "/screenshots/trans_kam_remote_")))

  ################
  # Select Import
  ################
  nav_tab(remDr, "Import")
  select_files(remDr, d_preformat[2])
  click_button(remDr, "get_data")
  ui_loaded(remDr)
  expect_false(test_error(remDr))
  ad <- remDr$findElement(using = "css selector", value = "[id = 'data_info']")
  expect_match(unlist(ad$getElementText()), "Active dataset: finches.csv")

  # Test indiv, trans, animate
  nav_tab(remDr, "Visualizations")
  ui_loaded(remDr)
  expect_false(test_error(remDr))
  #take_screenshot(remDr, file = paste0(test_dir, "/screenshots/anim_kam_remote2_"), ref = TRUE)
  take_screenshot(remDr, file = paste0(test_dir, "/screenshots/anim_kam_remote2_"))
  expect_lt(99, compare_screenshot(file = paste0(test_dir, "/screenshots/anim_kam_remote2_")))

  nav_tab(remDr, "Individuals")
  ui_loaded(remDr)
  expect_false(test_error(remDr))
  e <- remDr$findElements("css", "tr[role = 'row']")
  e_txt <- which(unlist(lapply(e, function(x) stringr::str_detect(unlist(x$getElementText()), "041868D396"))))
  e[[e_txt]]$clickElement()
  #take_screenshot(remDr, file = paste0(test_dir, "/screenshots/indiv_kam_remote2_"), ref = TRUE)
  take_screenshot(remDr, file = paste0(test_dir, "/screenshots/indiv_kam_remote2_"))
  expect_lt(99, compare_screenshot(file = paste0(test_dir, "/screenshots/indiv_kam_remote2_")))

  nav_tab(remDr, "Transformations")
  ui_loaded(remDr)
  expect_false(test_error(remDr))
  #take_screenshot(remDr, file = paste0(test_dir, "/screenshots/trans_kam_remote2_"), ref = TRUE)
  take_screenshot(remDr, file = paste0(test_dir, "/screenshots/trans_kam_remote2_"))
  expect_lt(99, compare_screenshot(file = paste0(test_dir, "/screenshots/trans_kam_remote2_")))

  shiny_test_cleanup(remDr)
})

# test_that("Transformations", {
#
#
# })
