context("ui_db() locally")

library(RSelenium)
library(testthat)
library(feedr)

# Start Selenium Server ---------------------------------------------------
system("(java -jar ~/R/x86_64-pc-linux-gnu-library/3.3/RSelenium/bin/selenium-server-standalone.jar &)", ignore.stdout = TRUE, ignore.stderr = TRUE)


# Setup -------------------------------------------------------------------
test_dir <- "/home/steffi/Projects/feedr Project/tests/"
appURL <- "http://127.0.0.1:4100"
f <- "feedr::ui_db"

# Download reference data set
write.csv(dl_data(start = "2017-01-01", end = "2017-03-02", site_id = "kl", species = "Mountain Chickadee"),
          "../tests/ref_db_kamloops.csv", row.names = FALSE)
write.csv(dl_data(start = "2013-04-01", end = "2013-05-02",
                          tz_disp = "America/Costa_Rica", site_id = "cr", species = "Green Hermit"),
          "../tests/ref_db_costa_rica.csv", row.names = FALSE)

# Select Kamloops data -------------------------------------------------------------------
test_that("Select Kamloops data", {
  remDr <- shiny_test_startup(f, appURL)

  # Test valide data range
  test_db_site(remDr, site = "Kamloops, BC")
  test_db_dates(remDr, dates = c("2017-01-01", "2017-03-02"))

  test_db_species(remDr, species = c("Mountain Chickadee"))
  test_db_n(remDr, species = "Mountain Chickadee", n = 1198)

  test_db_species(remDr, species = c("House Finch"))
  test_db_n(remDr, species = "House Finch", n = 29978)

  # test_db_species(remDr, species = c("Dark-eyed Junco"))
  # test_db_n(remDr, species = "Dark-eyed Junco", n = 0)
  # expect_false(test_error(remDr))

  # Test empty data range
  #test_db_site(remDr, site = "Kamloops, BC")
  #test_db_dates(remDr, dates = c("2017-01-01", "2017-02-02"))
  #test_db_species(remDr, species = c("Dark-eyed Junco"))

  #expect_false(test_error(remDr))

  # Check Data access message
  a <- remDr$findElement(using = "css selector", value = "[id $= 'data_access']")
  expect_equivalent(a$getElementText(), "Fully Public")

  # Take and compare screenshots
  take_screenshot(remDr, file = "../tests/db_kam_")
  expect_lt(99, compare_screenshot(file = "../tests/db_kam_"))

  shiny_test_cleanup(remDr, f)
})


# Select Costa Rican Data -------------------------------------------------
test_that("Select Costa Rican Data", {
  remDr <- shiny_test_startup(f, appURL)

  # Test valid data range
  test_db_site(remDr, site = "Costa Rica")
  test_db_dates(remDr, dates = c("2013-04-01", "2013-05-02"))
  test_db_species(remDr, species = c("Green Hermit"))
  expect_false(test_error(remDr))

  # Test empty data range
  # test_db_site(remDr, site = "Costa Rica")
  # test_db_dates(remDr, dates = c("2015-04-01", "2015-05-02"))
  # expect_false(test_error(remDr))

  # Check Data access message
  a <- remDr$findElement(using = "css selector", value = "[id $= 'data_access'")
  expect_equivalent(a$getElementText(), "Visualizations Only")

  # Take and compare screenshots
  take_screenshot(remDr, file = "../tests/db_cr_")
  expect_lt(99, compare_screenshot(file = "../tests/db_cr_"))

  shiny_test_cleanup(remDr, f)
})


# Select Advanced Options -------------------------------------------------
test_that("Select Advanced Options", {
  remDr <- shiny_test_startup(f, appURL)

  test_db_site(remDr, site = "Kamloops, BC")
  test_db_dates(remDr, dates = c("2017-01-01", "2017-03-02"))
  test_db_species(remDr, species = c("Mountain Chickadee"))

  # Advanced Options show with click
  click_button(remDr, "showadv")
  expect_error(animal_ids <- remDr$findElement("css", "[id $= 'data_animal_id']"), NA)
  expect_error(logger_ids <- remDr$findElement("css", "[id $= 'data_logger_id']"), NA)

  # Some selected, some disabled
  n <- animal_ids$findChildElements("css", "[name $= 'data_animal_id']")
  expect_gt(length(n), 1)
  n_checked <- sapply(n, function(x) unlist(x$getElementAttribute("class")))
  expect_equal(length(n_checked[n_checked != "disabled"]), 3)

  # Some selected, some disabled
  n <- logger_ids$findChildElements("css", "[name $= 'data_logger_id']")
  expect_gt(length(n), 1)
  n_checked <- sapply(n, function(x) unlist(x$getElementAttribute("class")))
  expect_equal(length(n_checked[n_checked != "disabled"]), 5)

  # Deselect IDs, change amount of MOCH
  test_db_n(remDr, species = "Mountain Chickadee", n = 1198) # before

  # animal_id
  n_id <- animal_ids$findChildElement("css", "[value = '062000034E']")
  n_id$clickElement()
  test_db_n(remDr, species = "Mountain Chickadee", n = 453) # after

  # logger_id
  n_id <- logger_ids$findChildElement("css", "[value = '1500']")
  n_id$clickElement()
  test_db_n(remDr, species = "Mountain Chickadee", n = 391) # after

  shiny_test_cleanup(remDr, f)
})


# Reset Data - Kamloops selected ------------------------------------------
test_that("Reset Data - Kamloops selected", {

  remDr <- shiny_test_startup(f, appURL)

  # No data selected - Reset Disabled
  r <- remDr$findElement("css", "[id $= 'data_reset']")
  expect_false(unlist(r$isElementEnabled()))

  # Kamloops data selected
  test_db_site(remDr, site = "Kamloops, BC")
  test_db_dates(remDr, dates = c("2017-01-01", "2017-03-02"))
  test_db_species(remDr, species = c("Mountain Chickadee"))

  click_button(remDr, "showadv")
  remDr$findElement("css", "[value $= '062000034E'")$clickElement()
  remDr$findElement("css", "[value $= '1500'")$clickElement()

  # Reset
  click_button(remDr, "data_reset")

  # Expect non selected
  expect_false(test_error(remDr))
  expect_false(test_present(remDr, "[name $='data_species']"))
  expect_false(test_present(remDr, "[data-initial-date]"))

  # Expect previous settings not saved
  test_db_site(remDr, site = "Kamloops, BC")
  expect_equal(length(remDr$findElements("css", "[name $='data_species']:checked")), 3)

  click_button(remDr, "showadv")

  # Animal ids
  animal_ids_all <- remDr$findElements("css", "[name $= 'data_animal_id']")
  animal_ids_checked <- remDr$findElements("css", "[name $= 'data_animal_id']:checked")
  expect_equal(length(animal_ids_all), length(animal_ids_checked))

  # Logger ids
  logger_ids_all <- remDr$findElements("css", "[name $= 'data_logger_id']")
  logger_ids_checked <- remDr$findElements("css", "[name $= 'data_logger_id']:checked")
  expect_equal(length(logger_ids_all), length(logger_ids_checked))

  shiny_test_cleanup(remDr, f)
})


# Reset Data - Costa Rica selected ----------------------------------------
test_that("Reset Data - Costa Rica selected", {

  remDr <- shiny_test_startup(f, appURL)

  # No data selected - Reset Disabled
  r <- remDr$findElement("css", "[id $= 'data_reset']")
  expect_false(unlist(r$isElementEnabled()))

  # Costa rican data selected
  test_db_site(remDr, site = "Costa Rica")
  test_db_dates(remDr, dates = c("2013-04-01", "2013-05-02"))
  test_db_species(remDr, species = c("Green Hermit"))

  click_button(remDr, "showadv")
  remDr$findElement("css", "[value $= '30D27598596F0001'")$clickElement()
  remDr$findElement("css", "[value $= '10sc2-175'")$clickElement()

  # Reset
  click_button(remDr, "data_reset")

  # Expect non selected
  expect_false(test_error(remDr))
  expect_false(test_present(remDr, "[name $='data_species']"))
  expect_false(test_present(remDr, "[data-initial-date]"))

  # Expect previous settings not saved
  test_db_site(remDr, site = "Costa Rica")
  expect_equal(length(remDr$findElements("css", "[name $='data_species']:checked")), 3)

  # Check advanced
  click_button(remDr, "showadv")

  # Animal ids
  animal_ids_all <- remDr$findElements("css", "[name $= 'data_animal_id']")
  animal_ids_checked <- remDr$findElements("css", "[name $= 'data_animal_id']:checked")
  expect_equal(length(animal_ids_all), length(animal_ids_checked))

  # Logger ids
  logger_ids_all <- remDr$findElements("css", "[name $= 'data_logger_id']")
  logger_ids_checked <- remDr$findElements("css", "[name $= 'data_logger_id']:checked")
  expect_equal(length(logger_ids_all), length(logger_ids_checked))

  shiny_test_cleanup(remDr, f)
})


# Reset Data - Kam then CR selected ---------------------------------------
test_that("Reset Data - Kam then CR selected", {

  remDr <- shiny_test_startup(f, appURL)

  # No data selected - Reset Disabled
  r <- remDr$findElement("css", "[id $= 'data_reset']")
  expect_false(unlist(r$isElementEnabled()))

  # Kamloops data selected
  test_db_site(remDr, site = "Kamloops, BC")
  test_db_dates(remDr, dates = c("2017-01-01", "2017-03-02"))
  test_db_species(remDr, species = c("Mountain Chickadee"))

  click_button(remDr, "showadv")
  remDr$findElement("css", "[value $= '062000034E'")$clickElement()
  remDr$findElement("css", "[value $= '1500'")$clickElement()

  # Costa rican data selected
  test_db_site(remDr, site = "Costa Rica")
  test_db_dates(remDr, dates = c("2013-04-01", "2013-05-02"))
  test_db_species(remDr, species = c("Green Hermit"))

  click_button(remDr, "showadv")
  remDr$findElement("css", "[value $= '30D27598596F0001'")$clickElement()
  remDr$findElement("css", "[value $= '10sc2-175'")$clickElement()

  # Reset
  click_button(remDr, "data_reset")

  # Expect non selected
  expect_false(test_error(remDr))
  expect_false(test_present(remDr, "[name $='data_species']"))
  expect_false(test_present(remDr, "[data-initial-date]"))

  # Expect previous settings not saved
  test_db_site(remDr, site = "Kamloops, BC")
  expect_equal(length(remDr$findElements("css", "[name $='data_species']:checked")), 3)

  # Check advanced
  click_button(remDr, "showadv")

  # Animal ids
  animal_ids_all <- remDr$findElements("css", "[name $= 'data_animal_id']")
  animal_ids_checked <- remDr$findElements("css", "[name $= 'data_animal_id']:checked")
  expect_equal(length(animal_ids_all), length(animal_ids_checked))

  # Logger ids
  logger_ids_all <- remDr$findElements("css", "[name $= 'data_logger_id']")
  logger_ids_checked <- remDr$findElements("css", "[name $= 'data_logger_id']:checked")
  expect_equal(length(logger_ids_all), length(logger_ids_checked))

  shiny_test_cleanup(remDr, f)
})

# Download Kamloops Data --------------------------------------------------
test_that("Download Kamloops Data", {
  remDr <- shiny_test_startup(f, appURL)

  test_db_site(remDr, site = "Kamloops, BC")
  test_db_dates(remDr, dates = c("2017-01-01", "2017-03-02"))
  test_db_species(remDr, species = c("Mountain Chickadee"))

  # Test get data
  click_button(remDr, "data_get")
  ui_loaded(remDr)

  # Check downloaded data against expectations
  expect_equal(readLines(paste0(test_dir, "/downloads/output.csv")),
               readLines(paste0(test_dir, "ref_db_kamloops.csv")))

  file.remove(paste0(test_dir, "/downloads/output.csv"))

  shiny_test_cleanup(remDr, f)
})


# Download Costa Rica Data ------------------------------------------------
test_that("Download Costa Rica Data", {
  remDr <- shiny_test_startup(f, appURL)

  test_db_site(remDr, site = "Costa Rica")
  test_db_dates(remDr, dates = c("2013-04-01", "2013-05-02"))
  test_db_species(remDr, species = c("Green Hermit"))

  # Test get data
  click_button(remDr, "data_get")
  ui_loaded(remDr)

  # Check downloaded data against expectations
  expect_equal(readLines(paste0(test_dir, "/downloads/output.csv")),
               readLines(paste0(test_dir, "ref_db_costa_rica.csv")))

  file.remove(paste0(test_dir, "/downloads/output.csv"))

  shiny_test_cleanup(remDr, f)
})


# Update map --------------------------------------------------------------
test_that("Update map", {
  remDr <- shiny_test_startup(f, appURL)

  test_db_site(remDr, site = "Kamloops, BC")

  take_screenshot(remDr, "../tests/db_map_", ref = TRUE)

  test_db_dates(remDr, dates = c("2017-01-01", "2017-03-02"))
  test_db_species(remDr, species = c("Mountain Chickadee"))

  # Expect little difference (just checkboxes)
  take_screenshot(remDr, "../tests/db_map_")
  expect_lt(90, compare_screenshot("../tests/db_map_"))

  # Click map update button
  click_button(remDr, "map_update")

  # Expected bigger change with update map
  take_screenshot(remDr, "../tests/db_map_")
  expect_gt(90, compare_screenshot("../tests/db_map_"))

  shiny_test_cleanup(remDr, f)
})


# Select and Download Random Kamloops data --------------------------------
test_that("Random - Kamloops data", {
})

# Select and Download Random Costa Rican data -----------------------------
test_that("Random - Costa Rican data", {
})

# Clean Up ----------------------------------------------------------------
# Get server PIDs and terminate
pid_sel <- system("pgrep -f [s]elenium-server-standalone.jar", intern = TRUE)
system(paste0("kill -TERM ", pid_sel))
