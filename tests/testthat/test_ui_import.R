library(RSelenium)
library(testthat)
library(feedr)
library(magrittr)

context("ui_import() locally")

# Start Selenium Server ---------------------------------------------------
system("(java -jar ~/R/x86_64-pc-linux-gnu-library/3.3/RSelenium/bin/selenium-server-standalone.jar &)", ignore.stdout = TRUE, ignore.stderr = TRUE)


# Setup -------------------------------------------------------------------
test_dir <- "/home/steffi/Projects/feedr Project/tests/"
appURL <- "http://127.0.0.1:4100"
f <- "feedr::ui_import"


# Import single pretty file -----------------------------------------------
test_that("Import single pretty file", {
  remDr <- shiny_test_startup(f, appURL)

  # Load file
  click_button(remDr, id = "import_reveal")
  e <- remDr$findElement("css", "[id $= 'import_settings']")
  e$sendKeysToElement(list(paste0("/home/steffi/Downloads/animalnexus_settings_", Sys.Date(), ".csv")))

  shiny_test_cleanup(remDr, f)
})

test_that("Import multiple pretty file", {
  remDr <- shiny_test_startup(f, appURL)

  shiny_test_cleanup(remDr, f)
})

test_that("Import single logger file", {
  remDr <- shiny_test_startup(f, appURL)

  shiny_test_cleanup(remDr, f)
})

test_that("Import multiple logger file", {
  remDr <- shiny_test_startup(f, appURL)

  shiny_test_cleanup(remDr, f)
})

test_that("Error on incorrect pretty file", {
  remDr <- shiny_test_startup(f, appURL)

  shiny_test_cleanup(remDr, f)
})

test_that("Error on incorrect logger file", {
  remDr <- shiny_test_startup(f, appURL)

  shiny_test_cleanup(remDr, f)
})

