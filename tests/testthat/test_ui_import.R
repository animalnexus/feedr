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


# Files
d_logger <- c(system.file("extdata", "raw", "exp2", "GR10DATA_2016_01_16.TXT", package = "feedr"),
              system.file("extdata", "raw", "exp2", "GR11DATA_2016_01_16.TXT", package = "feedr"))

d_preformat <- c(system.file("extdata", "chickadees.csv", package = "feedr"),
                 system.file("extdata", "finches.csv", package = "feedr"))

d_preformat_errors <- c(system.file("extdata", "import tests", "finches_colnames.csv", package = "feedr"),
                        system.file("extdata", "import tests", "finches_cols.csv", package = "feedr"))

d_preformat_dmy <- c(system.file("extdata", "import tests", "finches_dmy1.csv", package = "feedr"),
                     system.file("extdata", "import tests", "finches_dmy2.csv", package = "feedr"),
                     system.file("extdata", "import tests", "finches_dmy3.csv", package = "feedr"))

d_preformat_mdy <- c(system.file("extdata", "import tests", "finches_mdy1.csv", package = "feedr"),
                     system.file("extdata", "import tests", "finches_mdy2.csv", package = "feedr"),
                     system.file("extdata", "import tests", "finches_mdy3.csv", package = "feedr"))

d_logger_ymd <- c(system.file("extdata", "import tests", "logger_myd1.TXT", package = "feedr"))

d_logger_dmy <- c(system.file("extdata", "import tests", "logger_dmy1.TXT", package = "feedr"),
                  system.file("extdata", "import tests", "logger_dmy2.TXT", package = "feedr"),
                  system.file("extdata", "import tests", "logger_dmy3.TXT", package = "feedr"))

d_logger_mdy <- c(system.file("extdata", "import tests", "logger_mdy1.TXT", package = "feedr"),
                 system.file("extdata", "import tests", "logger_mdy2.TXT", package = "feedr"),
                 system.file("extdata", "import tests", "logger_mdy3.TXT", package = "feedr"))

d_logger_inline <- c(system.file("extdata", "import tests", "logger_inline.TXT", package = "feedr"))

d_logger_index <- c(system.file("shiny-examples", "app_files", "logger_example1.txt", package = "feedr"),
                    system.file("shiny-examples", "app_files", "logger_index_example.csv", package = "feedr"))

# Import single preformat file -----------------------------------------------
test_that("Import single preformat file", {
  remDr <- shiny_test_startup(f, appURL)

  # Select file
  select_files(remDr, d_preformat[2])

  # Preview File
  e <- unlist(remDr$findElement("css", "[id $= 'preview_file']")$getElementText())
  expect_equivalent(e, paste0(readLines(d_preformat[2], 10), collapse = "\n"))

  # Preview Table
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:first-child")$getElementText())
  expect_equivalent(e, "0620000514 2016-01-28 12:34:25 2200 House Finch F -120.3612389 50.66778333")
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(10)")$getElementText())
  expect_equivalent(e, "062000043E 2016-01-28 12:36:47 2200 House Finch M -120.3612389 50.66778333")

  # Save preview table
  preview <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody")$getElementText())

  # Download and compare
  download_files(remDr, d_preformat[2], preview)

  # Clean up
  shiny_test_cleanup(remDr, f)
})

test_that("Import multiple preformat files", {
  remDr <- shiny_test_startup(f, appURL)

  # Select file
  select_files(remDr, d_preformat)

  # Preview File
  e <- unlist(remDr$findElement("css", "[id $= 'preview_file']")$getElementText())
  expect_equivalent(e, paste0(readLines(d_preformat[1], 10), collapse = "\n"))

  # Preview Table
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:first-child")$getElementText())
  expect_equivalent(e, "06200004BF 2016-01-11 10:48:49 exp2-GR10 exp2 2016-01-11 53.89086 -122.81933")
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(10)")$getElementText())
  expect_equivalent(e, "06200004BE 2016-01-11 10:53:02 exp2-GR10 exp2 2016-01-11 53.89086 -122.81933")

  # Save preview table
  preview <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody")$getElementText())

  # Download and compare
  download_files(remDr, d_preformat, preview)

  # Clean up
  shiny_test_cleanup(remDr, f)
})

test_that("Import single logger file", {
  remDr <- shiny_test_startup(f, appURL)

  # Select file
  select_files(remDr, d_logger[2])

  # Expect format fail
  expect_match(test_msg(remDr), "Error importing data, try a different format")

  # Click on logger format
  e <- remDr$findElement("css", "[type = 'radio'][value = 'logger']")
  e$clickElement()
  Sys.sleep(0.5)

  # Expect fail message to disappear
  expect_null(test_msg(remDr))

  # Preview File
  e <- unlist(remDr$findElement("css", "[id $= 'preview_file']")$getElementText())
  expect_equivalent(e, paste0(readLines(d_logger[2], 10), collapse = "\n"))

  # Preview Table
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:first-child")$getElementText())
  expect_equivalent(e, "06200001F0 2016-01-13 10:29:57 GR11DATA")
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(10)")$getElementText())
  expect_equivalent(e, "06200003C3 2016-01-13 10:31:12 GR11DATA")

  # Save preview table
  preview <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody")$getElementText())

  # Download and compare
  download_files(remDr, d_logger[2], preview, type = "logger", time_format = "mdy HMS")

  # Clean up
  shiny_test_cleanup(remDr, f)
})

test_that("Import multiple logger files", {
  remDr <- shiny_test_startup(f, appURL)

  # Select files
  select_files(remDr, d_logger)

  # Click on logger format
  e <- remDr$findElement("css", "[type = 'radio'][value = 'logger']")
  e$clickElement()
  Sys.sleep(0.5)

  # Expect fail message to disappear
  expect_null(test_msg(remDr))

  # Preview File
  e <- unlist(remDr$findElement("css", "[id $= 'preview_file']")$getElementText())
  expect_equivalent(e, paste0(readLines(d_logger[1], 10), collapse = "\n"))

  # Preview Table
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:first-child")$getElementText())
  expect_equivalent(e, "06200004BF 2016-01-11 10:48:49 GR10DATA")
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(10)")$getElementText())
  expect_equivalent(e, "06200004BE 2016-01-11 10:53:02 GR10DATA")

  # Save preview table
  preview <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody")$getElementText())

  # Download and compare
  download_files(remDr, d_logger, preview, type = "logger", time_format = "mdy HMS")

  # Clean up
  shiny_test_cleanup(remDr, f)
})

test_that("Preformat - Incorrect column names", {
  remDr <- shiny_test_startup(f, appURL)

  select_files(remDr, d_preformat_errors[1])
  expect_match(test_msg(remDr), "Error importing data, try a different format")

  shiny_test_cleanup(remDr, f)
})

test_that("Preformat - Incorrect columns", {
  remDr <- shiny_test_startup(f, appURL)

  select_files(remDr, d_preformat_errors[2])
  expect_match(test_msg(remDr), "Error importing data, try a different format")

  shiny_test_cleanup(remDr, f)
})

# Test Time Format --------------------------------------------------------

lapply(c(d_preformat, d_logger_ymd),
       function(x) test_time_formats(f, file = x, format = "ymd"))

lapply(c(d_preformat_mdy, d_logger_mdy),
       function(x) test_time_formats(f, file = x, format = "mdy"))

lapply(c(d_preformat_dmy, d_logger_dmy),
       function(x) test_time_formats(f, file = x, format = "dmy"))


# Test Timezone -----------------------------------------------------------
test_that("Timezone", {
  # remDr <- shiny_test_startup(f, appURL)
  #
  # shiny_test_cleanup(remDr, f)
})

test_that("DST setting", {
  # remDr <- shiny_test_startup(f, appURL)
  #
  # shiny_test_cleanup(remDr, f)
})

test_that("Skip", {
  remDr <- shiny_test_startup(f, appURL)

  shiny_test_cleanup(remDr, f)
})

test_that("Preformat - Separator", {
  remDr <- shiny_test_startup(f, appURL)

  shiny_test_cleanup(remDr, f)
})

test_that("Logger file - Logger id pattern", {
  remDr <- shiny_test_startup(f, appURL)

  shiny_test_cleanup(remDr, f)
})

test_that("Logger file - Lat/Lon in Data file", {
  remDr <- shiny_test_startup(f, appURL)
  select_files(remDr, d_logger_pattern[1])

  # Click on logger format
  e <- remDr$findElement("css", "[type = 'radio'][value = 'logger']")
  e$clickElement()
  Sys.sleep(0.5)
  expect_match(test_msg(remDr), "Error importing data, try a different format or settings")

  # Click on lat/lon format
  remDr$findElement("css", "[type = 'radio'][value = 'inline2']")$clickElement()

  # Expect fail message to disappear
  expect_null(test_msg(remDr))

  # Preview Table
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:first-child")$getElementText())
  expect_equivalent(e, "06200004BB 2016-01-29 14:04:31 GR10DATA 53.89086 -122.81933")
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(10)")$getElementText())
  expect_equivalent(e, "0700EE19CE 2016-01-31 08:43:15 GR10DATA 53.89086 -122.81933")

  shiny_test_cleanup(remDr, f)
})

test_that("Logger file - Lat/Lon in Index file", {
  remDr <- shiny_test_startup(f, appURL)
  select_files(remDr, c(d_logger_index[1]))

  # Click on logger format
  e <- remDr$findElement("css", "[type = 'radio'][value = 'logger']")$clickElement()
  Sys.sleep(0.5)
  expect_null(test_msg(remDr))

  # Click on lat/lon format
  remDr$findElement("css", "[type = 'radio'][value = 'file1']")$clickElement()

  # Expect missing logger_index message:
  expect_match(test_msg(remDr), "Expected file 'logger_index' not in files. Re-select files or choose a different location for logger details.")

  select_files(remDr, c(d_logger_index[2]))

  # Expect no fail message
  expect_null(test_msg(remDr))

  # Preview Table
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:first-child")$getElementText())
  expect_equivalent(e, "062000039D 2015-12-05 10:35:13 GR10DATA 53.914484 -122.769248")
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(10)")$getElementText())
  expect_equivalent(e, "06200003DE 2015-12-05 10:40:52 GR10DATA 53.914484 -122.769248")

  shiny_test_cleanup(remDr, f)
})


# Clean Up ----------------------------------------------------------------
# Get server PIDs and terminate
pid_sel <- system("pgrep -f [s]elenium-server-standalone.jar", intern = TRUE)
system(paste0("kill -TERM ", pid_sel))
