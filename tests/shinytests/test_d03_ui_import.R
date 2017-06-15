context("ui_import() locally")

# Import single preformat file -----------------------------------------------
test_that("Import single preformat file", {
  remDr <- shiny_test_startup(f_import, appURL, browserName = "chrome")

  # Select file
  select_files(remDr, d_preformat[2])

  # Preview File
  e <- unlist(remDr$findElement("css", "[id $= 'preview_file']")$getElementText())
  expect_equivalent(e, paste0(readLines(d_preformat[2], 10), collapse = "\n"))

  # Preview Table
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:first-child")$getElementText())
  expect_equivalent(e, "0620000514 2016-01-28 12:34:25 2200 House Finch F -120.3612389 50.66778333 2016-01-28")
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(10)")$getElementText())
  expect_equivalent(e, "062000043E 2016-01-28 12:36:47 2200 House Finch M -120.3612389 50.66778333 2016-01-28")

  # Save preview table
  preview <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody")$getElementText())

  # Download and compare
  download_files(remDr, d_preformat[2], preview)

  # Clean up
  shiny_test_cleanup(remDr, f_import)
})


# Import multiple preformat files -----------------------------------------
test_that("Import multiple preformat files", {
  remDr <- shiny_test_startup(f_import, appURL, browserName = "chrome")

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
  shiny_test_cleanup(remDr, f_import)
})

# Import single logger file -----------------------------------------------
test_that("Import single logger file", {
  remDr <- shiny_test_startup(f_import, appURL, browserName = "chrome")

  # Select file
  select_files(remDr, d_logger[2])

  # Expect format fail
  expect_match(test_msg(remDr), "Error importing data, try a different format or settings")

  # Click on logger format
  remDr$findElement("css", "[type = 'radio'][value = 'logger']")$clickElement()
  Sys.sleep(0.5)

  # Expect fail message to disappear
  expect_null(test_msg(remDr))

  # Preview File
  e <- unlist(remDr$findElement("css", "[id $= 'preview_file']")$getElementText())
  expect_equivalent(e, paste0(readLines(d_logger[2], 10), collapse = "\n"))

  # Preview Table
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:first-child")$getElementText())
  expect_equivalent(e, "06200001F0 2016-01-13 10:29:57 GR11DATA 2016-01-13")
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(10)")$getElementText())
  expect_equivalent(e, "06200003C3 2016-01-13 10:31:12 GR11DATA 2016-01-13")

  # Save preview table
  preview <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody")$getElementText())

  # Download and compare
  download_files(remDr, d_logger[2], preview, type = "logger", time_format = "mdy HMS")

  # Clean up
  shiny_test_cleanup(remDr, f_import)
})

# Import multiple logger files --------------------------------------------
test_that("Import multiple logger files", {
  remDr <- shiny_test_startup(f_import, appURL, browserName = "chrome")

  # Select files
  select_files(remDr, d_logger)

  # Click on logger format
  remDr$findElement("css", "[type = 'radio'][value = 'logger']")$clickElement()
  Sys.sleep(0.5)

  # Expect fail message to disappear
  expect_null(test_msg(remDr))

  # Preview File
  e <- unlist(remDr$findElement("css", "[id $= 'preview_file']")$getElementText())
  expect_equivalent(e, paste0(readLines(d_logger[1], 10), collapse = "\n"))

  # Preview Table
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:first-child")$getElementText())
  expect_equivalent(e, "06200004BF 2016-01-11 10:48:49 GR10DATA 2016-01-11")
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(10)")$getElementText())
  expect_equivalent(e, "06200004BE 2016-01-11 10:53:02 GR10DATA 2016-01-11")

  # Save preview table
  preview <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody")$getElementText())

  # Download and compare
  download_files(remDr, d_logger, preview, type = "logger", time_format = "mdy HMS")

  # Clean up
  shiny_test_cleanup(remDr, f_import)
})

# Import multiple files contrasting types --------------------------------------------
test_that("Import multiple files contrasting types", {
  remDr <- shiny_test_startup(f_import, appURL, browserName = "chrome")

  # Select files
  select_files(remDr, c(d_logger[1], d_preformat[1]))

  # Expect error message
  expect_match(test_msg(remDr), "Cannot proceed: Some or all of your logger ids are missing \\(i.e. NA\\)")

  # Click on logger format
  remDr$findElement("css", "[type = 'radio'][value = 'logger']")$clickElement()
  Sys.sleep(0.5)

  # Expect error message
  expect_match(test_msg(remDr), "Cannot proceed: NA times detected, check your time format")

    # Clean up
  shiny_test_cleanup(remDr, f_import)
})

# Preformat - Fix some column names ---------------------------------------
test_that("Preformat - Fix column names", {
  remDr <- shiny_test_startup(f_import, appURL, browserName = "chrome")

  select_files(remDr, d_preformat_errors[3])
  expect_null(test_msg(remDr))

  # Preview File
  e <- unlist(remDr$findElement("css", "[id $= 'preview_file']")$getElementText())
  expect_equivalent(e, paste0(readLines(d_preformat_errors[3], 10), collapse = "\n"))

  # Preview Table
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:first-child")$getElementText())
  expect_equivalent(e, "0620000514 2016-01-28 12:34:25 2200 House Finch F -120.3612389 50.66778333 2016-01-28")
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(10)")$getElementText())
  expect_equivalent(e, "062000043E 2016-01-28 12:36:47 2200 House Finch M -120.3612389 50.66778333 2016-01-28")

  # Save preview table
  preview <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody")$getElementText())

  # Download and compare
  download_files(remDr, d_preformat_errors[3], preview)

  # Clean up
  shiny_test_cleanup(remDr, f_import)
})

# Preformat - Incorrect column names --------------------------------------
test_that("Preformat - Incorrect column names", {
  remDr <- shiny_test_startup(f_import, appURL, browserName = "chrome")

  select_files(remDr, d_preformat_errors[1])
  expect_match(test_msg(remDr), "Cannot proceed: Required columns aren't present")

  shiny_test_cleanup(remDr, f_import)
})

test_that("Preformat - Incorrect columns", {
  remDr <- shiny_test_startup(f_import, appURL, browserName = "chrome")

  select_files(remDr, d_preformat_errors[2])
  expect_match(test_msg(remDr), "Cannot proceed: Required columns aren't present")

  shiny_test_cleanup(remDr, f_import)
})

# Test Time Format --------------------------------------------------------

lapply(c(d_preformat, d_logger_ymd),
       function(x) test_time_formats(f_import, file = x, format = "ymd"))

lapply(c(d_preformat_mdy, d_logger_mdy),
       function(x) test_time_formats(f_import, file = x, format = "mdy"))

lapply(c(d_preformat_dmy, d_logger_dmy),
       function(x) test_time_formats(f_import, file = x, format = "dmy"))


# Test Timezone -----------------------------------------------------------
test_that("Timezone", {
  # remDr <- shiny_test_startup(f_import, appURL)
  #
  # shiny_test_cleanup(remDr, f_import)
})


# Test DST ----------------------------------------------------------------
test_that("DST setting", {
  # remDr <- shiny_test_startup(f_import, appURL)
  #
  # shiny_test_cleanup(remDr, f_import)
})


# Test skip - logger ------------------------------------------------------
test_that("Skip - logger", {
  remDr <- shiny_test_startup(f_import, appURL, browserName = "chrome")
  select_files(remDr, d_logger[1])

  # Click on logger format
  remDr$findElement("css", "[type = 'radio'][value = 'logger']")$clickElement()
  Sys.sleep(1)

  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(3)")$getElementText())
  expect_equivalent(e, "06200004E4 2016-01-11 10:48:55 GR10DATA 2016-01-11")

  # Modify skip
  e <- remDr$findElement("css", "[id $= '-skip']")
  e$clearElement()
  e$sendKeysToElement(list("1"))
  Sys.sleep(1)

  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(2)")$getElementText())
  expect_equivalent(e, "06200004E4 2016-01-11 10:48:55 GR10DATA 2016-01-11")

  # Modify skip
  e <- remDr$findElement("css", "[id $= '-skip']")
  e$clearElement()
  e$sendKeysToElement(list("2"))
  Sys.sleep(1)

  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(1)")$getElementText())
  expect_equivalent(e, "06200004E4 2016-01-11 10:48:55 GR10DATA 2016-01-11")

  shiny_test_cleanup(remDr, f_import)
})


# Test skip - prefromat1 --------------------------------------------------
test_that("Skip - preformat1", {
  remDr <- shiny_test_startup(f_import, appURL, browserName = "chrome")
  select_files(remDr, d_preformat[1])

  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(3)")$getElementText())
  expect_equivalent(e, "06200004E4 2016-01-11 10:48:55 exp2-GR10 exp2 2016-01-11 53.89086 -122.81933")

  # Modify skip
  e <- remDr$findElement("css", "[id $= '-skip']")
  e$clearElement()
  e$sendKeysToElement(list("1"))
  Sys.sleep(0.5)

  expect_match(test_msg(remDr), "Error importing data, try a different format.")

  shiny_test_cleanup(remDr, f_import)
})


# Test skip - preformat2 --------------------------------------------------
test_that("Skip - preformat2", {
  remDr <- shiny_test_startup(f_import, appURL, browserName = "chrome")
  select_files(remDr, d_preformat_skip[1])
  expect_match(test_msg(remDr), "Error importing data, try a different format.")

  # Modify skip
  e <- remDr$findElement("css", "[id $= '-skip']")
  e$clearElement()
  e$sendKeysToElement(list("1"))
  Sys.sleep(0.5)

  expect_null(test_msg(remDr))

  shiny_test_cleanup(remDr, f_import)
})


# Preformat - Sep (tab) ---------------------------------------------------
test_that("Preformat - Separator (tab)", {
  remDr <- shiny_test_startup(f_import, appURL, browserName = "chrome")
  select_files(remDr, d_preformat_sep[1])

  expect_match(test_msg(remDr), "Error importing data, try a different format.")

  remDr$findElement("css", "[name = 'standalone-sep'][type = 'radio'][value = ';']")$clickElement()
  Sys.sleep(0.5)
  expect_match(test_msg(remDr), "Error importing data, try a different format.")

  remDr$findElement("css", "[name = 'standalone-sep'][type = 'radio'][value = '\t']")$clickElement()
  Sys.sleep(0.5)
  expect_null(test_msg(remDr))

  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:first-child")$getElementText())
  expect_equivalent(e, "620000514 2016-01-28 12:34:25 2200 House Finch F -120.3612389 50.66778333 2016-01-28")
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(10)")$getElementText())
  expect_equivalent(e, "062000043E 2016-01-28 12:36:47 2200 House Finch M -120.3612389 50.66778333 2016-01-28")

  shiny_test_cleanup(remDr, f_import)
})

# Preformat - Sep (semicolon) ---------------------------------------------
test_that("Preformat - Separator (semicolon)", {
  remDr <- shiny_test_startup(f_import, appURL, browserName = "chrome")
  select_files(remDr, d_preformat_sep[2])

  expect_match(test_msg(remDr), "Error importing data, try a different format.")

  remDr$findElement("css", "[name = 'standalone-sep'][type = 'radio'][value = '\t']")$clickElement()
  Sys.sleep(0.5)
  expect_match(test_msg(remDr), "Error importing data, try a different format.")

  remDr$findElement("css", "[name = 'standalone-sep'][type = 'radio'][value = ';']")$clickElement()
  Sys.sleep(0.5)
  expect_null(test_msg(remDr))

  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:first-child")$getElementText())
  expect_equivalent(e, "620000514 2016-01-28 12:34:25 2200 House Finch F -120.3612389 50.66778333 2016-01-28")
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(10)")$getElementText())
  expect_equivalent(e, "062000043E 2016-01-28 12:36:47 2200 House Finch M -120.3612389 50.66778333 2016-01-28")

  shiny_test_cleanup(remDr, f_import)
})


# Logger Id Pattern -------------------------------------------------------
test_that("Logger file - Logger id pattern", {
  remDr <- shiny_test_startup(f_import, appURL, browserName = "chrome")
  select_files(remDr, d_logger[1])

  # Click on logger format
  remDr$findElement("css", "[type = 'radio'][value = 'logger']")$clickElement()
  Sys.sleep(0.5)

  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:first-child")$getElementText())
  expect_match(e, "GR10DATA")

  # Click on TRU logger id pattern
  remDr$findElement("css", "[data-value = 'NA']")$clickElement()
  Sys.sleep(0.5)
  remDr$findElement("css", "[data-value ^= '[GPR]{2,3}']")$clickElement()

  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:first-child")$getElementText())
  expect_match(e, "GR10")

  shiny_test_cleanup(remDr, f_import)
})



# Logger lat/lon in data file ---------------------------------------------
test_that("Logger file - Lat/Lon in Data file", {
  remDr <- shiny_test_startup(f_import, appURL, browserName = "chrome")
  select_files(remDr, d_logger_inline)

  # Click on logger format
  remDr$findElement("css", "[type = 'radio'][value = 'logger']")$clickElement()
  Sys.sleep(0.5)
  expect_match(test_msg(remDr), "Error importing data, try a different format or settings")

  # Click on lat/lon format
  remDr$findElement("css", "[type = 'radio'][value = 'inline2']")$clickElement()
  Sys.sleep(0.5)

  # Expect fail message to disappear
  expect_null(test_msg(remDr))

  # Preview Table
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:first-child")$getElementText())
  expect_equivalent(e, "06200004BB 2016-01-29 14:04:31 GR10DATA 53.89086 -122.81933 2016-01-29")
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(10)")$getElementText())
  expect_equivalent(e, "0700EE19CE 2016-01-31 08:43:15 GR10DATA 53.89086 -122.81933 2016-01-31")

  shiny_test_cleanup(remDr, f_import)
})


# Logger lat/lon in index -------------------------------------------------
test_that("Logger file - Lat/Lon in Index file", {
  remDr <- shiny_test_startup(f_import, appURL, browserName = "chrome")
  select_files(remDr, c(d_logger_index[1]))

  # Click on logger format
  remDr$findElement("css", "[type = 'radio'][value = 'logger']")$clickElement()
  Sys.sleep(0.5)
  expect_null(test_msg(remDr))

  # Click on lat/lon format
  remDr$findElement("css", "[type = 'radio'][value = 'file1']")$clickElement()

  # Expect missing logger_index message:
  expect_match(test_msg(remDr), "Expected file 'logger_index' not in files. Re-select files or choose a different location for logger details.")

  select_files(remDr, c(d_logger_index))

  # Expect no fail message
  expect_null(test_msg(remDr))

  # Preview Table
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:first-child")$getElementText())
  expect_equivalent(e, "062000039D 2015-12-05 10:35:13 GR10DATA 2015-12-05 53.914484 -122.769248")
  e <- unlist(remDr$findElement("css", "[id $= 'preview'] * tbody > tr:nth-child(10)")$getElementText())
  expect_equivalent(e, "06200003DE 2015-12-05 10:40:52 GR10DATA 2015-12-05 53.914484 -122.769248")

  shiny_test_cleanup(remDr, f_import)
})
