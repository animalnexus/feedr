library(feedr)
context("Internal formating and error checks")

# check_name()
test_that("check_name returns error if name missing", {
  d <- data.frame(animal_id = NA, logger_id = NA, start = 2, end = 2)
  expect_error(check_name(d, c("animal_ID", "logGer_id")))
  expect_silent(check_name(d, c("animal_id", "logger_id")))
})

# check_time()
test_that("check_time returns error if col not time", {
  d <- data.frame(animal_id = NA, logger_id = NA, start = as.POSIXct("2015-01-01 12:34:00"), end = 54)
  expect_error(check_time(d, c("start", "end")))
  expect_silent(check_name(d, c("start")))

  expect_error(check_time(d, c("start", "end")), "Columns 'start', 'end' must be in R's date/time formating \\(POSIXct\\).")
  expect_error(check_time(d, c("start", "end"), internal = FALSE), "Consider using as.POSIXct()")
})

# check_tz()
test_that("check_tz returns UTC if problems", {
  tz1 <- "America/Vancouver"
  tz2 <- "UTC"
  tz3 <- c("America/Halifax", "UTC")
  tz4 <- NULL
  tz5 <- NA
  tz6 <- ""

  expect_silent(check_tz("America/Vancouver"))
  expect_equal(check_tz("America/Vancouver"), "America/Vancouver")

  expect_silent(check_tz("UTC"))
  expect_equal(check_tz("UTC"), "UTC")

  expect_message(expect_equal(check_tz("AMerica/Vancouver"), "America/Vancouver"), "not in OlsonNames\\(\\), assuming")
  expect_message(expect_equal(check_tz("Amer"), "UTC"), "not in OlsonNames\\(\\), defaulting to UTC")

  expect_message(expect_equal(check_tz(NULL), "UTC"), "Cannot set timezone, defaulting to UTC")
  expect_message(expect_equal(check_tz(NA), "UTC"), "Cannot set timezone, defaulting to UTC")
  expect_message(expect_equal(check_tz(""), "UTC"), "Cannot set timezone, defaulting to UTC")
})

# check_indiv()
test_that("check_indiv returns error if more than one animal_id", {
  d <- data.frame(animal_id = c("041868D396", "041868D861", "041868FF93", "062000043E", "06200004F8", "0620000514"),
                  logger_id = NA)
  expect_error(check_indiv(d))
  expect_silent(check_indiv(data.frame(animal_id = rep("041868D396", 3), logger_id = 45)))
})

# check_format()
test_that("check_format returns error if '_' in column values", {
  d <- data.frame(animal_id = c("041868D396", "041868D861", "041868FF93", "062000__043E", "0620__0004F8", "0620000514"),
                  logger_id = c("456", "5678", "TR_567", "GH_563", "YU848", "56_67"))
  expect_message(check_format(d))
  expect_silent(check_format(d[1:2,]))

  expect_message(check_format(d, map = TRUE), regexp = "Using '_' in logger_id values conflicts with the mapping functions.\n$")
  expect_message(check_format(d, map = TRUE), regexp = "Using '_' in animal_id values conflicts with the displacement/dominance functions. You should remove any '_'s if you plan to use these functions.\n")

  expect_message(check_format(d, disp = TRUE), regexp = "Using '_' in logger_id values conflicts with the mapping functions. You should remove any '_'s if you plan to use these functions.\n$")
  expect_message(check_format(d, disp = TRUE), regexp = "Using '_' in animal_id values conflicts with the displacement/dominance functions.\n")
})
