library(feedr)
context("Loading files")

# load_raw() - logger_id from file name
test_that("load_raw loads and formats data correctly - logger id from file name", {
  f <- system.file("extdata", "raw", "exp2", "GR10DATA_2016_01_16.TXT", package = "feedr")
  expect_is(r <- load_raw(r_file = f, tz = "America/Vancouver"), "data.frame")
  expect_match(names(r)[1:3], "^animal_id$|^time$|^logger_id$")
  expect_is(r$animal_id, "character")
  expect_is(r$logger_id, "character")
  expect_is(r$time, "POSIXct")

  expect_equal(r$animal_id[2], "062000014F")
  expect_equal(r$logger_id[2], "GR10")
  expect_equal(r$time[2], as.POSIXct("2016-01-11 12:00:54", tz = "America/Vancouver"))
  expect_equal(load_raw(f, tz = "America/Vancouver", tz_disp = "America/Toronto")$time[2], as.POSIXct("2016-01-11 15:00:54", tz = "America/Toronto"))
  expect_equal(load_raw(f)$time[2], as.POSIXct("2016-01-11 12:00:54", tz = Sys.timezone()))
})

# load_raw() - logger_id from first line
test_that("load_raw loads and formats data correctly - logger id from first line", {
  f <- system.file("extdata", "raw", "exp2", "GR10DATA_2016_01_16.TXT", package = "feedr")
  expect_is(r <- load_raw(r_file = f, details = 1, tz = "America/Vancouver"), "data.frame")
  expect_match(names(r)[1:3], "^animal_id$|^time$|^logger_id$")
  expect_is(r$animal_id, "character")
  expect_is(r$logger_id, "character")
  expect_is(r$time, "POSIXct")

  expect_equal(r$animal_id[2], "062000014F")
  expect_equal(r$logger_id[2], "GR10")
  expect_equal(r$time[2], as.POSIXct("2016-01-11 12:00:54", tz = "America/Vancouver"))
  expect_equal(load_raw(f, details = 1, tz = "America/Vancouver", tz_disp = "America/Toronto")$time[2], as.POSIXct("2016-01-11 15:00:54", tz = "America/Toronto"))
  expect_equal(load_raw(f)$time[2], as.POSIXct("2016-01-11 12:00:54", tz = Sys.timezone()))
})

# load_raw() empty file
test_that("load_raw handles empty files gracefully", {
  f <- system.file("extdata", "raw", "exp2", "GR10DATA_2016_01_18.TXT", package = "feedr")
  expect_message(r <- load_raw(r_file = f))
  expect_null(r, NULL)
})

# load_raw_all()
test_that("load_raw_all loads and formats data correctly", {
  d <- system.file("extdata", "raw", package = "feedr")
  load <- load_raw_all(r_dir = d, extra_pattern = "exp[0-9]{1,2}", extra_name = "Experiment", tz = "America/Vancouver")
  load2 <- load_raw_all(r_dir = d, extra_pattern = "exp[0-9]{1,2}", extra_name = "Experiment", tz = "America/Vancouver", tz_disp = "America/Toronto")
  expect_is(load, "data.frame")
  expect_match(names(load)[1:3], "^animal_id$|^time$|^logger_id$")
  expect_is(load$animal_id, "factor")
  expect_is(load$logger_id, "factor")
  expect_is(load$time, "POSIXct")

  expect_equal(load$animal_id[1], factor("0000000000", levels =  c("0000000000", "011017A536", "011017A605", "03000314F9", "0620000062", "062000014F", "06200001F0", "06200002E7", "0620000380", "062000038D", "062000038F", "0620000392", "06200003A7", "06200003B4", "06200003C3", "06200003F3", "0620000400", "0620000418", "06200004A9", "06200004BB", "06200004BE", "06200004BF", "06200004E4", "0620000525", "07008D9E08", "0700ED9E0E", "0700EDAB15", "0700EDF012", "0700EDF015", "0700EE022B", "0700EE0E42", "0700EE1461", "0700EE1467", "0700EE147F", "0700EE1504", "0700EE19CE", "0700EE2B10", "0700EE2B11")))
  expect_equal(load$logger_id[1], factor("GR10", levels = c("GR10", "GR11", "GR12", "GR13")))
  expect_equal(load$time[1], as.POSIXct("2016-01-11 11:34:08", tz = "America/Vancouver"))
  expect_equal(load2$time[1], as.POSIXct("2016-01-11 14:34:08", tz = "America/Toronto"))
})

# dl_data()
test_that("dl_data loads and formats data correctly", {
  load <- dl_data(start = "2016-01-01", end = "2016-02-01")
  load2 <- dl_data(start = "2016-01-01", end = "2016-02-01", tz_disp = "America/Toronto")
  expect_is(load, "data.frame")
  expect_match(names(load)[1:3], "^animal_id$|^time$|^logger_id$")
  expect_is(load$animal_id, "factor")
  expect_is(load$logger_id, "factor")
  expect_is(load$time, "POSIXct")

  expect_equal(load$animal_id[1], factor("0620000514", levels = c("041868D396", "041868D861", "041868FF93", "062000043E", "06200004F8", "0620000514")))
  expect_equal(load$logger_id[1], factor("2200", levels = c("2100", "2200", "2400", "2700")))
  expect_equal(load$time[1], as.POSIXct("2016-01-28 12:34:25", tz = "America/Vancouver"))
  expect_equal(load2$time[1], as.POSIXct("2016-01-28 15:34:25", tz = "America/Toronto"))
})
