library(feedr)
context("Loading files")

# load_web()
test_that("load_web loads and formats data correctly", {
  load <- load_web("../data/test_load_web.csv")
  load2 <- load_web("../data/test_load_web.csv", tz_disp = "America/Toronto")
  expect_is(load, "data.frame")
  expect_match(names(load)[1:3], "^bird_id$|^time$|^feeder_id$")
  expect_is(load$bird_id, "factor")
  expect_is(load$feeder_id, "factor")
  expect_is(load$time, "POSIXct")

  expect_equal(load$bird_id[1], factor("0620000514", levels = c("041868D396", "041868D861", "041868FF93", "062000043E", "06200004F8", "0620000514")))
  expect_equal(load$feeder_id[1], factor("2200", levels = c("2100", "2200", "2400", "2700")))
  expect_equal(load$time[1], as.POSIXct("2016-01-28 12:34:25", tz = "America/Vancouver"))
  expect_equal(load2$time[1], as.POSIXct("2016-01-28 15:34:25", tz = "America/Toronto"))
})

# load_raw() - feeder_id from file name
test_that("load_raw loads and formats data correctly - feeder id from file name", {
  f <- system.file("extdata", "raw", "exp1", "GR12DATA_2015_12_24.TXT", package = "feedr")
  expect_is(r <- load_raw(r_file = f), "data.frame")
  expect_match(names(r)[1:3], "^bird_id$|^time$|^feeder_id$")
  expect_is(r$bird_id, "character")
  expect_is(r$feeder_id, "character")
  expect_is(r$time, "POSIXct")

  expect_equal(r$bird_id[1], "062000014F")
  expect_equal(r$feeder_id[1], "GR12")
  expect_equal(r$time[1], as.POSIXct("2015-12-15 11:16:08", tz = "America/Vancouver"))
  expect_equal(load_raw(f, tz_disp = "America/Toronto")$time[1], as.POSIXct("2015-12-15 14:16:08", tz = "America/Toronto"))
})

# load_raw() - feeder_id from first line
test_that("load_raw loads and formats data correctly - feeder id from first line", {
  f <- system.file("extdata", "raw", "exp1", "GR12DATA_2015_12_24.TXT", package = "feedr")
  expect_is(r <- load_raw(r_file = f, feeder_id_loc = "firstline"), "data.frame")
  expect_match(names(r)[1:3], "^bird_id$|^time$|^feeder_id$")
  expect_is(r$bird_id, "character")
  expect_is(r$feeder_id, "character")
  expect_is(r$time, "POSIXct")

  expect_equal(r$bird_id[1], "062000014F")
  expect_equal(r$feeder_id[1], "GR12")
  expect_equal(r$time[1], as.POSIXct("2015-12-15 11:16:08", tz = "America/Vancouver"))
  expect_equal(load_raw(f, feeder_id_loc = "firstline", tz_disp = "America/Toronto")$time[1], as.POSIXct("2015-12-15 14:16:08", tz = "America/Toronto"))
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
  load <- load_raw_all(r_dir = d, extra_pattern = "exp[0-9]{1,2}", extra_name = "Experiment")
  load2 <- load_raw_all(r_dir = d, extra_pattern = "exp[0-9]{1,2}", extra_name = "Experiment", tz_disp = "America/Toronto")
  expect_is(load, "data.frame")
  expect_match(names(load)[1:3], "^bird_id$|^time$|^feeder_id$")
  expect_is(load$bird_id, "factor")
  expect_is(load$feeder_id, "factor")
  expect_is(load$time, "POSIXct")

  expect_equal(load$bird_id[1], factor("0620000006", levels =  c("0000000000", "03000314F9", "0620000006", "0620000062", "06200000F7", "062000014F", "06200001F0", "06200002E7", "0620000318", "0620000380", "0620000384", "062000038D", "062000038F", "0620000392", "062000039D", "06200003A7", "06200003B4", "06200003C3", "06200003DE", "06200003E0", "06200003F3", "0620000400", "0620000418", "0620000453", "06200004A9", "06200004BB", "06200004BE", "06200004BF", "06200004E4", "0620000503", "0620000525", "0700EDAB15", "0700EE022B", "0700EE0E42", "0700EE1504")))
  expect_equal(load$feeder_id[1], factor("GR10", levels = c("GR10", "GR11", "GR12", "GR13")))
  expect_equal(load$time[1], as.POSIXct("2015-12-05 12:57:48", tz = "America/Vancouver"))
  expect_equal(load2$time[1], as.POSIXct("2015-12-05 15:57:48", tz = "America/Toronto"))
})

# dl_data()
test_that("dl_data loads and formats data correctly", {
  load <- dl_data(start = "2016-01-01", end = "2016-02-01")
  load2 <- dl_data(start = "2016-01-01", end = "2016-02-01", tz_disp = "America/Toronto")
  expect_is(load, "data.frame")
  expect_match(names(load)[1:3], "^bird_id$|^time$|^feeder_id$")
  expect_is(load$bird_id, "factor")
  expect_is(load$feeder_id, "factor")
  expect_is(load$time, "POSIXct")

  expect_equal(load$bird_id[1], factor("0620000514", levels = c("041868D396", "041868D861", "041868FF93", "062000043E", "06200004F8", "0620000514")))
  expect_equal(load$feeder_id[1], factor("2200", levels = c("2100", "2200", "2400", "2700")))
  expect_equal(load$time[1], as.POSIXct("2016-01-28 12:34:25", tz = "America/Vancouver"))
  expect_equal(load2$time[1], as.POSIXct("2016-01-28 15:34:25", tz = "America/Toronto"))
})
