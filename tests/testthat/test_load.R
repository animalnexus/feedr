library(feedr)
context("Loading files")

# load.web()
test_that("load.web loads and formats data correctly", {
  load <- load.web("../data/test_load_web.csv")
  load2 <- load.web("../data/test_load_web.csv", tz_disp = "America/Toronto")
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

# load.raw()
test_that("load.raw loads and formats data correctly", {
  expect_is(r <- load.raw("../data/raw_tests/exp1/GR12DATA.csv"), "data.frame")
  expect_match(names(r)[1:3], "^bird_id$|^time$|^feeder_id$")
  expect_is(r$bird_id, "character")
  expect_is(r$feeder_id, "character")
  expect_is(r$time, "POSIXct")

  expect_equal(r$bird_id[1], "0620000418")
  expect_equal(r$feeder_id[1], "GR12")
  expect_equal(r$time[1], as.POSIXct("2015-12-10 09:17:54", tz = "America/Vancouver"))
  expect_equal(load.raw("../data/raw_tests/exp1/GR12DATA.csv", tz_disp = "America/Toronto")$time[1], as.POSIXct("2015-12-10 12:17:54", tz = "America/Toronto"))
})

# load.raw.all()
test_that("load.raw.all loads and formats data correctly", {
  load <- load.raw.all(r_dir = "../data/raw_tests/", extra_pattern = "exp[0-9]{1,2}", extra_name = "Experiment")
  load2 <- load.raw.all(r_dir = "../data/raw_tests/", extra_pattern = "exp[0-9]{1,2}", extra_name = "Experiment", tz_disp = "America/Toronto")
  expect_is(load, "data.frame")
  expect_match(names(load)[1:3], "^bird_id$|^time$|^feeder_id$")
  expect_is(load$bird_id, "factor")
  expect_is(load$feeder_id, "factor")
  expect_is(load$time, "POSIXct")

  expect_equal(load$bird_id[1], factor("0620000418", levels = c("06200001F0", "06200003C3", "0620000418", "06200004BB")))
  expect_equal(load$feeder_id[1], factor("GR12", levels = c("GR11","GR12")))
  expect_equal(load$time[1], as.POSIXct("2015-12-10 09:17:54", tz = "America/Vancouver"))
  expect_equal(load2$time[1], as.POSIXct("2015-12-10 12:17:54", tz = "America/Toronto"))
})

# get.data()
test_that("get.data loads and formats data correctly", {
  load <- get.data(start = "2016-01-01", end = "2016-02-01", sites = "Kamloops")
  load2 <- get.data(start = "2016-01-01", end = "2016-02-01", sites = "Kamloops", tz_disp = "America/Toronto")
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
