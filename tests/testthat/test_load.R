library(feedr)
context("Loading files")

# load.web()
test_that("load.web loads and formats data correctly", {
  f <- "../test_load_web.csv"
  expect_is(load.web(f), "data.frame")
  expect_match(names(load.web(f))[1:3], "^bird_id$|^time$|^feeder_id$")
  expect_is(load.web(f)$bird_id, "factor")
  expect_is(load.web(f)$feeder_id, "factor")
  expect_is(load.web(f)$time, "POSIXct")

  expect_equal(load.web(f)$bird_id[1], factor("062000031A", levels = c("062000031A","0620000500")))
  expect_equal(load.web(f)$feeder_id[1], factor("2100", levels = c("2100","2700")))
  expect_equal(load.web(f)$time[1], as.POSIXct("2015-09-02 15:25:58"))
})

# load.raw()
test_that("load.raw loads and formats data correctly", {
  f <- "../raw_tests/exp1/GR12DATA.csv"
  expect_is(load.raw(f), "data.frame")
  expect_match(names(load.raw(f))[1:3], "^bird_id$|^time$|^feeder_id$")
  expect_is(load.raw(f)$bird_id, "character")
  expect_is(load.raw(f)$feeder_id, "character")
  expect_is(load.raw(f)$time, "POSIXct")

  expect_equal(load.raw(f)$bird_id[1], "06200004BB")
  expect_equal(load.raw(f)$feeder_id[1], "GR12")
  expect_equal(load.raw(f)$time[1], as.POSIXct("2015-12-09 15:20:59"))
})

# load.raw.all()
test_that("load.raw.all loads and formats data correctly", {
  load <- load.raw.all(r_dir = "../raw_tests/", extra_pattern = "exp[0-9]{1,2}", extra_name = "Experiment")
  expect_is(load, "data.frame")
  expect_match(names(load)[1:3], "^bird_id$|^time$|^feeder_id$")
  expect_is(load$bird_id, "factor")
  expect_is(load$feeder_id, "factor")
  expect_is(load$time, "POSIXct")

  expect_equal(load$bird_id[1], factor("06200004BB", levels = c("06200001F0", "06200003C3", "0620000418", "06200004BB")))
  expect_equal(load$feeder_id[1], factor("GR12", levels = c("GR11","GR12")))
  expect_equal(load$time[1], as.POSIXct("2015-12-09 15:20:59"))
})

# get.data()
test_that("get.data loads and formats data correctly", {
  data <- get.data(start = "2016-03-17", end = "2016-03-17 09:00:00", sites = "Kamloops")
  expect_is(data, "data.frame")
  expect_match(names(data)[1:3], "^bird_id$|^time$|^feeder_id$")
  expect_is(data$bird_id, "factor")
  expect_is(data$feeder_id, "factor")
  expect_is(data$time, "POSIXct")

  expect_equal(data$bird_id[1], factor("062000043E", levels = c("062000043E")))
  expect_equal(data$feeder_id[1], factor("1500", levels = c("1500")))
  expect_equal(data$time[1], as.POSIXct("2016-03-17 07:54:29"))
})
