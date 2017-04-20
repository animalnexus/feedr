library(feedr)
library(magrittr)

context("Transformations to activity")

# activity()
test_that("activity() in general", {
  p <- presence(visits(finches))

  expect_message(a <- activity(p), "041868D396: Skipping. Individual has less than 24hrs of data")
  expect_message(activity(p), "0620000514: 88.89% of obs")
  expect_message(activity(p, res = 5), "0620000514: 55.56% of obs")

  expect_is(a, "data.frame")
  expect_match(names(a)[1:6], "^animal_id$|^time$|^date$|^activity$|^activity_c$|^logger_id$")
  expect_is(a$animal_id, "factor")
  expect_is(a$logger_id, "factor")
  expect_is(a$time, "POSIXct")

  expect_equal(a$animal_id[1], factor("06200004F8", levels = c("041868D396", "041868D861", "062000043E", "06200004F8", "0620000514")))
  expect_equal(a$logger_id[1], factor(NA, levels = c("2100", "2200", "2400", "2700")))
  expect_equal(a$time[1], as.POSIXct("2016-01-28", tz = "America/Vancouver"))
  expect_equal(nrow(a), 386)
})

# activity()
test_that("activity() no lat/lon", {
  p <- finches[, !(names(finches) %in% c("lat", "lon"))]
  expect_message(expect_error(a <- activity(presence(visits(p))), NA))
  expect_true(!all(c("lat", "lon") %in% names(a)))
  expect_equal(a$logger_id[1], factor(NA, levels = c("2100", "2200", "2400", "2700")))
  expect_equal(nrow(a), 386)
})

# activity()
test_that("activity() no lat/lon, by logger", {
  p <- finches[, !(names(finches) %in% c("lat", "lon"))]
  expect_message(expect_error(a <- activity(presence(visits(p)), by_logger = TRUE), NA))
  expect_true(!all(c("lat", "lon") %in% names(a)))
  expect_equal(a$logger_id[1], factor(2100, levels = c("2100", "2200", "2400", "2700")))
  expect_equal(nrow(a), 1544)
})

# activity()
test_that("activity() no missing, by logger", {
  a <- activity(presence(visits(finches)), by_logger = TRUE)

  expect_equal(a$logger_id[1], factor(2100, levels = c("2100", "2200", "2400", "2700")))
  expect_equal(nrow(a), 1544)
})



# activity()
test_that("activity() missing", {
  # p <- presence(visits(finches))
  # m <- read.csv("../data/missing.csv")
  #
  # a <- activity(p, by_logger = TRUE, missing = m)
  # expect_equal(nrow(a[a$activity_c == "unknown",]), 260)
  #
  # a <- activity(p, missing = m)
  # expect_equal(nrow(a[a$activity_c == "unknown",]), 130)
  #
  # a <- activity(p, missing = "../data/missing.csv")
  # expect_equal(nrow(a[a$activity_c == "unknown",]), 130)
  #
  # expect_error(activity(p, missing = c(1, 2)), "'missing' must be")
})

# daily()
test_that("daily() by_logger == FALSE", {
  p <- presence(visits(finches))
  a <- activity(p)
  d <- daily(a)

  expect_equal(d$animal_id[1], factor("06200004F8", levels = c("041868D396", "041868D861", "062000043E", "06200004F8", "0620000514")))
  expect_equal(d$logger_id[1], factor(NA, levels = c("2100", "2200", "2400", "2700")))
  expect_equal(d$time[1], as.POSIXct("1970-01-01", tz = "America/Vancouver"))
  expect_equal(nrow(d), 192)

  a <- activity(p, by_logger = TRUE)
  d <- daily(a)

  expect_equal(d$animal_id[1], factor("06200004F8", levels = c("041868D396", "041868D861", "062000043E", "06200004F8", "0620000514")))
  expect_equal(d$logger_id[1], factor(2100, levels = c("2100", "2200", "2400", "2700")))
  expect_equal(d$time[1], as.POSIXct("1970-01-01", tz = "America/Vancouver"))
  expect_equal(nrow(d), 768)
})
