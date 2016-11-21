library(feedr)
library(magrittr)

context("Transformations to activity")

# activity()
test_that("activity() in general", {
  f <- feeding(visits(finches))

  expect_message(a <- activity(f), "041868D396: Skipping. Individual has less than 24hrs of data")
  expect_message(activity(f), "0620000514: 88.89% of obs")
  expect_message(activity(f, res = 5), "0620000514: 55.56% of obs")

  expect_is(a, "data.frame")
  expect_match(names(a)[1:6], "^bird_id$|^time$|^date$|^activity$|^activity_c$|^feeder_id$")
  expect_is(a$bird_id, "factor")
  expect_is(a$feeder_id, "factor")
  expect_is(a$time, "POSIXct")

  expect_equal(a$bird_id[1], factor("06200004F8", levels = c("041868D396", "041868D861", "041868FF93", "062000043E", "06200004F8", "0620000514")))
  expect_equal(a$feeder_id[1], factor(NA, levels = c("2100", "2200", "2400", "2700")))
  expect_equal(a$time[1], as.POSIXct("2016-01-28", tz = "America/Vancouver"))
  expect_equal(nrow(a), 386)
})

# activity()
test_that("activity() no missing, by feeder", {
  a <- activity(feeding(visits(finches)), by_feeder = TRUE)

  expect_equal(a$feeder_id[1], factor(2100, levels = c("2100", "2200", "2400", "2700")))
  expect_equal(nrow(a), 1544)
})

# activity()
test_that("activity() missing", {
  # f <- feeding(visits(finches))
  # m <- read.csv("../data/missing.csv")
  #
  # a <- activity(f, by_feeder = TRUE, missing = m)
  # expect_equal(nrow(a[a$activity_c == "unknown",]), 260)
  #
  # a <- activity(f, missing = m)
  # expect_equal(nrow(a[a$activity_c == "unknown",]), 130)
  #
  # a <- activity(f, missing = "../data/missing.csv")
  # expect_equal(nrow(a[a$activity_c == "unknown",]), 130)
  #
  # expect_error(activity(f, missing = c(1, 2)), "'missing' must be")
})

# daily()
test_that("daily() by_feeder == FALSE", {
  f <- feeding(visits(finches))
  a <- activity(f)
  d <- daily(a)

  expect_equal(d$bird_id[1], factor("06200004F8", levels = c("041868D396", "041868D861", "041868FF93", "062000043E", "06200004F8", "0620000514")))
  expect_equal(d$feeder_id[1], factor(NA, levels = c("2100", "2200", "2400", "2700")))
  expect_equal(d$time[1], as.POSIXct("1970-01-01", tz = "UTM"))
  expect_equal(nrow(d), 192)

  a <- activity(f, by_feeder = TRUE)
  d <- daily(a)

  expect_equal(d$bird_id[1], factor("06200004F8", levels = c("041868D396", "041868D861", "041868FF93", "062000043E", "06200004F8", "0620000514")))
  expect_equal(d$feeder_id[1], factor(2100, levels = c("2100", "2200", "2400", "2700")))
  expect_equal(d$time[1], as.POSIXct("1970-01-01", tz = "UTM"))
  expect_equal(nrow(d), 768)
})
